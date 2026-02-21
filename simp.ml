(* simp — a simple scripting language interpreter in OCaml *)

(* ============================================================
   Types
   ============================================================ *)

type value =
  | Nil
  | Num of float
  | Str of string
  | Arr of dynarr
  | Dict of (string, value) Hashtbl.t
  | Rex of Pcre.regexp
and dynarr = {
  mutable data : value array;
  mutable len : int;
}

type sub_def = { params: string list; body: string list }

(* ============================================================
   Helpers
   ============================================================ *)

let to_str = function
  | Nil -> ""
  | Num f ->
    let finite = match classify_float f with FP_nan | FP_infinite -> false | _ -> true in
    let frac, _ = modf f in
    if finite && frac = 0.0 then string_of_int (int_of_float f)
    else string_of_float f
  | Str s -> s
  | Arr _ -> "<array>"
  | Dict _ -> "<dict>"
  | Rex _ -> "<regex>"

let to_num = function
  | Nil -> 0.0
  | Num f -> f
  | Str s -> (try float_of_string s with Failure _ -> 0.0)
  | _ -> 0.0

let is_truthy = function
  | Nil -> false
  | Num f -> f <> 0.0
  | Str "" -> false
  | Str "0" -> false
  | Str _ -> true
  | Arr _ -> true
  | Dict _ -> true
  | Rex _ -> true

(* ============================================================
   Global state
   ============================================================ *)

let main_module = "main"
let current_module = ref main_module

let globals : (string, value) Hashtbl.t = Hashtbl.create 64
let global_arrays : (string, value) Hashtbl.t = Hashtbl.create 16
let global_dicts : (string, value) Hashtbl.t = Hashtbl.create 16

let module_vars : (string, (string, value) Hashtbl.t) Hashtbl.t = Hashtbl.create 16
let module_arrays : (string, (string, value) Hashtbl.t) Hashtbl.t = Hashtbl.create 16
let module_dicts : (string, (string, value) Hashtbl.t) Hashtbl.t = Hashtbl.create 16
let module_subs : (string, (string, sub_def) Hashtbl.t) Hashtbl.t = Hashtbl.create 16
let module_dirs : (string, string) Hashtbl.t = Hashtbl.create 16

let builtins : (string, value list -> value) Hashtbl.t = Hashtbl.create 64

let hashtbl_find_opt tbl key =
  try Some (Hashtbl.find tbl key) with Not_found -> None

let string_index_opt s ch =
  try Some (String.index s ch) with Not_found -> None

let string_rindex_opt s ch =
  try Some (String.rindex s ch) with Not_found -> None

let rec list_nth_opt lst n =
  match lst with
  | [] -> None
  | x :: _ when n = 0 -> Some x
  | _ :: tl when n > 0 -> list_nth_opt tl (n - 1)
  | _ -> None

let dynarr_empty () = { data = [||]; len = 0 }

let dynarr_of_list lst =
  let data = Array.of_list lst in
  { data; len = Array.length data }

let dynarr_to_list arr =
  let rec loop i acc =
    if i < 0 then acc
    else loop (i - 1) (arr.data.(i) :: acc)
  in
  loop (arr.len - 1) []

let dynarr_iter f arr =
  for i = 0 to arr.len - 1 do
    f arr.data.(i)
  done

let dynarr_ensure_capacity arr needed =
  let current = Array.length arr.data in
  if current < needed then begin
    let base = if current = 0 then 4 else current in
    let rec grow cap =
      if cap >= needed then cap else grow (cap * 2)
    in
    let next = grow base in
    let data = Array.make next Nil in
    Array.blit arr.data 0 data 0 arr.len;
    arr.data <- data
  end

let dynarr_push arr v =
  dynarr_ensure_capacity arr (arr.len + 1);
  arr.data.(arr.len) <- v;
  arr.len <- arr.len + 1

let dynarr_get arr idx =
  if idx < 0 || idx >= arr.len then None
  else Some arr.data.(idx)

let dynarr_pop arr =
  if arr.len = 0 then None
  else begin
    let idx = arr.len - 1 in
    let v = arr.data.(idx) in
    arr.data.(idx) <- Nil;
    arr.len <- idx;
    Some v
  end

let split_last lst =
  let rec aux rev_prefix = function
    | [] -> (List.rev rev_prefix, None)
    | [x] -> (List.rev rev_prefix, Some x)
    | x :: xs -> aux (x :: rev_prefix) xs
  in
  aux [] lst

let list_filter_map f lst =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs ->
      match f x with
      | Some y -> aux (y :: acc) xs
      | None -> aux acc xs
  in
  aux [] lst

let is_relative_path path =
  let len = String.length path in
  if len = 0 then true
  else if path.[0] = '/' then false
  else if len > 2 && path.[0] = '\\' && path.[1] = '\\' then false
  else if len > 1 && path.[1] = ':' then false
  else true

let ensure_module_table store module_name =
  match hashtbl_find_opt store module_name with
  | Some tbl -> tbl
  | None ->
    let tbl = Hashtbl.create 16 in
    Hashtbl.replace store module_name tbl;
    tbl

let module_vars_ref module_name = ensure_module_table module_vars module_name
let module_arrays_ref module_name = ensure_module_table module_arrays module_name
let module_dicts_ref module_name = ensure_module_table module_dicts module_name
let module_subs_ref module_name = ensure_module_table module_subs module_name

let () =
  ignore (module_vars_ref main_module);
  ignore (module_arrays_ref main_module);
  ignore (module_dicts_ref main_module);
  ignore (module_subs_ref main_module);
  Hashtbl.replace module_dirs main_module "."

(* Forward ref so load builtin can call run_lines *)
let run_lines_ref : (string list -> unit) ref = ref (fun _ -> ())
let call_depth = ref 0
let returning = ref false

(* ============================================================
   Precompiled regexes
   ============================================================ *)

let re_set_var =
  Str.regexp {|^set[ 	]+\$\([^ 	=]+\)[ 	]*=[ 	]*\(.*\)$|}
let re_set_arr =
  Str.regexp {|^set[ 	]+@\([^ 	=]+\)[ 	]*=[ 	]*\(.*\)$|}
let re_set_dict =
  Str.regexp {|^set[ 	]+%\([^ 	=]+\)[ 	]*=[ 	]*\(.*\)$|}
let re_global_set =
  Str.regexp {|^\$\([^ 	=]+\)[ 	]*=[ 	]*\(.*\)$|}
let re_if = Str.regexp {|^if[ 	]+\(.*\)$|}
let re_sub_def =
  Str.regexp {|^sub[ 	]+\([^ 	(]+\)[ 	]*(\([^)]*\))[ 	]*$|}
let re_alias =
  Str.regexp
    {|^alias[ 	]+\([^ 	=]+\)[ 	]*=[ 	]*\([^ 	]+\)[ 	]*$|}
let re_foreach =
  Str.regexp
    {|^foreach[ 	]+\$\([^ 	]+\)[ 	]+@\([^ 	]+\)[ 	]*$|}
let re_return = Str.regexp {|^return[ 	]*(\(.*\))[ 	]*$|}
let re_bare_call = Str.regexp {|^[^ 	(][^ 	(]*[ 	]*(|}
let re_number = Str.regexp {|^-?[0-9]+\(\.[0-9]+\)?$|}
let re_regex_lit = Str.regexp {|^#"\(.*\)"$|}

let re_matches rex s = Str.string_match rex s 0

let is_blank_or_comment s =
  let trimmed = String.trim s in
  trimmed = "" || (String.length trimmed > 0 && trimmed.[0] = '#')

let starts_block s =
  re_matches (Str.regexp {|^\(if\|sub\|foreach\)[ 	]|}) s

let is_end s =
  re_matches (Str.regexp {|^end\([ 	].*\)?$|}) s

let is_else s =
  re_matches (Str.regexp {|^else\([ 	].*\)?$|}) s

let is_ident_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

let parse_symbol_at s i =
  let len = String.length s in
  if i >= len || not (is_ident_start s.[i]) then None
  else
    let j = ref (i + 1) in
    while !j < len && is_ident_char s.[!j] do incr j done;
    let rec loop k =
      if k >= len then Some k
      else if s.[k] = '-' then begin
        let after_sep =
          if k + 1 < len && s.[k + 1] = '>' then k + 2 else k + 1
        in
        if after_sep >= len || not (is_ident_char s.[after_sep]) then Some k
        else
          let t = ref (after_sep + 1) in
          while !t < len && is_ident_char s.[!t] do incr t done;
          loop !t
      end else if s.[k] = '?' then begin
        if k + 1 >= len then Some (k + 1)
        else if not (is_ident_char s.[k + 1]) then Some k
        else
          let t = ref (k + 2) in
          while !t < len && is_ident_char s.[!t] do incr t done;
          loop !t
      end else
        Some k
    in
    loop !j

let parse_module_at s i =
  let len = String.length s in
  if i >= len || not (is_ident_start s.[i]) then None
  else
    let j = ref (i + 1) in
    while !j < len && is_ident_char s.[!j] do incr j done;
    let rec loop k =
      if k < len && s.[k] = '-' && k + 1 < len && is_ident_char s.[k + 1] then
        let t = ref (k + 2) in
        while !t < len && is_ident_char s.[!t] do incr t done;
        loop !t
      else
        Some k
    in
    loop !j

let is_symbol_name name =
  match parse_symbol_at name 0 with
  | Some idx -> idx = String.length name
  | None -> false

let is_module_name name =
  match parse_module_at name 0 with
  | Some idx -> idx = String.length name
  | None -> false

let is_global_name name =
  if not (is_symbol_name name) then false
  else
    let has_upper = ref false in
    let has_lower = ref false in
    String.iter (function
      | 'A' .. 'Z' -> has_upper := true
      | 'a' .. 'z' -> has_lower := true
      | _ -> ()
    ) name;
    !has_upper && not !has_lower

let is_local_name name =
  if not (is_symbol_name name) then false
  else
    let has_upper = ref false in
    String.iter (function
      | 'A' .. 'Z' -> has_upper := true
      | _ -> ()
    ) name;
    not !has_upper

let split_qualified_symbol token =
  match String.split_on_char '/' token with
  | [name] when is_symbol_name name -> Some (None, name)
  | [module_name; name] when is_module_name module_name && is_symbol_name name ->
    Some (Some module_name, name)
  | _ -> None

let split_qualified_func token =
  match String.split_on_char '/' token with
  | [name] when is_symbol_name name -> Some (None, name)
  | [first; second] when is_symbol_name first && is_symbol_name second ->
    Some (Some first, second)
  | _ -> None

let parse_func_call expr =
  let len = String.length expr in
  match parse_symbol_at expr 0 with
  | None -> None
  | Some name_end ->
    let fname, after_fname =
      if name_end < len && expr.[name_end] = '/' then
        match parse_symbol_at expr (name_end + 1) with
        | Some end2 ->
          (String.sub expr 0 end2, end2)
        | None ->
          ("", -1)
      else
        (String.sub expr 0 name_end, name_end)
    in
    if after_fname < 0 || fname = "" then None
    else
      let j = ref after_fname in
      while !j < len && (expr.[!j] = ' ' || expr.[!j] = '\t') do incr j done;
      if !j >= len || expr.[!j] <> '(' then None
      else
        let rec find_last_non_space k =
          if k < 0 then -1
          else if expr.[k] = ' ' || expr.[k] = '\t' then find_last_non_space (k - 1)
          else k
        in
        let last = find_last_non_space (len - 1) in
        if last <= !j || expr.[last] <> ')' then None
        else
          let raw_args = String.sub expr (!j + 1) (last - !j - 1) in
          Some (fname, raw_args)

let get_global_var name =
  match hashtbl_find_opt globals name with
  | Some v -> v
  | None -> Nil

let get_module_var module_name name =
  let vars = module_vars_ref module_name in
  match hashtbl_find_opt vars name with
  | Some v -> v
  | None -> Nil

let get_module_array module_name name =
  let arrays = module_arrays_ref module_name in
  match hashtbl_find_opt arrays name with
  | Some v -> v
  | None -> Arr (dynarr_empty ())

let get_module_dict module_name name =
  let dicts = module_dicts_ref module_name in
  match hashtbl_find_opt dicts name with
  | Some v -> v
  | None -> Dict (Hashtbl.create 4)

let get_global_array name =
  match hashtbl_find_opt global_arrays name with
  | Some v -> v
  | None -> Arr (dynarr_empty ())

let get_global_dict name =
  match hashtbl_find_opt global_dicts name with
  | Some v -> v
  | None -> Dict (Hashtbl.create 4)

let module_name_from_file file =
  let base = Filename.basename file in
  let name =
    match string_rindex_opt base '.' with
    | Some idx when idx > 0 -> String.sub base 0 idx
    | _ -> base
  in
  if is_module_name name then name
  else failwith ("load: invalid module name '" ^ name ^ "'")

let has_extension file =
  let base = Filename.basename file in
  String.contains base '.'

let file_exists path =
  Sys.file_exists path &&
  (try not (Sys.is_directory path) with _ -> false)

let resolve_load_path file =
  let candidates = ref [] in
  let add p = candidates := p :: !candidates in
  if is_relative_path file then begin
    let base_dir = match hashtbl_find_opt module_dirs !current_module with
      | Some d -> d
      | None -> "." in
    add (Filename.concat base_dir file);
    if not (has_extension file) then add (Filename.concat base_dir (file ^ ".simp"));
    add file;
    if not (has_extension file) then add (file ^ ".simp")
  end else begin
    add file;
    if not (has_extension file) then add (file ^ ".simp")
  end;
  let rec pick = function
    | [] -> file
    | c :: rest -> if file_exists c then c else pick rest
  in
  pick (List.rev !candidates)

let resolve_sub_target fname =
  match split_qualified_func fname with
  | Some (Some module_name, sub_name) when is_module_name module_name ->
    let subs = module_subs_ref module_name in
    if Hashtbl.mem subs sub_name then Some (module_name, sub_name) else None
  | Some (None, sub_name) ->
    let curr_subs = module_subs_ref !current_module in
    if Hashtbl.mem curr_subs sub_name then Some (!current_module, sub_name)
    else
      let main_subs = module_subs_ref main_module in
      if Hashtbl.mem main_subs sub_name then Some (main_module, sub_name)
      else None
  | _ -> None

let is_string_literal s =
  let len = String.length s in
  if len < 2 || s.[0] <> '"' || s.[len - 1] <> '"' then false
  else
    let in_escape = ref false in
    let ok = ref true in
    for i = 1 to len - 2 do
      let ch = s.[i] in
      if !in_escape then
        in_escape := false
      else if ch = '\\' then
        in_escape := true
      else if ch = '"' then
        ok := false
    done;
    !ok

(* ============================================================
   parse_arglist — split on comma at depth 0
   ============================================================ *)

let parse_arglist str =
  let len = String.length str in
  let buf = Buffer.create 32 in
  let args = ref [] in
  let depth = ref 0 in
  let in_quote = ref false in
  let escaped = ref false in
  for i = 0 to len - 1 do
    let ch = str.[i] in
    if !in_quote then begin
      if !escaped then begin
        escaped := false;
        Buffer.add_char buf ch
      end else if ch = '\\' then begin
        escaped := true;
        Buffer.add_char buf ch
      end else if ch = '"' then begin
        in_quote := false;
        Buffer.add_char buf ch
      end else
        Buffer.add_char buf ch
    end else if ch = '"' then begin
      in_quote := true;
      Buffer.add_char buf ch
    end else if ch = '(' || ch = '[' || ch = '{' then begin
      incr depth;
      Buffer.add_char buf ch
    end else if ch = ')' || ch = ']' || ch = '}' then begin
      decr depth;
      Buffer.add_char buf ch
    end else if ch = ',' && !depth = 0 then begin
      args := Buffer.contents buf :: !args;
      Buffer.clear buf
    end else
      Buffer.add_char buf ch
  done;
  if !in_quote then
    failwith ("Unterminated string in argument list: '" ^ str ^ "'");
  if !depth <> 0 then
    failwith ("Unbalanced brackets in argument list: '" ^ str ^ "'");
  let last = Buffer.contents buf in
  if String.trim last <> "" then args := last :: !args;
  List.rev !args

let parse_string_literal raw =
  let len = String.length raw in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let ch = raw.[!i] in
    if ch = '\\' then begin
      incr i;
      if !i >= len then Buffer.add_char buf '\\'
      else begin
        let esc = raw.[!i] in
        match esc with
        | 'n' -> Buffer.add_char buf '\n'
        | 't' -> Buffer.add_char buf '\t'
        | 'r' -> Buffer.add_char buf '\r'
        | '"' | '\\' | '$' -> Buffer.add_char buf esc
        | _ -> Buffer.add_char buf '\\'; Buffer.add_char buf esc
      end
    end else if ch = '$' then begin
      if !i + 1 < len then begin
        match parse_symbol_at raw (!i + 1) with
        | Some first_end when first_end > !i + 1 ->
          let first = String.sub raw (!i + 1) (first_end - !i - 1) in
          let qualified_done =
            if first_end < len && raw.[first_end] = '/' && is_module_name first then
              match parse_symbol_at raw (first_end + 1) with
              | Some second_end when second_end > first_end + 1 ->
                let name = String.sub raw (first_end + 1) (second_end - first_end - 1) in
                let repl =
                  if is_global_name name then
                    get_global_var name
                  else if is_local_name name then
                    get_module_var first name
                  else
                    failwith ("Invalid local variable name '$" ^ name ^ "' (locals must be lowercase)")
                in
                Buffer.add_string buf (to_str repl);
                i := second_end - 1;
                true
              | _ -> false
            else false
          in
          if not qualified_done then begin
            let repl =
              if is_global_name first then
                get_global_var first
              else if is_local_name first then
                let vars = module_vars_ref !current_module in
                (match hashtbl_find_opt vars first with
                | Some v -> v
                | None -> get_global_var first)
              else
                failwith ("Invalid local variable name '$" ^ first ^ "' (locals must be lowercase)")
            in
            Buffer.add_string buf (to_str repl);
            i := first_end - 1
          end
        | _ ->
          Buffer.add_char buf '$'
      end else
        Buffer.add_char buf '$'
    end else
      Buffer.add_char buf ch;
    incr i
  done;
  Str (Buffer.contents buf)

let split_dict_pair pair =
  let len = String.length pair in
  let depth = ref 0 in
  let in_quote = ref false in
  let escaped = ref false in
  let found = ref None in
  let i = ref 0 in
  while !i < len && !found = None do
    let ch = pair.[!i] in
    if !in_quote then begin
      if !escaped then
        escaped := false
      else if ch = '\\' then
        escaped := true
      else if ch = '"' then
        in_quote := false
    end else if ch = '"' then
      in_quote := true
    else begin
      if ch = '(' || ch = '[' || ch = '{' then incr depth
      else if ch = ')' || ch = ']' || ch = '}' then decr depth
      else if ch = ':' && !depth = 0 then
        found := Some !i
    end;
    incr i
  done;
  match !found with
  | None -> None
  | Some idx ->
    let left = String.sub pair 0 idx in
    let right = String.sub pair (idx + 1) (len - idx - 1) in
    Some (left, right)

(* ============================================================
   eval_expr / exec_line / run_lines — mutually recursive
   ============================================================ *)

let parse_prefixed_symbol expr prefix =
  let len = String.length expr in
  if len > 1 && expr.[0] = prefix then
    let token = String.sub expr 1 (len - 1) in
    split_qualified_symbol token
  else
    None

let rec eval_expr raw_expr =
  let expr = String.trim raw_expr in

  (* String literal with $var interpolation *)
  if is_string_literal expr then
    parse_string_literal (String.sub expr 1 (String.length expr - 2))

  (* Number *)
  else if re_matches re_number expr then
    Num (float_of_string expr)

  (* Variable *)
  else if String.length expr > 1 && expr.[0] = '$' then begin
    match parse_prefixed_symbol expr '$' with
    | Some (Some module_name, name) ->
      if is_global_name name then
        get_global_var name
      else if is_local_name name then
        get_module_var module_name name
      else
        failwith ("Invalid local variable name '$" ^ name ^ "' (locals must be lowercase)")
    | Some (None, name) ->
      if is_global_name name then
        get_global_var name
      else if is_local_name name then
        let vars = module_vars_ref !current_module in
        (match hashtbl_find_opt vars name with
        | Some v -> v
        | None -> get_global_var name)
      else
        failwith ("Invalid local variable name '$" ^ name ^ "' (locals must be lowercase)")
    | None ->
      failwith ("Cannot evaluate expression: '" ^ expr ^ "'")
  end

  (* Array variable *)
  else if String.length expr > 1 && expr.[0] = '@' then begin
    match parse_prefixed_symbol expr '@' with
    | Some (Some module_name, name) ->
      if is_global_name name then
        get_global_array name
      else if is_local_name name then
        get_module_array module_name name
      else
        failwith ("Invalid local array name '@" ^ name ^ "' (locals must be lowercase)")
    | Some (None, name) ->
      if is_global_name name then
        get_global_array name
      else if is_local_name name then
        get_module_array !current_module name
      else
        failwith ("Invalid local array name '@" ^ name ^ "' (locals must be lowercase)")
    | None ->
      failwith ("Cannot evaluate expression: '" ^ expr ^ "'")
  end

  (* Dict variable *)
  else if String.length expr > 1 && expr.[0] = '%' then begin
    match parse_prefixed_symbol expr '%' with
    | Some (Some module_name, name) ->
      if is_global_name name then
        get_global_dict name
      else if is_local_name name then
        get_module_dict module_name name
      else
        failwith ("Invalid local dict name '%" ^ name ^ "' (locals must be lowercase)")
    | Some (None, name) ->
      if is_global_name name then
        get_global_dict name
      else if is_local_name name then
        get_module_dict !current_module name
      else
        failwith ("Invalid local dict name '%" ^ name ^ "' (locals must be lowercase)")
    | None ->
      failwith ("Cannot evaluate expression: '" ^ expr ^ "'")
  end

  (* Vector literal: [a, b, c] *)
  else if String.length expr >= 2 && expr.[0] = '[' && expr.[String.length expr - 1] = ']' then
    let inner = String.sub expr 1 (String.length expr - 2) in
    let items = parse_arglist inner in
    Arr (dynarr_of_list (List.map eval_expr items))

  (* Dict literal: {key: val, "quoted key": val2} *)
  else if String.length expr >= 2 && expr.[0] = '{' && expr.[String.length expr - 1] = '}' then
    let inner = String.sub expr 1 (String.length expr - 2) in
    let h = Hashtbl.create 8 in
    List.iter (fun pair ->
      let pair = String.trim pair in
      match split_dict_pair pair with
      | None -> failwith ("Bad dict entry: '" ^ pair ^ "'")
      | Some (kexpr, vexpr) ->
        let kexpr = String.trim kexpr in
        let vexpr = String.trim vexpr in
        let key =
          if is_symbol_name kexpr then kexpr
          else to_str (eval_expr kexpr)
        in
        Hashtbl.replace h key (eval_expr vexpr)
    ) (parse_arglist inner);
    Dict h

  (* Regex literal: #"pattern" *)
  else if re_matches re_regex_lit expr then
    let pat = Str.matched_group 1 expr in
    Rex (Pcre.regexp pat)

  (* Function call: name( args ) *)
  else
    match parse_func_call expr with
    | Some (fname, raw_args) ->
      let args = parse_arglist raw_args in
      let evaled = List.map eval_expr args in
      if fname = "print" then begin
        let parts, to_stderr =
          match split_last evaled with
          | prefix, Some last when prefix <> [] && to_str last = "1" -> (prefix, true)
          | _ -> (evaled, false)
        in
        let out = String.concat "" (List.map to_str parts) in
        if to_stderr then
          Printf.eprintf "%s\n" out
        else
          Printf.printf "%s\n" out;
        Str out
      end else (
        match resolve_sub_target fname with
        | Some (target_module, sub_name) ->
          let sub = Hashtbl.find (module_subs_ref target_module) sub_name in
          let vars = module_vars_ref target_module in
          let saved_vars = Hashtbl.copy vars in
          let saved_depth = !call_depth in
          let saved_returning = !returning in
          let saved_module = !current_module in
          let restore () =
            Hashtbl.reset vars;
            Hashtbl.iter (Hashtbl.replace vars) saved_vars;
            call_depth := saved_depth;
            returning := saved_returning;
            current_module := saved_module
          in
          call_depth := !call_depth + 1;
          returning := false;
          current_module := target_module;
          (try
             let rec bind_params params values =
               match params with
               | [] -> ()
               | p :: ps ->
                 let v, vs =
                   match values with
                   | [] -> (Nil, [])
                   | x :: xs -> (x, xs)
                 in
                 Hashtbl.replace vars p v;
                 bind_params ps vs
             in
             bind_params sub.params evaled;
             Hashtbl.remove vars "_return";
             run_lines sub.body;
             let ret = match hashtbl_find_opt vars "_return" with
               | Some v -> v | None -> Nil in
             restore ();
             ret
           with exn ->
             restore ();
             raise exn)
        | None ->
          if Hashtbl.mem builtins fname then
            (Hashtbl.find builtins fname) evaled
          else
            failwith ("Unknown function: " ^ fname)
      )
    | None ->
      failwith ("Cannot evaluate expression: '" ^ expr ^ "'")

and exec_line lines i =
  let line = String.trim lines.(i) in

  (* blank / comment *)
  if is_blank_or_comment line then i + 1

  (* set $var = expr *)
  else if re_matches re_set_var line then begin
    let name = Str.matched_group 1 line in
    let expr = Str.matched_group 2 line in
    if is_global_name name then
      Hashtbl.replace globals name (eval_expr expr)
    else if is_local_name name then
      Hashtbl.replace (module_vars_ref !current_module) name (eval_expr expr)
    else
      failwith ("Invalid local variable name '$" ^ name ^ "' (locals must be lowercase)");
    i + 1
  end

  (* $GLOBAL = expr *)
  else if re_matches re_global_set line then begin
    let name = Str.matched_group 1 line in
    let expr = Str.matched_group 2 line in
    if not (is_global_name name) then
      failwith ("Global names must be uppercase: '$" ^ name ^ "'");
    Hashtbl.replace globals name (eval_expr expr);
    i + 1
  end

  (* set @arr = expr *)
  else if re_matches re_set_arr line then begin
    let name = Str.matched_group 1 line in
    let expr = Str.matched_group 2 line in
    if is_global_name name then
      Hashtbl.replace global_arrays name (eval_expr expr)
    else if is_local_name name then
      Hashtbl.replace (module_arrays_ref !current_module) name (eval_expr expr)
    else
      failwith ("Invalid local array name '@" ^ name ^ "' (locals must be lowercase)");
    i + 1
  end

  (* set %dict = expr *)
  else if re_matches re_set_dict line then begin
    let name = Str.matched_group 1 line in
    let expr = Str.matched_group 2 line in
    if is_global_name name then
      Hashtbl.replace global_dicts name (eval_expr expr)
    else if is_local_name name then
      Hashtbl.replace (module_dicts_ref !current_module) name (eval_expr expr)
    else
      failwith ("Invalid local dict name '%" ^ name ^ "' (locals must be lowercase)");
    i + 1
  end

  (* return(expr) *)
  else if re_matches re_return line then begin
    if !call_depth <= 0 then
      failwith (Printf.sprintf "return outside sub on line %d" (i + 1));
    let raw = Str.matched_group 1 line in
    let ret = if String.trim raw = "" then Nil else eval_expr raw in
    Hashtbl.replace (module_vars_ref !current_module) "_return" ret;
    returning := true;
    i + 1
  end

  (* if cond ... else ... end *)
  else if re_matches re_if line then begin
    let cond = eval_expr (Str.matched_group 1 line) in
    let truthy = is_truthy cond in
    let true_block = ref [] in
    let false_block = ref [] in
    let depth = ref 1 in
    let in_else = ref false in
    let j = ref (i + 1) in
    let stop = ref false in
    while !j < Array.length lines && not !stop do
      let l = String.trim lines.(!j) in
      if starts_block l then
        incr depth
      else if is_end l then begin
        decr depth;
        if !depth = 0 then begin stop := true; end
      end;
      if not !stop then begin
        if is_else l && !depth = 1 then
          in_else := true
        else if !in_else then
          false_block := l :: !false_block
        else
          true_block := l :: !true_block
      end;
      if not !stop then incr j
    done;
    if not !stop then
      failwith (Printf.sprintf "if without matching end on line %d" (i + 1));
    if truthy then
      run_lines (List.rev !true_block)
    else
      run_lines (List.rev !false_block);
    !j + 1
  end

  (* sub name($params) ... end *)
  else if re_matches re_sub_def line then begin
    let name = Str.matched_group 1 line in
    if not (is_symbol_name name) then
      failwith (Printf.sprintf "Syntax error on line %d: %s" (i + 1) line);
    let raw_params = Str.matched_group 2 line in
    let params =
      list_filter_map (fun s ->
        let p = String.trim s in
        if p = "" then None
        else
          let p =
            if String.length p > 0 && p.[0] = '$' then
              String.sub p 1 (String.length p - 1)
            else
              p
          in
          if not (is_local_name p) then
            failwith ("Invalid parameter name '$" ^ p ^ "' (locals must be lowercase)");
          Some p
      ) (String.split_on_char ',' raw_params)
    in
    let body = ref [] in
    let depth = ref 1 in
    let j = ref (i + 1) in
    let stop = ref false in
    while !j < Array.length lines && not !stop do
      let l = String.trim lines.(!j) in
      if starts_block l then incr depth
      else if is_end l then begin
        decr depth;
        if !depth = 0 then (stop := true)
      end;
      if not !stop then body := l :: !body;
      if not !stop then incr j
    done;
    if not !stop then
      failwith (Printf.sprintf "sub without matching end on line %d" (i + 1));
    Hashtbl.replace (module_subs_ref !current_module) name { params; body = List.rev !body };
    !j + 1
  end

  (* alias new = old *)
  else if re_matches re_alias line then begin
    let new_name = Str.matched_group 1 line in
    let old_name = Str.matched_group 2 line in
    if not (is_symbol_name new_name) then
      failwith (Printf.sprintf "Syntax error on line %d: %s" (i + 1) line);
    if Hashtbl.mem builtins old_name then
      Hashtbl.replace builtins new_name (Hashtbl.find builtins old_name)
    else
      (match resolve_sub_target old_name with
      | Some (module_name, sub_name) ->
        Hashtbl.replace (module_subs_ref !current_module) new_name
          (Hashtbl.find (module_subs_ref module_name) sub_name)
      | None ->
        failwith ("alias: unknown function '" ^ old_name ^ "'"));
    i + 1
  end

  (* foreach $var @arr ... end *)
  else if re_matches re_foreach line then begin
    let var = Str.matched_group 1 line in
    let arrname = Str.matched_group 2 line in
    if not (is_global_name var || is_local_name var) then
      failwith ("Invalid local variable name '$" ^ var ^ "' (locals must be lowercase)");
    let arr_val =
      match split_qualified_symbol arrname with
      | Some (Some module_name, name) ->
        if is_global_name name then
          get_global_array name
        else if is_local_name name then
          get_module_array module_name name
        else
          failwith ("Invalid local array name '@" ^ name ^ "' (locals must be lowercase)")
      | Some (None, name) ->
        if is_global_name name then
          get_global_array name
        else if is_local_name name then
          get_module_array !current_module name
        else
          failwith ("Invalid local array name '@" ^ name ^ "' (locals must be lowercase)")
      | None ->
        failwith (Printf.sprintf "Syntax error on line %d: %s" (i + 1) line)
    in
    let arr_snapshot = match arr_val with
      | Arr r -> Array.sub r.data 0 r.len
      | _ -> [||] in
    let body = ref [] in
    let depth = ref 1 in
    let j = ref (i + 1) in
    let stop = ref false in
    while !j < Array.length lines && not !stop do
      let l = String.trim lines.(!j) in
      if starts_block l then incr depth
      else if is_end l then begin
        decr depth;
        if !depth = 0 then (stop := true)
      end;
      if not !stop then body := l :: !body;
      if not !stop then incr j
    done;
    if not !stop then
      failwith (Printf.sprintf "foreach without matching end on line %d" (i + 1));
    let body_lines = List.rev !body in
    let rec run_each i =
      if i >= Array.length arr_snapshot || !returning then ()
      else begin
        let elem = arr_snapshot.(i) in
        if is_global_name var then
          Hashtbl.replace globals var elem
        else
          Hashtbl.replace (module_vars_ref !current_module) var elem;
        run_lines body_lines;
        run_each (i + 1)
      end
    in
    run_each 0;
    !j + 1
  end

  (* Bare function call *)
  else if re_matches re_bare_call line then begin
    ignore (eval_expr line);
    i + 1
  end

  else
    failwith (Printf.sprintf "Syntax error on line %d: %s" (i + 1) line)

and run_lines lines =
  let arr = Array.of_list lines in
  let i = ref 0 in
  while !i < Array.length arr && not !returning do
    i := exec_line arr !i
  done

(* ============================================================
   register_builtins
   ============================================================ *)

let nth_str args n =
  match list_nth_opt args n with Some v -> to_str v | None -> ""

let nth_num args n =
  match list_nth_opt args n with Some v -> to_num v | None -> 0.0

let nth_val args n =
  match list_nth_opt args n with Some v -> v | None -> Nil

let require_arr fname = function
  | Arr r -> r
  | _ -> failwith (fname ^ ": first argument must be an array")

let require_dict fname = function
  | Dict h -> h
  | _ -> failwith (fname ^ ": first argument must be a dict")

let register_builtins () =
  let add name f = Hashtbl.replace builtins name f in

  (* Math *)
  add "add" (fun a -> Num (nth_num a 0 +. nth_num a 1));
  add "sub" (fun a -> Num (nth_num a 0 -. nth_num a 1));
  add "mul" (fun a -> Num (nth_num a 0 *. nth_num a 1));

  (* String *)
  add "concat" (fun a -> Str (nth_str a 0 ^ nth_str a 1));
  add "length" (fun a -> Num (float_of_int (String.length (nth_str a 0))));
  add "upper"  (fun a -> Str (String.uppercase_ascii (nth_str a 0)));
  add "lower"  (fun a -> Str (String.lowercase_ascii (nth_str a 0)));
  add "split"  (fun a ->
    let delim = nth_str a 0 in
    let str = nth_str a 1 in
    if delim = "" then Arr (dynarr_of_list [Str str])
    else begin
      let parts = ref [] in
      let dlen = String.length delim in
      let slen = String.length str in
      let start = ref 0 in
      let i = ref 0 in
      while !i <= slen - dlen do
        if String.sub str !i dlen = delim then begin
          parts := Str (String.sub str !start (!i - !start)) :: !parts;
          start := !i + dlen;
          i := !i + dlen
        end else
          incr i
      done;
      parts := Str (String.sub str !start (slen - !start)) :: !parts;
      Arr (dynarr_of_list (List.rev !parts))
    end);
  add "join" (fun a ->
    let delim = nth_str a 0 in
    let r = require_arr "join" (nth_val a 1) in
    Str (String.concat delim (List.map to_str (dynarr_to_list r))));

  (* Comparison *)
  add "eq" (fun a -> Num (if nth_str a 0 = nth_str a 1 then 1.0 else 0.0));
  add "gt" (fun a -> Num (if nth_num a 0 > nth_num a 1 then 1.0 else 0.0));
  add "lt" (fun a -> Num (if nth_num a 0 < nth_num a 1 then 1.0 else 0.0));
  add "not" (fun a -> Num (if is_truthy (nth_val a 0) then 0.0 else 1.0));
  add "is-empty" (fun a ->
    let v = nth_val a 0 in
    Num (match v with Nil -> 1.0 | Str "" -> 1.0 | _ -> 0.0));

  (* Regex *)
  add "matchrx" (fun a ->
    let str = nth_str a 0 in
    let rex = match nth_val a 1 with
      | Rex r -> r
      | _ -> failwith "matchrx: second argument must be a regex #\"...\"" in
    Num (if Pcre.pmatch ~rex str then 1.0 else 0.0));
  add "extract" (fun a ->
    let str = nth_str a 0 in
    let rex = match nth_val a 1 with
      | Rex r -> r
      | _ -> failwith "extract: second argument must be a regex #\"...\"" in
    try
      let ss = Pcre.extract ~rex str in
      let caps = ref [] in
      for i = Array.length ss - 1 downto 1 do
        caps := Str ss.(i) :: !caps
      done;
      Arr (dynarr_of_list !caps)
    with Not_found -> Arr (dynarr_empty ()));

  (* Array *)
  add "array" (fun a -> Arr (dynarr_of_list a));
  add "push" (fun a ->
    let r = require_arr "push" (nth_val a 0) in
    let rest =
      match a with
      | [] -> []
      | _ :: tl -> tl
    in
    List.iter (dynarr_push r) rest;
    Arr r);
  add "pop" (fun a ->
    let r = require_arr "pop" (nth_val a 0) in
    match dynarr_pop r with
    | Some v -> v
    | None -> Nil);
  add "get" (fun a ->
    let r = require_arr "get" (nth_val a 0) in
    let idx = int_of_float (nth_num a 1) in
    (match dynarr_get r idx with Some v -> v | None -> Nil));
  add "len" (fun a ->
    let r = require_arr "len" (nth_val a 0) in
    Num (float_of_int r.len));

  (* Dict *)
  add "dict" (fun a ->
    let h = Hashtbl.create 8 in
    let rec fill = function
      | k :: v :: rest -> Hashtbl.replace h (to_str k) v; fill rest
      | _ -> () in
    fill a;
    Dict h);
  add "dict-get" (fun a ->
    let h = require_dict "dict-get" (nth_val a 0) in
    let key = nth_str a 1 in
    (match hashtbl_find_opt h key with Some v -> v | None -> Nil));
  add "dict-set" (fun a ->
    let h = require_dict "dict-set" (nth_val a 0) in
    let key = nth_str a 1 in
    let v = nth_val a 2 in
    Hashtbl.replace h key v; v);
  add "dict-keys" (fun a ->
    let h = require_dict "dict-keys" (nth_val a 0) in
    Arr (dynarr_of_list (Hashtbl.fold (fun k _ acc -> Str k :: acc) h [])));
  add "dict-has" (fun a ->
    let h = require_dict "dict-has" (nth_val a 0) in
    Num (if Hashtbl.mem h (nth_str a 1) then 1.0 else 0.0));
  add "dict-del" (fun a ->
    let h = require_dict "dict-del" (nth_val a 0) in
    let key = nth_str a 1 in
    let v = match hashtbl_find_opt h key with Some v -> v | None -> Nil in
    Hashtbl.remove h key; v);

  (* File I/O *)
  add "save" (fun a ->
    let path = nth_str a 0 in
    let content = nth_str a 1 in
    if path = "" then failwith "save: missing filename";
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    Str path);
  add "write-file" (fun a ->
    let text = nth_str a 0 in
    let path = nth_str a 1 in
    if path = "" then failwith "write-file: missing path";
    let oc = open_out path in
    output_string oc text;
    close_out oc;
    Str path);
  add "read-file" (fun a ->
    let file = nth_str a 0 in
    if file = "" then failwith "read-file: missing filename";
    let ic = open_in file in
    let n = in_channel_length ic in
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    Str (Bytes.to_string buf));
  add "write-lines-file" (fun a ->
    let r = require_arr "write-lines-file" (nth_val a 0) in
    let path = nth_str a 1 in
    if path = "" then failwith "write-lines-file: missing path";
    let oc = open_out path in
    dynarr_iter (fun v -> output_string oc (to_str v ^ "\n")) r;
    close_out oc;
    Str path);
  add "read-file-lines" (fun a ->
    let file = nth_str a 0 in
    if file = "" then failwith "read-file-lines: missing filename";
    let ic = open_in file in
    let lines = ref [] in
    (try while true do lines := input_line ic :: !lines done
     with End_of_file -> ());
    close_in ic;
    Arr (dynarr_of_list (List.rev_map (fun s -> Str s) !lines)));
  add "read-dir" (fun a ->
    let dir = nth_str a 0 in
    if dir = "" then failwith "read-dir: missing directory";
    let dh = Unix.opendir dir in
    let entries = ref [] in
    (try while true do
      let e = Unix.readdir dh in
      if e <> "." && e <> ".." then entries := Str e :: !entries
    done with End_of_file -> ());
    Unix.closedir dh;
    Arr (dynarr_of_list (List.rev !entries)));
  add "load" (fun a ->
    let file = nth_str a 0 in
    if file = "" then failwith "load: missing filename";
    let path = resolve_load_path file in
    let ic = open_in path in
    let lines = ref [] in
    (try while true do lines := input_line ic :: !lines done
     with End_of_file -> ());
    close_in ic;
    let module_name = module_name_from_file path in
    Hashtbl.replace module_dirs module_name (Filename.dirname path);
    ignore (module_vars_ref module_name);
    ignore (module_arrays_ref module_name);
    ignore (module_dicts_ref module_name);
    ignore (module_subs_ref module_name);
    let saved_module = !current_module in
    current_module := module_name;
    (try !run_lines_ref (List.rev !lines)
     with exn ->
       current_module := saved_module;
       raise exn);
    current_module := saved_module;
    Str file);
  add "file-exists" (fun a ->
    let f = nth_str a 0 in
    Num (if f <> "" && Sys.file_exists f && not (Sys.is_directory f) then 1.0 else 0.0));
  add "dir-exists" (fun a ->
    let d = nth_str a 0 in
    Num (if d <> "" && Sys.file_exists d && Sys.is_directory d then 1.0 else 0.0));
  add "mkdir" (fun a ->
    let dir = nth_str a 0 in
    if dir = "" then failwith "mkdir: missing directory";
    (* mkdir -p equivalent: create each component *)
    let parts = String.split_on_char '/' dir in
    let _ = List.fold_left (fun acc p ->
      let path = if acc = "" then p else acc ^ "/" ^ p in
      if path <> "" && not (Sys.file_exists path) then
        Unix.mkdir path 0o755;
      path
    ) "" parts in
    Str dir);
  add "mv" (fun a ->
    let old_path = nth_str a 0 in
    let new_path = nth_str a 1 in
    if old_path = "" || new_path = "" then failwith "mv: missing arguments";
    Sys.rename old_path new_path;
    Str new_path);
  add "cp" (fun a ->
    let old_path = nth_str a 0 in
    let new_path = nth_str a 1 in
    if old_path = "" || new_path = "" then failwith "cp: missing arguments";
    let ic = open_in old_path in
    let oc = open_out new_path in
    (try while true do output_char oc (input_char ic) done
     with End_of_file -> ());
    close_in ic; close_out oc;
    Str new_path);

  (* Misc *)
  add "user-input" (fun a ->
    let prompt = nth_str a 0 in
    if prompt <> "" then (print_string prompt; flush stdout);
    let line = try input_line stdin with End_of_file -> "" in
    Str line);
  add "die" (fun a ->
    let msg = nth_str a 0 in
    failwith (if msg = "" then "died" else msg));
  add "sh" (fun a ->
    let cmd = nth_str a 0 in
    if cmd = "" then failwith "sh: missing command";
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 256 in
    (try while true do Buffer.add_char buf (input_char ic) done
     with End_of_file -> ());
    let status = Unix.close_process_in ic in
    (match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
      failwith (Printf.sprintf "sh: '%s' exited with status %d" cmd n)
    | Unix.WSIGNALED n ->
      failwith (Printf.sprintf "sh: '%s' terminated by signal %d" cmd n)
    | Unix.WSTOPPED n ->
      failwith (Printf.sprintf "sh: '%s' stopped by signal %d" cmd n));
    (* chomp trailing newline *)
    let s = Buffer.contents buf in
    let s = if String.length s > 0 && s.[String.length s - 1] = '\n'
            then String.sub s 0 (String.length s - 1) else s in
    Str s)

(* ============================================================
   Main
   ============================================================ *)

let () =
  register_builtins ();
  run_lines_ref := run_lines;

  let lines =
    if Array.length Sys.argv > 1 then begin
      let file = Sys.argv.(1) in
      let ic = open_in file in
      let lines = ref [] in
      (try while true do lines := input_line ic :: !lines done
       with End_of_file -> ());
      close_in ic;
      Hashtbl.replace module_dirs main_module (Filename.dirname file);
      (* Set $PATH and $ARG1..$ARGN, @ARGS as globals *)
      Hashtbl.replace globals "PATH" (Str file);
      let args = ref [] in
      for i = Array.length Sys.argv - 1 downto 2 do
        Hashtbl.replace globals (Printf.sprintf "ARG%d" (i - 1)) (Str Sys.argv.(i));
        args := Str Sys.argv.(i) :: !args
      done;
      Hashtbl.replace global_arrays "ARGS" (Arr (dynarr_of_list !args));
      List.rev !lines
    end else begin
      let lines = ref [] in
      (try while true do lines := input_line stdin :: !lines done
       with End_of_file -> ());
      Hashtbl.replace global_arrays "ARGS" (Arr (dynarr_empty ()));
      List.rev !lines
    end
  in
  let env_dict = Hashtbl.create 64 in
  Array.iter (fun entry ->
    match string_index_opt entry '=' with
    | Some idx ->
      let k = String.sub entry 0 idx in
      let v = String.sub entry (idx + 1) (String.length entry - idx - 1) in
      Hashtbl.replace env_dict k (Str v)
    | None -> ()
  ) (Unix.environment ());
  Hashtbl.replace global_dicts "ENV" (Dict env_dict);
  run_lines lines
