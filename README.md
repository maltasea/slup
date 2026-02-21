# simp

`simp` is a small scripting language interpreter.

Main entrypoints:
- Perl: `perl simp.pl <script.simp>`
- OCaml build: `dune build ./simp.exe` then `_build/default/simp.exe <script.simp>`

Execution model:
- line-based parse (one line = one statement, except block constructs)
- scripts/modules are compiled into a simple AST before execution

## Globals vs Locals

Name rules:
- Globals: uppercase symbols (for example: `$DB_HOST`, `@ARGS`, `%ENV`, `$MY-GLOBAL?`)
- Locals: lowercase symbols (for example: `$host`, `@items`, `%cfg`)

Supported symbol characters include:
- `-`
- `->`
- `?`

Examples:

```simp
set $host = "localhost"   # local
$DB_HOST = "localhost"    # global
set @items = ["a", "b"]   # local array
set @ARGS = ["x", "y"]    # global array
```

## Declaring Globals

You can declare globals explicitly:

```simp
global $DB_HOST required
global $DB_PORT default("5432")
```

Declaration modifiers:
- `required`
- `default(<expr>)`

## Static Checking

Use static mode to verify global declarations/assignments without executing the script:

```sh
perl simp.pl --check main.simp
```

`--check` enforces:
- uppercase global declarations
- no undeclared global assignments
- required globals must be assigned (unless they have `default(...)`)

It recursively scans `load("...")` when the load target is a string literal.

Limitation:
- dynamic loads (for example `load($path)`) are rejected in static mode because they cannot be resolved statically.

## Strict Runtime Mode

Use strict mode to enforce global declarations at runtime:

```sh
perl simp.pl --strict-globals main.simp
```

`--strict-globals` does:
- startup static precheck (`--check` behavior)
- runtime error on undeclared global read/assignment
- runtime required-global validation after execution

Built-in globals predeclared by the interpreter:
- `$PATH`
- `$ARG1`, `$ARG2`, ...
- `@ARGS`
- `%ENV`

## Modules and Namespacing

Load modules:

```simp
load("moda")
```

Module-local symbols are namespaced:
- function call: `moda/who("x")`
- variable read: `$moda/value`
- array read: `@moda/items`
- dict read: `%moda/cfg`

Main module symbols stay callable without module prefix.

## Shell Commands

Use `run` for one command (argv form, no shell parsing):

```simp
set %r = run(["/bin/sh", "-c", "printf ok; printf warn 1>&2"])
print(dict-get(%r, "code"))
print(dict-get(%r, "out"))
print(dict-get(%r, "err"))
```

Use `pipe` for command pipelines:

```simp
set %r = pipe([["/bin/sh", "-c", "printf hi"], ["tr", "a-z", "A-Z"]])
print(dict-get(%r, "code"))
print(dict-get(%r, "out"))
print(dict-get(%r, "err"))
```

Both return a dict:
- `code` (exit status; for pipelines, the last command)
- `out` (captured stdout)
- `err` (captured stderr)

## Tests

Run all tests:

```sh
./tests/run-tests.sh
```

Key test suites:
- `tests/simp.t`
- `tests/conformance.t`
- `tests/static-check.t`
- `tests/strict-globals.t`
- `tests/simp-ocaml.t`

## Benchmark

Quick call-overhead benchmark:

```sh
perl bench/call-overhead.pl --impl perl --calls 20000 --iters 5 --warmup 1
opam exec -- dune build ./simp.exe
opam exec -- perl bench/call-overhead.pl --impl ocaml --calls 20000 --iters 5 --warmup 1
```

It compares:
- baseline loop with no user-sub call
- equivalent loop that calls a user-defined `sub` each iteration

Convenience targets:

```sh
make bench-call-perl
make bench-call-ocaml
```

Extended overhead benchmark suite:

```sh
perl bench/overhead-suite.pl --calls 20000 --depth 400 --iters 5 --warmup 1
```

It reports:
- recursion call overhead vs a loop baseline
- module-qualified calls (`mod/sub(...)`) vs local sub calls
- strict globals runtime cost (`--strict-globals`) vs normal mode

Append a dated benchmark snapshot:

```sh
make bench
```

`make bench` runs the overhead suite and appends results to `bench/baseline.md`.
It also prints a comparison against the previous baseline snapshot.
Each run also writes a machine-readable JSON snapshot to `bench/history/<timestamp>.json`.

Optional overrides:

```sh
make bench BENCH_CALLS=20000 BENCH_DEPTH=400 BENCH_ITERS=5 BENCH_WARMUP=1
```

Compare the latest two snapshots without running a new benchmark:

```sh
make bench-compare
```

`make bench-compare` reads the latest two files in `bench/history/`.

Fail if latest snapshot regressed beyond thresholds:

```sh
make bench-guard
```

Default max regression thresholds are 10% per metric.

Threshold overrides:

```sh
make bench-guard BENCH_MAX_REC_PCT=4 BENCH_MAX_MOD_PCT=3 BENCH_MAX_STRICT_PCT=2
```

`make bench-guard` also reads from `bench/history/`.
