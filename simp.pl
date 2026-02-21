#!/usr/bin/env perl
use strict;
use warnings;
use File::Spec;
use File::Temp qw(tempfile);

# ============================================================
#  simp — a simple scripting language interpreter in Perl
# ============================================================

# --- Variable store ---
my $MAIN_MODULE = 'main';
our $current_module = $MAIN_MODULE;
my %globals;
my %global_arrays;
my %global_dicts;
my %global_decls; # name => { required => 0|1, has_default => 0|1, default_expr => ... }
my $strict_globals_mode = 0;
my %module_vars   = ($MAIN_MODULE => {});
my %module_var_frames = ($MAIN_MODULE => []);
my %module_arrays = ($MAIN_MODULE => {});
my %module_dicts  = ($MAIN_MODULE => {});
my %module_subs   = ($MAIN_MODULE => {}); # name => { params => [...], body => [...] }
my %module_dirs   = ($MAIN_MODULE => '.');
my $MODULE_NAME_RE = qr/[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z0-9_]+)*/;
my $SYMBOL_NAME_RE = qr/[A-Za-z_][A-Za-z0-9_]*(?:(?:->|-|\?)[A-Za-z0-9_]+)*\??/;
our $call_depth = 0;
our $returning = 0;

# --- Built-in function registry ---
my %builtins = (
    'is-empty' => sub {
        my ($val) = @_;
        return (!defined $val || $val eq '') ? 1 : 0;
    },
    'add' => sub { return ($_[0] // 0) + ($_[1] // 0) },
    'sub' => sub { return ($_[0] // 0) - ($_[1] // 0) },
    'mul' => sub { return ($_[0] // 0) * ($_[1] // 0) },
    'concat' => sub { return ($_[0] // '') . ($_[1] // '') },
    'length' => sub { return length($_[0] // '') },
    'upper'  => sub { return uc($_[0] // '') },
    'lower'  => sub { return lc($_[0] // '') },
    'not'    => sub { return $_[0] ? 0 : 1 },
    'eq'     => sub { return ($_[0] // '') eq ($_[1] // '') ? 1 : 0 },
    'gt'     => sub { return ($_[0] // 0) >  ($_[1] // 0) ? 1 : 0 },
    'lt'     => sub { return ($_[0] // 0) <  ($_[1] // 0) ? 1 : 0 },
    'extract' => sub {
        my ($str, $pat) = @_;
        $str //= '';
        die "extract: second argument must be a regex #\"...\"\n" unless defined $pat;
        my @caps = ($str =~ $pat);
        return @caps ? \@caps : [];
    },
    'matchrx' => sub {
        my ($str, $pat) = @_;
        $str //= '';
        die "matchrx: second argument must be a regex #\"...\"\n" unless defined $pat;
        return ($str =~ $pat) ? 1 : 0;
    },
    'array'  => sub { return [@_] },
    'dict'   => sub {
        my %h;
        while (@_) { my $k = shift; $h{$k} = shift }
        return \%h;
    },
    'dict-get'   => sub {
        my ($d, $key) = @_;
        die "dict-get: first argument must be a dict\n" unless ref $d eq 'HASH';
        return $d->{$key};
    },
    'dict-set'   => sub {
        my ($d, $key, $val) = @_;
        die "dict-set: first argument must be a dict\n" unless ref $d eq 'HASH';
        $d->{$key} = $val;
        return $val;
    },
    'dict-keys'  => sub {
        my ($d) = @_;
        die "dict-keys: argument must be a dict\n" unless ref $d eq 'HASH';
        return [keys %$d];
    },
    'dict-has'   => sub {
        my ($d, $key) = @_;
        die "dict-has: first argument must be a dict\n" unless ref $d eq 'HASH';
        return exists $d->{$key} ? 1 : 0;
    },
    'dict-del'   => sub {
        my ($d, $key) = @_;
        die "dict-del: first argument must be a dict\n" unless ref $d eq 'HASH';
        return delete $d->{$key};
    },
    'push'   => sub {
        my $arr = shift;
        die "push: first argument must be an array\n" unless ref $arr eq 'ARRAY';
        push @$arr, @_;
        return $arr;
    },
    'pop'    => sub {
        my ($arr) = @_;
        die "pop: argument must be an array\n" unless ref $arr eq 'ARRAY';
        return pop @$arr;
    },
    'get'    => sub {
        my ($arr, $idx) = @_;
        die "get: first argument must be an array\n" unless ref $arr eq 'ARRAY';
        return $arr->[$idx // 0];
    },
    'len'    => sub {
        my ($arr) = @_;
        die "len: argument must be an array\n" unless ref $arr eq 'ARRAY';
        return scalar @$arr;
    },
    'save'   => sub {
        my ($path, $content) = @_;
        die "save: missing filename\n" unless defined $path && $path ne '';
        open my $fh, '>', $path or die "save: cannot open '$path': $!\n";
        print $fh ($content // '');
        close $fh;
        return $path;
    },
    'mkdir' => sub {
        my ($dir) = @_;
        die "mkdir: missing directory\n" unless defined $dir && $dir ne '';
        require File::Path;
        File::Path::make_path($dir);
        return $dir;
    },
    'dir-exists' => sub {
        my ($dir) = @_;
        return (defined $dir && -d $dir) ? 1 : 0;
    },
    'file-exists' => sub {
        my ($file) = @_;
        return (defined $file && -f $file) ? 1 : 0;
    },
    'join' => sub {
        my ($delim, $arr) = @_;
        die "join: second argument must be an array\n" unless ref $arr eq 'ARRAY';
        return join(($delim // ''), @$arr);
    },
    'split' => sub {
        my ($delim, $str) = @_;
        return [split quotemeta($delim // ''), ($str // '')];
    },
    'read-file' => sub {
        my ($file) = @_;
        die "read-file: missing filename\n" unless defined $file && $file ne '';
        open my $fh, '<', $file or die "read-file: cannot open '$file': $!\n";
        local $/;
        my $content = <$fh>;
        close $fh;
        return $content;
    },
    'write-lines-file' => sub {
        my ($arr, $path) = @_;
        die "write-lines-file: first argument must be an array\n" unless ref $arr eq 'ARRAY';
        die "write-lines-file: missing path\n" unless defined $path && $path ne '';
        open my $fh, '>', $path or die "write-lines-file: cannot open '$path': $!\n";
        print $fh "$_\n" for @$arr;
        close $fh;
        return $path;
    },
    'read-file-lines' => sub {
        my ($file) = @_;
        die "read-file-lines: missing filename\n" unless defined $file && $file ne '';
        open my $fh, '<', $file or die "read-file-lines: cannot open '$file': $!\n";
        chomp(my @lines = <$fh>);
        close $fh;
        return \@lines;
    },
    'read-dir' => sub {
        my ($dir) = @_;
        die "read-dir: missing directory\n" unless defined $dir && $dir ne '';
        opendir my $dh, $dir or die "read-dir: cannot open '$dir': $!\n";
        my @entries = grep { $_ ne '.' && $_ ne '..' } readdir $dh;
        closedir $dh;
        return \@entries;
    },
    'load' => sub {
        my ($file) = @_;
        die "load: missing filename\n" unless defined $file && $file ne '';
        my $path = resolve_load_path($file);
        open my $fh, '<', $path or die "load: cannot open '$file': $!\n";
        chomp(my @lines = <$fh>);
        close $fh;

        my $module = module_name_from_file($path);
        require File::Basename;
        $module_dirs{$module} = File::Basename::dirname($path);
        module_vars_ref($module);
        module_var_frames_ref($module);
        module_arrays_ref($module);
        module_dicts_ref($module);
        module_subs_ref($module);

        my $nodes = compile_program(\@lines);
        local $current_module = $module;
        run_lines($nodes);
        return $file;
    },
    'user-input' => sub {
        my ($prompt) = @_;
        print($prompt // '') if defined $prompt && $prompt ne '';
        my $line = <STDIN>;
        chomp $line if defined $line;
        return $line;
    },
    'die' => sub {
        my ($msg) = @_;
        die(($msg // 'died') . "\n");
    },
    'sh' => sub {
        my ($cmd) = @_;
        die "sh: missing command\n" unless defined $cmd && $cmd ne '';
        my $out = `$cmd`;
        die "sh: failed to execute '$cmd': $!\n" if $? == -1;
        if ($? & 127) {
            my $sig = $? & 127;
            die "sh: '$cmd' terminated by signal $sig\n";
        }
        if ($? != 0) {
            my $code = $? >> 8;
            die "sh: '$cmd' exited with status $code\n";
        }
        chomp $out;
        return $out;
    },
    'run' => sub {
        my ($argv) = @_;
        die "run: first argument must be an array\n" unless ref($argv) eq 'ARRAY';
        return run_command_capture($argv);
    },
    'pipe' => sub {
        my ($commands) = @_;
        die "pipe: first argument must be an array of command arrays\n" unless ref($commands) eq 'ARRAY';
        return run_pipeline_capture($commands);
    },
    'mv' => sub {
        my ($old, $new) = @_;
        die "mv: missing arguments\n" unless defined $old && defined $new;
        rename $old, $new or die "mv: cannot rename '$old' to '$new': $!\n";
        return $new;
    },
    'cp' => sub {
        my ($old, $new) = @_;
        die "cp: missing arguments\n" unless defined $old && defined $new;
        open my $in,  '<', $old or die "cp: cannot open '$old': $!\n";
        open my $out, '>', $new or die "cp: cannot open '$new': $!\n";
        print $out $_ while <$in>;
        close $in; close $out;
        return $new;
    },
    'write-file' => sub {
        my ($text, $path) = @_;
        die "write-file: missing path\n" unless defined $path && $path ne '';
        open my $fh, '>', $path or die "write-file: cannot open '$path': $!\n";
        print $fh ($text // '');
        close $fh;
        return $path;
    },
);

# ============================================================
#  Module helpers
# ============================================================

sub module_vars_ref {
    my ($module) = @_;
    $module //= $current_module;
    $module_vars{$module} //= {};
    return $module_vars{$module};
}

sub module_arrays_ref {
    my ($module) = @_;
    $module //= $current_module;
    $module_arrays{$module} //= {};
    return $module_arrays{$module};
}

sub module_var_frames_ref {
    my ($module) = @_;
    $module //= $current_module;
    $module_var_frames{$module} //= [];
    return $module_var_frames{$module};
}

sub local_var_lookup {
    my ($module, $name) = @_;
    my $frames = module_var_frames_ref($module);
    for (my $i = $#$frames; $i >= 0; $i--) {
        my $frame = $frames->[$i];
        if (exists $frame->{$name}) {
            return (1, $frame->{$name});
        }
    }
    my $base = module_vars_ref($module);
    if (exists $base->{$name}) {
        return (1, $base->{$name});
    }
    return (0, undef);
}

sub local_var_set {
    my ($module, $name, $value) = @_;
    my $frames = module_var_frames_ref($module);
    if (@$frames) {
        $frames->[-1]{$name} = $value;
    } else {
        module_vars_ref($module)->{$name} = $value;
    }
    return $value;
}

sub module_dicts_ref {
    my ($module) = @_;
    $module //= $current_module;
    $module_dicts{$module} //= {};
    return $module_dicts{$module};
}

sub module_subs_ref {
    my ($module) = @_;
    $module //= $current_module;
    $module_subs{$module} //= {};
    return $module_subs{$module};
}

sub module_name_from_file {
    my ($file) = @_;
    require File::Basename;
    my $name = File::Basename::basename($file);
    $name =~ s/\.[^.]+$//;
    die "load: invalid module name '$name'\n" unless $name =~ /^$MODULE_NAME_RE$/;
    return $name;
}

sub resolve_load_path {
    my ($file) = @_;
    require File::Spec;
    my @candidates;
    push @candidates, $file;
    push @candidates, "$file.simp" if $file !~ /\.[^\/\\]+$/;

    my $base_dir = $module_dirs{$current_module} // '.';
    if (!File::Spec->file_name_is_absolute($file)) {
        push @candidates, File::Spec->catfile($base_dir, $file);
        push @candidates, File::Spec->catfile($base_dir, "$file.simp")
            if $file !~ /\.[^\/\\]+$/;
    }

    for my $candidate (@candidates) {
        return $candidate if -f $candidate;
    }
    return $file;
}

sub resolve_sub_target {
    my ($fname) = @_;
    if ($fname =~ /^($MODULE_NAME_RE)\/($SYMBOL_NAME_RE)$/) {
        my ($module, $sub_name) = ($1, $2);
        my $subs = module_subs_ref($module);
        return ($module, $sub_name) if exists $subs->{$sub_name};
        return;
    }

    my $curr_subs = module_subs_ref($current_module);
    return ($current_module, $fname) if exists $curr_subs->{$fname};

    my $main_subs = module_subs_ref($MAIN_MODULE);
    return ($MAIN_MODULE, $fname) if exists $main_subs->{$fname};

    return;
}

sub is_global_name {
    my ($name) = @_;
    return 0 unless defined $name && is_symbol_name($name);
    return $name =~ /[A-Z]/ && $name !~ /[a-z]/;
}

sub is_local_name {
    my ($name) = @_;
    return 0 unless defined $name && is_symbol_name($name);
    return $name !~ /[A-Z]/;
}

sub is_symbol_name {
    my ($name) = @_;
    return defined $name && $name =~ /^$SYMBOL_NAME_RE$/;
}

sub require_declared_global {
    my ($name, $where, $verb) = @_;
    return unless $strict_globals_mode;
    return if exists $global_decls{$name};
    $verb //= 'use';
    die "$where: undeclared global $verb: '\$$name'\n";
}

sub predeclare_global_if_missing {
    my ($name) = @_;
    $global_decls{$name} //= {
        required => 0,
        has_default => 0,
        default_expr => undef,
    };
}

sub validate_required_globals_runtime {
    for my $name (sort keys %global_decls) {
        my $d = $global_decls{$name};
        next unless $d->{required};
        next if $d->{has_default};
        next if exists $globals{$name};
        die "required global is not assigned at runtime: '\$$name'\n";
    }
}

sub parse_plain_string_literal {
    my ($raw) = @_;
    my $out = '';
    my $i = 0;
    while ($i < length $raw) {
        my $ch = substr($raw, $i, 1);
        if ($ch eq '\\') {
            $i++;
            if ($i >= length $raw) {
                $out .= '\\';
                last;
            }
            my $esc = substr($raw, $i, 1);
            if ($esc eq 'n') {
                $out .= "\n";
            } elsif ($esc eq 't') {
                $out .= "\t";
            } elsif ($esc eq 'r') {
                $out .= "\r";
            } elsif ($esc eq '"' || $esc eq '\\' || $esc eq '$') {
                $out .= $esc;
            } else {
                $out .= '\\' . $esc;
            }
        } else {
            $out .= $ch;
        }
        $i++;
    }
    return $out;
}

sub parse_global_decl_modifier {
    my ($raw) = @_;
    my %spec = (required => 0, has_default => 0, default_expr => undef);
    return \%spec unless defined $raw && $raw =~ /\S/;
    $raw =~ s/^\s+//;
    $raw =~ s/\s+$//;
    if ($raw eq 'required') {
        $spec{required} = 1;
        return \%spec;
    }
    if ($raw =~ /^default\s*\((.*)\)$/) {
        $spec{has_default} = 1;
        $spec{default_expr} = $1;
        return \%spec;
    }
    die "global: expected 'required' or 'default(<expr>)'\n";
}

sub declare_global_spec {
    my ($name, $spec, $where) = @_;
    die "$where: global name must be uppercase: '\$$name'\n" unless is_global_name($name);
    my $existing = $global_decls{$name};
    if ($existing) {
        if ($existing->{required} != $spec->{required}
            || $existing->{has_default} != $spec->{has_default}
            || (($existing->{default_expr} // '') ne ($spec->{default_expr} // ''))) {
            die "$where: conflicting declaration for '\$$name'\n";
        }
        return;
    }
    $global_decls{$name} = {
        required => $spec->{required},
        has_default => $spec->{has_default},
        default_expr => $spec->{default_expr},
    };
}

sub resolve_load_path_from_file {
    my ($from_file, $target) = @_;
    require File::Spec;
    require File::Basename;
    my @candidates;
    push @candidates, $target;
    push @candidates, "$target.simp" if $target !~ /\.[^\/\\]+$/;

    if (!File::Spec->file_name_is_absolute($target)) {
        my $base_dir = File::Basename::dirname($from_file);
        push @candidates, File::Spec->catfile($base_dir, $target);
        push @candidates, File::Spec->catfile($base_dir, "$target.simp")
            if $target !~ /\.[^\/\\]+$/;
    }

    for my $candidate (@candidates) {
        return $candidate if -f $candidate;
    }
    return;
}

sub parse_load_literal_target {
    my ($line) = @_;
    return unless $line =~ /^load\s*\((.*)\)\s*$/;
    my @args = parse_arglist($1);
    return '__DYNAMIC__' unless @args == 1;
    my $arg = $args[0];
    $arg =~ s/^\s+//;
    $arg =~ s/\s+$//;
    return '__DYNAMIC__' unless $arg =~ /^"((?:\\.|[^"\\])*)"$/;
    return parse_plain_string_literal($1);
}

sub static_scan_file {
    my ($path, $state) = @_;
    return if $state->{seen}{$path}++;

    open my $fh, '<', $path or do {
        push @{$state->{errors}}, "$path: cannot open: $!";
        return;
    };
    my @lines = <$fh>;
    close $fh;
    chomp @lines;

    for my $i (0 .. $#lines) {
        my $line = $lines[$i];
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        next if $line eq '' || $line =~ /^#/;

        if ($line =~ /^global\s+\$($SYMBOL_NAME_RE)(?:\s+(.+))?$/) {
            my ($name, $raw_mod) = ($1, $2);
            my $where = "$path:" . ($i + 1);
            my $spec;
            eval { $spec = parse_global_decl_modifier($raw_mod); 1 } or do {
                my $err = $@ || 'global declaration parse error';
                chomp $err;
                push @{$state->{errors}}, "$where: $err";
                next;
            };
            if (!is_global_name($name)) {
                push @{$state->{errors}}, "$where: global name must be uppercase: '\$$name'";
                next;
            }
            my $existing = $state->{decls}{$name};
            if ($existing) {
                if ($existing->{required} != $spec->{required}
                    || $existing->{has_default} != $spec->{has_default}
                    || (($existing->{default_expr} // '') ne ($spec->{default_expr} // ''))) {
                    push @{$state->{errors}}, "$where: conflicting declaration for '\$$name'";
                }
            } else {
                $state->{decls}{$name} = $spec;
            }
            next;
        }

        if ($line =~ /^\$($SYMBOL_NAME_RE)\s*=\s*(.+)$/) {
            my $name = $1;
            my $where = "$path:" . ($i + 1);
            if (!is_global_name($name)) {
                push @{$state->{errors}}, "$where: global names must be uppercase: '\$$name'";
            } else {
                $state->{assigned}{$name} = 1;
            }
            next;
        }

        if ($line =~ /^set\s+\$($SYMBOL_NAME_RE)\s*=\s*(.+)$/) {
            my $name = $1;
            my $where = "$path:" . ($i + 1);
            if (is_global_name($name)) {
                $state->{assigned}{$name} = 1;
            } elsif (!is_local_name($name)) {
                push @{$state->{errors}}, "$where: local variable names must be lowercase: '\$$name'";
            }
            next;
        }

        my $target = parse_load_literal_target($line);
        next unless defined $target;
        my $where = "$path:" . ($i + 1);
        if ($target eq '__DYNAMIC__') {
            push @{$state->{errors}}, "$where: static check requires load(\"literal\")";
            next;
        }
        my $resolved = resolve_load_path_from_file($path, $target);
        if (!defined $resolved) {
            push @{$state->{errors}}, "$where: cannot statically resolve load('$target')";
            next;
        }
        static_scan_file($resolved, $state);
    }
}

sub run_static_check {
    my ($entry) = @_;
    my %state = (
        seen => {},
        decls => {},
        assigned => {},
        errors => [],
    );
    static_scan_file($entry, \%state);

    for my $name (sort keys %{$state{assigned}}) {
        next if exists $state{decls}{$name};
        push @{$state{errors}}, "undeclared global assignment: '\$$name'";
    }

    for my $name (sort keys %{$state{decls}}) {
        my $d = $state{decls}{$name};
        next unless $d->{required};
        next if $d->{has_default};
        next if $state{assigned}{$name};
        push @{$state{errors}}, "required global is never assigned: '\$$name'";
    }

    if (@{$state{errors}}) {
        print STDERR "$_\n" for @{$state{errors}};
        return 0;
    }
    return 1;
}

sub slurp_file {
    my ($path) = @_;
    open my $fh, '<', $path or die "cannot open '$path': $!\n";
    local $/;
    my $content = <$fh>;
    close $fh;
    return $content // '';
}

sub normalize_command_argv {
    my ($argv, $ctx) = @_;
    die "$ctx: command must be an array\n" unless ref($argv) eq 'ARRAY';
    die "$ctx: command array must not be empty\n" unless @$argv;
    return [map { defined $_ ? "$_" : '' } @$argv];
}

sub status_to_code {
    my ($status) = @_;
    return -1 if !defined $status || $status == -1;
    return 128 + ($status & 127) if ($status & 127);
    return $status >> 8;
}

sub run_command_capture {
    my ($argv) = @_;
    my $cmd = normalize_command_argv($argv, 'run');
    my ($outfh, $outpath) = tempfile();
    my ($errfh, $errpath) = tempfile();

    my $pid = fork();
    die "run: fork failed: $!\n" unless defined $pid;
    if ($pid == 0) {
        open STDIN, '<', File::Spec->devnull() or die "run: cannot open devnull: $!\n";
        open STDOUT, '>', $outpath or die "run: cannot open stdout capture: $!\n";
        open STDERR, '>', $errpath or die "run: cannot open stderr capture: $!\n";
        my $ok = exec { $cmd->[0] } @$cmd;
        print STDERR "run: failed to execute '$cmd->[0]': $!\n";
        exit 127;
    }

    close $outfh;
    close $errfh;
    waitpid($pid, 0);
    my $code = status_to_code($?);
    my $out = slurp_file($outpath);
    my $err = slurp_file($errpath);

    unlink $outpath;
    unlink $errpath;

    return {
        code => $code,
        out  => $out,
        err  => $err,
    };
}

sub run_pipeline_capture {
    my ($commands) = @_;
    die "pipe: command list must not be empty\n" unless @$commands;
    my @cmds = map {
        normalize_command_argv($_, 'pipe');
    } @$commands;

    my ($outfh, $outpath) = tempfile();
    my ($errfh, $errpath) = tempfile();
    close $outfh;
    close $errfh;

    my @pids;
    my $prev_read;
    for my $i (0 .. $#cmds) {
        my ($read_end, $write_end);
        if ($i < $#cmds) {
            pipe($read_end, $write_end) or die "pipe: failed to create pipe: $!\n";
        }

        my $pid = fork();
        die "pipe: fork failed: $!\n" unless defined $pid;
        if ($pid == 0) {
            if (defined $prev_read) {
                open STDIN, '<&', $prev_read or die "pipe: cannot connect stdin: $!\n";
            } else {
                open STDIN, '<', File::Spec->devnull() or die "pipe: cannot open devnull: $!\n";
            }

            if ($i < $#cmds) {
                open STDOUT, '>&', $write_end or die "pipe: cannot connect stdout: $!\n";
            } else {
                open STDOUT, '>', $outpath or die "pipe: cannot open stdout capture: $!\n";
            }

            open STDERR, '>>', $errpath or die "pipe: cannot open stderr capture: $!\n";

            close $read_end if defined $read_end;
            close $write_end if defined $write_end;
            close $prev_read if defined $prev_read;

            my $cmd = $cmds[$i];
            my $ok = exec { $cmd->[0] } @$cmd;
            print STDERR "pipe: failed to execute '$cmd->[0]': $!\n";
            exit 127;
        }

        push @pids, $pid;
        close $prev_read if defined $prev_read;
        if ($i < $#cmds) {
            close $write_end;
            $prev_read = $read_end;
        } else {
            $prev_read = undef;
        }
    }
    close $prev_read if defined $prev_read;

    my %statuses;
    for my $pid (@pids) {
        waitpid($pid, 0);
        $statuses{$pid} = $?;
    }

    my $last_status = $statuses{$pids[-1]};
    my $code = status_to_code($last_status);
    my $out = slurp_file($outpath);
    my $err = slurp_file($errpath);

    unlink $outpath;
    unlink $errpath;

    return {
        code => $code,
        out  => $out,
        err  => $err,
    };
}

# ============================================================
#  Parser helpers
# ============================================================

sub parse_string_literal {
    my ($raw) = @_;
    my $out = '';
    my $i = 0;

    while ($i < length $raw) {
        my $ch = substr($raw, $i, 1);
        if ($ch eq '\\') {
            $i++;
            if ($i >= length $raw) {
                $out .= '\\';
                last;
            }
            my $esc = substr($raw, $i, 1);
            if ($esc eq 'n') {
                $out .= "\n";
            } elsif ($esc eq 't') {
                $out .= "\t";
            } elsif ($esc eq 'r') {
                $out .= "\r";
            } elsif ($esc eq '"' || $esc eq '\\' || $esc eq '$') {
                $out .= $esc;
            } else {
                $out .= '\\' . $esc;
            }
        } elsif ($ch eq '$') {
            my $rest = substr($raw, $i);
            if ($rest =~ /^\$($MODULE_NAME_RE)\/($SYMBOL_NAME_RE)/) {
                my ($module, $name) = ($1, $2);
                if (is_global_name($name)) {
                    require_declared_global($name, "string interpolation", 'read');
                    $out .= ($globals{$name} // '');
                } else {
                    die "Invalid local variable name '\$$name' (locals must be lowercase)\n"
                        unless is_local_name($name);
                    my ($found, $val) = local_var_lookup($module, $name);
                    $out .= ($found ? ($val // '') : '');
                }
                $i += length($module) + 1 + length($name);
            } elsif ($rest =~ /^\$($SYMBOL_NAME_RE)/) {
                my $name = $1;
                if (is_global_name($name)) {
                    require_declared_global($name, "string interpolation", 'read');
                    $out .= ($globals{$name} // '');
                } else {
                    die "Invalid local variable name '\$$name' (locals must be lowercase)\n"
                        unless is_local_name($name);
                    my ($found, $val) = local_var_lookup($current_module, $name);
                    if ($found) {
                        $out .= ($val // '');
                    } else {
                        $out .= ($globals{$name} // '');
                    }
                }
                $i += length($name);
            } else {
                $out .= '$';
            }
        } else {
            $out .= $ch;
        }
        $i++;
    }

    return $out;
}

sub split_dict_pair {
    my ($pair) = @_;
    my $depth = 0;
    my $in_quote = 0;
    my $escaped = 0;

    for my $i (0 .. length($pair) - 1) {
        my $ch = substr($pair, $i, 1);

        if ($in_quote && $escaped) {
            $escaped = 0;
            next;
        }
        if ($in_quote && $ch eq '\\') {
            $escaped = 1;
            next;
        }
        if ($ch eq '"') {
            $in_quote = !$in_quote;
            next;
        }
        next if $in_quote;

        if ($ch eq '(' || $ch eq '[' || $ch eq '{') {
            $depth++;
            next;
        }
        if ($ch eq ')' || $ch eq ']' || $ch eq '}') {
            $depth--;
            next;
        }
        if ($ch eq ':' && $depth == 0) {
            my $left = substr($pair, 0, $i);
            my $right = substr($pair, $i + 1);
            return ($left, $right);
        }
    }

    return;
}

# Evaluate a single expression (recursive for function calls)
sub eval_expr {
    my ($expr) = @_;
    $expr =~ s/^\s+//; $expr =~ s/\s+$//;

    # String literal (with $var interpolation)
    if ($expr =~ /^"((?:\\.|[^"\\])*)"$/) {
        return parse_string_literal($1);
    }

    # Number
    if ($expr =~ /^-?\d+(\.\d+)?$/) {
        return $expr + 0;
    }

    # Variable
    if ($expr =~ /^\$($MODULE_NAME_RE)\/($SYMBOL_NAME_RE)$/) {
        my ($module, $name) = ($1, $2);
        if (is_global_name($name)) {
            require_declared_global($name, "expression", 'read');
            return $globals{$name};
        }
        die "Invalid local variable name '\$$name' (locals must be lowercase)\n"
            unless is_local_name($name);
        my (undef, $value) = local_var_lookup($module, $name);
        return $value;
    }
    if ($expr =~ /^\$($SYMBOL_NAME_RE)$/) {
        my $name = $1;
        if (is_global_name($name)) {
            require_declared_global($name, "expression", 'read');
            return $globals{$name};
        }
        die "Invalid local variable name '\$$name' (locals must be lowercase)\n"
            unless is_local_name($name);
        my ($found, $value) = local_var_lookup($current_module, $name);
        return $found ? $value : $globals{$name};
    }

    # Array variable
    if ($expr =~ /^\@($MODULE_NAME_RE)\/($SYMBOL_NAME_RE)$/) {
        my ($module, $name) = ($1, $2);
        if (is_global_name($name)) {
            require_declared_global($name, "expression", 'read');
            return $global_arrays{$name} // [];
        }
        die "Invalid local array name '\@$name' (locals must be lowercase)\n"
            unless is_local_name($name);
        return module_arrays_ref($module)->{$name} // [];
    }
    if ($expr =~ /^\@($SYMBOL_NAME_RE)$/) {
        my $name = $1;
        if (is_global_name($name)) {
            require_declared_global($name, "expression", 'read');
            return $global_arrays{$name} // [];
        }
        die "Invalid local array name '\@$name' (locals must be lowercase)\n"
            unless is_local_name($name);
        return module_arrays_ref($current_module)->{$name} // [];
    }

    # Dict variable
    if ($expr =~ /^\%($MODULE_NAME_RE)\/($SYMBOL_NAME_RE)$/) {
        my ($module, $name) = ($1, $2);
        if (is_global_name($name)) {
            require_declared_global($name, "expression", 'read');
            return $global_dicts{$name} // {};
        }
        die "Invalid local dict name '\%$name' (locals must be lowercase)\n"
            unless is_local_name($name);
        return module_dicts_ref($module)->{$name} // {};
    }
    if ($expr =~ /^\%($SYMBOL_NAME_RE)$/) {
        my $name = $1;
        if (is_global_name($name)) {
            require_declared_global($name, "expression", 'read');
            return $global_dicts{$name} // {};
        }
        die "Invalid local dict name '\%$name' (locals must be lowercase)\n"
            unless is_local_name($name);
        return module_dicts_ref($current_module)->{$name} // {};
    }

    # Vector literal: [a, b, c]
    if ($expr =~ /^\[(.*)?\]$/) {
        my $inner = $1 // '';
        my @items = parse_arglist($inner);
        return [map { eval_expr($_) } @items];
    }

    # Dict literal: {key: val, key2: val2}
    if ($expr =~ /^\{(.*)\}$/) {
        my $inner = $1;
        my %h;
        for my $pair (parse_arglist($inner)) {
            $pair =~ s/^\s+//; $pair =~ s/\s+$//;
            my ($kexpr, $vexpr) = split_dict_pair($pair);
            die "Bad dict entry: '$pair'\n" unless defined $kexpr && defined $vexpr;
            $kexpr =~ s/^\s+//; $kexpr =~ s/\s+$//;
            $vexpr =~ s/^\s+//; $vexpr =~ s/\s+$//;
            my $key;
            if ($kexpr =~ /^$SYMBOL_NAME_RE$/) {
                $key = $kexpr;
            } else {
                my $kval = eval_expr($kexpr);
                $key = defined $kval ? "$kval" : '';
            }
            $h{$key} = eval_expr($vexpr);
        }
        return \%h;
    }

    # Regex literal:  #"pattern"
    if ($expr =~ /^#"(.*)"$/) {
        return qr/$1/;
    }

    # Function call:  func-name( args... )
    if ($expr =~ /^($SYMBOL_NAME_RE(?:\/$SYMBOL_NAME_RE)?)\s*\((.*)?\)\s*$/) {
        my $fname = $1;
        my $raw_args = $2 // '';
        my @args = parse_arglist($raw_args);
        my @evaled = map { eval_expr($_) } @args;

        if ($fname eq 'print') {
            my $last = @evaled > 1 ? $evaled[-1] : 0;
            my $to_stderr = $last && $last eq '1' && @evaled > 1;
            my @parts = $to_stderr ? @evaled[0..$#evaled-1] : @evaled;
            my $out = join('', map { $_ // '' } @parts);
            if ($to_stderr) {
                print STDERR $out, "\n";
            } else {
                print $out, "\n";
            }
            return $out;
        }

        my ($target_module, $sub_name) = resolve_sub_target($fname);
        if (defined $target_module) {
            my $sub = module_subs_ref($target_module)->{$sub_name};
            my $frames = module_var_frames_ref($target_module);
            push @$frames, {};
            my $frame = $frames->[-1];
            local $call_depth = $call_depth + 1;
            local $returning = 0;
            local $current_module = $target_module;
            for my $idx (0 .. $#{$sub->{params}}) {
                $frame->{$sub->{params}[$idx]} = $evaled[$idx];
            }
            delete $frame->{'_return'};
            my $body = $sub->{body_nodes} // $sub->{body} // [];
            run_lines($body);
            my $ret = $frame->{'_return'};
            pop @$frames;
            return $ret;
        }

        die "Unknown function: $fname\n" unless exists $builtins{$fname};
        return $builtins{$fname}->(@evaled);
    }

    die "Cannot evaluate expression: '$expr'\n";
}

# Split a comma-separated argument list, respecting parentheses and quotes
sub parse_arglist {
    my ($str) = @_;
    my @args;
    my $depth = 0;
    my $in_quote = 0;
    my $escaped = 0;
    my $current = '';

    for my $ch (split //, $str) {
        if ($in_quote && $escaped) {
            $escaped = 0;
            $current .= $ch;
        } elsif ($in_quote && $ch eq '\\') {
            $escaped = 1;
            $current .= $ch;
        } elsif ($ch eq '"') {
            $in_quote = !$in_quote;
            $current .= $ch;
        } elsif ($in_quote) {
            $current .= $ch;
        } elsif ($ch eq '(' || $ch eq '[' || $ch eq '{') {
            $depth++;
            $current .= $ch;
        } elsif ($ch eq ')' || $ch eq ']' || $ch eq '}') {
            $depth--;
            $current .= $ch;
        } elsif ($ch eq ',' && $depth == 0) {
            push @args, $current;
            $current = '';
        } else {
            $current .= $ch;
        }
    }
    die "Unterminated string in argument list: '$str'\n" if $in_quote;
    die "Unbalanced brackets in argument list: '$str'\n" if $depth != 0;
    push @args, $current if $current =~ /\S/;
    return @args;
}

# ============================================================
#  Execution engine (processes a list of lines)
# ============================================================

sub compile_program {
    my ($lines_ref) = @_;
    my ($nodes, $next, $term) = compile_block($lines_ref, 0, 0);
    if (defined $term) {
        my $line = $next;
        die "else without matching if on line $line\n" if $term eq 'else';
        die "end without matching block on line $line\n";
    }
    return $nodes;
}

sub compile_block {
    my ($lines_ref, $start, $allow_else) = @_;
    my @nodes;
    my $i = $start;

    while ($i < scalar @$lines_ref) {
        my $line = $lines_ref->[$i];
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;

        if ($line eq '' || $line =~ /^#/) {
            $i++;
            next;
        }

        if ($line =~ /^else\b/) {
            if ($allow_else) {
                return (\@nodes, $i + 1, 'else');
            }
            die "else without matching if on line " . ($i + 1) . "\n";
        }
        if ($line =~ /^end\b/) {
            return (\@nodes, $i + 1, 'end');
        }

        if ($line =~ /^global\s+\$($SYMBOL_NAME_RE)(?:\s+(.+))?$/) {
            push @nodes, {
                kind => 'global_decl',
                line => $i + 1,
                name => $1,
                raw_mod => $2,
            };
            $i++;
            next;
        }

        if ($line =~ /^set\s+\$($SYMBOL_NAME_RE)\s*=\s*(.+)$/) {
            push @nodes, {
                kind => 'set_scalar',
                line => $i + 1,
                var => $1,
                expr => $2,
            };
            $i++;
            next;
        }

        if ($line =~ /^\$($SYMBOL_NAME_RE)\s*=\s*(.+)$/) {
            push @nodes, {
                kind => 'assign_global',
                line => $i + 1,
                var => $1,
                expr => $2,
            };
            $i++;
            next;
        }

        if ($line =~ /^set\s+\@($SYMBOL_NAME_RE)\s*=\s*(.+)$/) {
            push @nodes, {
                kind => 'set_array',
                line => $i + 1,
                var => $1,
                expr => $2,
            };
            $i++;
            next;
        }

        if ($line =~ /^set\s+\%($SYMBOL_NAME_RE)\s*=\s*(.+)$/) {
            push @nodes, {
                kind => 'set_dict',
                line => $i + 1,
                var => $1,
                expr => $2,
            };
            $i++;
            next;
        }

        if ($line =~ /^return\s*\((.*)\)\s*$/) {
            push @nodes, {
                kind => 'return',
                line => $i + 1,
                raw => $1 // '',
            };
            $i++;
            next;
        }

        if ($line =~ /^if\s+(.+)$/) {
            my $start_line = $i + 1;
            my $cond = $1;
            my ($true_nodes, $next_i, $term) = compile_block($lines_ref, $i + 1, 1);
            die "if without matching end on line $start_line\n" unless defined $term;
            my $false_nodes = [];
            if ($term eq 'else') {
                my ($f_nodes, $after_false, $term2) = compile_block($lines_ref, $next_i, 0);
                die "if without matching end on line $start_line\n" unless defined $term2 && $term2 eq 'end';
                $false_nodes = $f_nodes;
                $i = $after_false;
            } elsif ($term eq 'end') {
                $i = $next_i;
            } else {
                die "if without matching end on line $start_line\n";
            }
            push @nodes, {
                kind => 'if',
                line => $start_line,
                cond => $cond,
                true_body => $true_nodes,
                false_body => $false_nodes,
            };
            next;
        }

        if ($line =~ /^sub\s+($SYMBOL_NAME_RE)\s*\(([^)]*)\)$/) {
            my $start_line = $i + 1;
            my $name = $1;
            my $raw_params = $2 // '';
            my @params = grep { $_ ne '' } map {
                my $p = $_;
                $p =~ s/^\s*\$//;
                $p =~ s/^\s+//;
                $p =~ s/\s+$//;
                die "Invalid parameter name '\$$p' (locals must be lowercase)\n"
                    unless is_local_name($p);
                $p;
            } split /,/, $raw_params;

            my ($body_nodes, $next_i, $term) = compile_block($lines_ref, $i + 1, 0);
            die "sub without matching end on line $start_line\n" unless defined $term && $term eq 'end';
            push @nodes, {
                kind => 'sub_def',
                line => $start_line,
                name => $name,
                params => \@params,
                body => $body_nodes,
            };
            $i = $next_i;
            next;
        }

        if ($line =~ /^alias\s+($SYMBOL_NAME_RE)\s*=\s*($SYMBOL_NAME_RE(?:\/$SYMBOL_NAME_RE)?)$/) {
            push @nodes, {
                kind => 'alias',
                line => $i + 1,
                new => $1,
                old => $2,
            };
            $i++;
            next;
        }

        if ($line =~ /^foreach\s+\$($SYMBOL_NAME_RE)\s+\@((?:$MODULE_NAME_RE\/)?$SYMBOL_NAME_RE)$/) {
            my $start_line = $i + 1;
            my ($body_nodes, $next_i, $term) = compile_block($lines_ref, $i + 1, 0);
            die "foreach without matching end on line $start_line\n" unless defined $term && $term eq 'end';
            push @nodes, {
                kind => 'foreach',
                line => $start_line,
                var => $1,
                arrname => $2,
                body => $body_nodes,
            };
            $i = $next_i;
            next;
        }

        if ($line =~ /^$SYMBOL_NAME_RE(?:\/$SYMBOL_NAME_RE)?\s*\(/) {
            push @nodes, {
                kind => 'call',
                line => $i + 1,
                expr => $line,
            };
            $i++;
            next;
        }

        die "Syntax error on line " . ($i + 1) . ": $line\n";
    }

    return (\@nodes, $i, undef);
}

sub run_nodes {
    my ($nodes_ref) = @_;
    for my $node (@$nodes_ref) {
        exec_node($node);
        last if $returning;
    }
}

sub run_lines {
    my ($program_ref) = @_;
    if (@$program_ref && ref($program_ref->[0]) eq 'HASH') {
        run_nodes($program_ref);
        return;
    }
    my $nodes = compile_program($program_ref);
    run_nodes($nodes);
}

sub exec_node {
    my ($node) = @_;
    my $line_no = $node->{line};
    my $kind = $node->{kind};

    if ($kind eq 'global_decl') {
        my $where = "line $line_no";
        my $spec = parse_global_decl_modifier($node->{raw_mod});
        declare_global_spec($node->{name}, $spec, $where);
        if ($spec->{has_default} && !exists $globals{$node->{name}}) {
            $globals{$node->{name}} = eval_expr($spec->{default_expr});
        }
        return;
    }

    if ($kind eq 'set_scalar') {
        my ($var, $expr) = ($node->{var}, $node->{expr});
        if (is_global_name($var)) {
            require_declared_global($var, "line $line_no", 'assignment');
            $globals{$var} = eval_expr($expr);
        } elsif (is_local_name($var)) {
            local_var_set($current_module, $var, eval_expr($expr));
        } else {
            die "Invalid local variable name '\$$var' (locals must be lowercase)\n";
        }
        return;
    }

    if ($kind eq 'assign_global') {
        my ($var, $expr) = ($node->{var}, $node->{expr});
        die "Global names must be uppercase: '\$$var'\n" unless is_global_name($var);
        require_declared_global($var, "line $line_no", 'assignment');
        $globals{$var} = eval_expr($expr);
        return;
    }

    if ($kind eq 'set_array') {
        my ($var, $expr) = ($node->{var}, $node->{expr});
        if (is_global_name($var)) {
            require_declared_global($var, "line $line_no", 'assignment');
            $global_arrays{$var} = eval_expr($expr);
        } elsif (is_local_name($var)) {
            module_arrays_ref($current_module)->{$var} = eval_expr($expr);
        } else {
            die "Invalid local array name '\@$var' (locals must be lowercase)\n";
        }
        return;
    }

    if ($kind eq 'set_dict') {
        my ($var, $expr) = ($node->{var}, $node->{expr});
        if (is_global_name($var)) {
            require_declared_global($var, "line $line_no", 'assignment');
            $global_dicts{$var} = eval_expr($expr);
        } elsif (is_local_name($var)) {
            module_dicts_ref($current_module)->{$var} = eval_expr($expr);
        } else {
            die "Invalid local dict name '\%$var' (locals must be lowercase)\n";
        }
        return;
    }

    if ($kind eq 'return') {
        die "return outside sub on line $line_no\n" if $call_depth <= 0;
        my $raw = $node->{raw} // '';
        if ($raw =~ /\S/) {
            local_var_set($current_module, '_return', eval_expr($raw));
        } else {
            local_var_set($current_module, '_return', undef);
        }
        $returning = 1;
        return;
    }

    if ($kind eq 'if') {
        my $cond = eval_expr($node->{cond});
        my $truthy = $cond && $cond ne '0' && $cond ne '';
        if ($truthy) {
            run_lines($node->{true_body});
        } else {
            run_lines($node->{false_body});
        }
        return;
    }

    if ($kind eq 'sub_def') {
        module_subs_ref($current_module)->{$node->{name}} = {
            params => $node->{params},
            body => $node->{body},
        };
        return;
    }

    if ($kind eq 'alias') {
        my ($new, $old) = ($node->{new}, $node->{old});
        if (exists $builtins{$old}) {
            $builtins{$new} = $builtins{$old};
        } else {
            my ($target_module, $sub_name) = resolve_sub_target($old);
            if (defined $target_module) {
                module_subs_ref($current_module)->{$new} = module_subs_ref($target_module)->{$sub_name};
            } else {
                die "alias: unknown function '$old'\n";
            }
        }
        return;
    }

    if ($kind eq 'foreach') {
        my ($var, $arrname) = ($node->{var}, $node->{arrname});
        if (!is_global_name($var) && !is_local_name($var)) {
            die "Invalid local variable name '\$$var' (locals must be lowercase)\n";
        }
        my $arr;
        if ($arrname =~ /^($MODULE_NAME_RE)\/($SYMBOL_NAME_RE)$/) {
            my ($module, $name) = ($1, $2);
            if (is_global_name($name)) {
                $arr = $global_arrays{$name} // [];
            } else {
                die "Invalid local array name '\@$name' (locals must be lowercase)\n"
                    unless is_local_name($name);
                $arr = module_arrays_ref($module)->{$name} // [];
            }
        } else {
            if (is_global_name($arrname)) {
                $arr = $global_arrays{$arrname} // [];
            } else {
                die "Invalid local array name '\@$arrname' (locals must be lowercase)\n"
                    unless is_local_name($arrname);
                $arr = module_arrays_ref($current_module)->{$arrname} // [];
            }
        }

        for my $elem (@$arr) {
            if (is_global_name($var)) {
                require_declared_global($var, "line $line_no", 'assignment');
                $globals{$var} = $elem;
            } else {
                local_var_set($current_module, $var, $elem);
            }
            run_lines($node->{body});
            last if $returning;
        }
        return;
    }

    if ($kind eq 'call') {
        eval_expr($node->{expr});
        return;
    }

    die "Unknown node kind '$kind'\n";
}

# ============================================================
#  Main – read from file or STDIN
# ============================================================

my @program;
my $check_mode = 0;
while (@ARGV && $ARGV[0] =~ /^--/) {
    if ($ARGV[0] eq '--check') {
        $check_mode = 1;
        shift @ARGV;
        next;
    }
    if ($ARGV[0] eq '--strict-globals') {
        $strict_globals_mode = 1;
        shift @ARGV;
        next;
    }
    die "Unknown option: $ARGV[0]\n";
}

if ($check_mode) {
    die "Usage: simp.pl --check <file>\n" unless @ARGV;
    my $ok = run_static_check($ARGV[0]);
    exit($ok ? 0 : 1);
}

if ($strict_globals_mode && @ARGV) {
    my $ok = run_static_check($ARGV[0]);
    exit 1 unless $ok;
}

if (@ARGV) {
    open my $fh, '<', $ARGV[0] or die "Cannot open $ARGV[0]: $!\n";
    @program = <$fh>;
    close $fh;
} else {
    @program = <STDIN>;
}

chomp @program;

# Populate script args: $PATH, $ARG1..$ARGN, @ARGS
if (@ARGV) {
    require File::Basename;
    $module_dirs{$MAIN_MODULE} = File::Basename::dirname($ARGV[0]);

    predeclare_global_if_missing('PATH');
    $globals{'PATH'} = $ARGV[0];
    for my $i (1 .. $#ARGV) {
        predeclare_global_if_missing("ARG$i");
        $globals{"ARG$i"} = $ARGV[$i];
    }
    predeclare_global_if_missing('ARGS');
    $global_arrays{'ARGS'} = [@ARGV[1 .. $#ARGV]];
} else {
    predeclare_global_if_missing('ARGS');
    $global_arrays{'ARGS'} = [];
}

predeclare_global_if_missing('ENV');
$global_dicts{'ENV'} = { %ENV };

my $program_nodes = compile_program(\@program);
run_lines($program_nodes);
validate_required_globals_runtime() if $strict_globals_mode;
