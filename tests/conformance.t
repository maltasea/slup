#!/usr/bin/env perl
use strict;
use warnings;
use Test::More;
use File::Path qw(make_path);
use File::Temp qw(tempdir);
use FindBin qw($Bin);
use File::Spec;

my $simp = File::Spec->catfile($Bin, '..', 'simp.pl');

sub write_text {
    my ($path, $content) = @_;
    my ($vol, $dir, undef) = File::Spec->splitpath($path);
    make_path($dir) if defined $dir && $dir ne '' && !-d $dir;
    open my $fh, '>', $path or die "cannot write $path: $!";
    print {$fh} $content;
    close $fh;
}

sub run_file {
    my ($path) = @_;
    my $cmd = qq{perl "$simp" "$path" 2>&1};
    my $out = `$cmd`;
    my $status = $? >> 8;
    return ($status, $out);
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'alpha.simp'), <<'SIMP');
set $name = "alpha"
sub who($x)
  return(concat(concat($name, ":"), $x))
end
SIMP
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set $name = "main"
sub who($x)
  return(concat(concat($name, ":"), $x))
end
load("alpha")
print(who("A"))
print(alpha/who("B"))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'qualified and unqualified function calls execute');
    is($out, "main:A\nalpha:B\n", 'main stays unqualified, module requires qualification');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'alpha.simp'), <<'SIMP');
sub only_alpha()
  return("x")
end
SIMP
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
load("alpha")
print(only_alpha())
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    ok($status != 0, 'module-only function is not available unqualified');
    like($out, qr/Unknown function: only_alpha/, 'unqualified module-only function fails clearly');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'alpha.simp'), <<'SIMP');
$G = "from-module"
sub getg()
  return($G)
end
SIMP
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
$G = "from-main"
load("alpha")
print($G)
print(alpha/getg())
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'global variables are shared across modules');
    is($out, "from-module\nfrom-module\n", 'global value is shared and visible in both main and module');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'alpha.simp'), <<'SIMP');
set $v = "module-local"
set @items = ["a", "b", "c"]
set %cfg = {k: "v"}
SIMP
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set $v = "main-local"
load("alpha")
print($v)
print($alpha/v)
print(len(@alpha/items))
print(dict-get(%alpha/cfg, "k"))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'qualified var/array/dict access executes');
    is($out, "main-local\nmodule-local\n3\nv\n", 'module symbols stay scoped and are reachable through module/name');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'mods', 'helper.simp'), <<'SIMP');
sub ping()
  return("pong")
end
SIMP
    write_text(File::Spec->catfile($dir, 'mods', 'alpha.simp'), <<'SIMP');
load("helper")
SIMP
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
load("mods/alpha")
print(helper/ping())
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'relative load from inside a module executes');
    is($out, "pong\n", 'module load resolves relative paths from the caller module directory');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'alpha.simp'), <<'SIMP');
sub call_main()
  return(who("z"))
end
SIMP
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
sub who($x)
  return(concat("main:", $x))
end
load("alpha")
print(alpha/call_main())
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'module can resolve main symbols unqualified');
    is($out, "main:z\n", 'main module remains an unqualified fallback namespace');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set @ARGS = ["a", "b"]
print(len(@ARGS))
print(gt(len(dict-keys(%ENV)), 0))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'uppercase @ARGS and %ENV are globals');
    is($out, "2\n1\n", 'global arrays/dicts are accessible without namespacing');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set %r = run(["/bin/sh", "-c", "printf out; printf err 1>&2"])
print(dict-get(%r, "code"))
print(dict-get(%r, "out"))
print(dict-get(%r, "err"))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'run() captures command output');
    is($out, "0\nout\nerr\n", 'run() returns dict with code/out/err');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set %r = run(["/bin/sh", "-c", "printf bad 1>&2; exit 7"])
print(dict-get(%r, "code"))
print(dict-get(%r, "out"))
print(dict-get(%r, "err"))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'run() does not crash on non-zero exit');
    is($out, "7\n\nbad\n", 'run() preserves non-zero status and stderr');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set %r = pipe([["/bin/sh", "-c", "printf hi"], ["tr", "a-z", "A-Z"]])
print(dict-get(%r, "code"))
print(dict-get(%r, "out"))
print(dict-get(%r, "err"))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'pipe() executes command pipeline');
    is($out, "0\nHI\n\n", 'pipe() returns last stdout and combined stderr');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set %r = pipe([["/bin/sh", "-c", "printf x"], ["/bin/sh", "-c", "cat >/dev/null; printf boom 1>&2; exit 9"]])
print(dict-get(%r, "code"))
print(dict-get(%r, "out"))
print(dict-get(%r, "err"))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'pipe() returns control for non-zero pipeline exits');
    is($out, "9\n\nboom\n", 'pipe() reports last command status and stderr');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set $user-name = "ben"
set $http->status = "200"
set $ready? = "yes"
$MY-GLOBAL? = "OK"
print($user-name)
print($http->status)
print($ready?)
print($MY-GLOBAL?)
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'dash, arrow, and question-mark symbols execute');
    is($out, "ben\n200\nyes\nOK\n", 'symbol names support -, ->, and ?');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set $Bad = "x"
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    ok($status != 0, 'mixed-case local variable names are rejected');
    like($out, qr/locals must be lowercase/, 'mixed-case local rejection is clear');
}

done_testing();
