#!/usr/bin/env perl
use strict;
use warnings;
use Test::More tests => 44;
use File::Path qw(make_path);
use File::Temp qw(tempfile tempdir);
use FindBin qw($Bin);
use File::Spec;

my $root = File::Spec->catdir($Bin, '..');
my $demo = File::Spec->catfile($root, 'demo.simp');

my $build_cmd = qq{cd "$root" && eval \$(opam env) && dune build ./simp.exe 2>&1};
my $build_out = `$build_cmd`;
my $build_status = $? >> 8;
if ($build_status != 0) {
    BAIL_OUT("dune build failed:\n$build_out");
}

my $simp = File::Spec->catfile($root, '_build', 'default', 'simp.exe');

sub run_simp {
    my ($program) = @_;
    my ($fh, $path) = tempfile(SUFFIX => '.simp', UNLINK => 1);
    print {$fh} $program;
    close $fh;

    my $cmd = qq{"$simp" "$path" < /dev/null 2>&1};
    my $out = `$cmd`;
    my $status = $? >> 8;
    return ($status, $out);
}

sub run_file {
    my ($path) = @_;
    my $cmd = qq{"$simp" "$path" < /dev/null 2>&1};
    my $out = `$cmd`;
    my $status = $? >> 8;
    return ($status, $out);
}

sub write_text {
    my ($path, $content) = @_;
    my ($vol, $dir, undef) = File::Spec->splitpath($path);
    make_path($dir) if defined $dir && $dir ne '' && !-d $dir;
    open my $fh, '>', $path or die "cannot write $path: $!";
    print {$fh} $content;
    close $fh;
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set $sum = add(2, 3)
print($sum)
SIMP
    is($status, 0, 'basic arithmetic exits successfully (ocaml)');
    is($out, "5\n", 'basic arithmetic output (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
sub double($n)
  return(mul($n, 2))
  print("dead code")
end
print(double(4))
SIMP
    is($status, 0, 'return(...) in sub exits successfully (ocaml)');
    is($out, "8\n", 'return(...) exits sub body early (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
sub find-blue()
  set @colors = ["red", "blue", "green"]
  foreach $c @colors
    if eq($c, "blue")
      return($c)
    end
  end
  return("none")
end
print(find-blue())
SIMP
    is($status, 0, 'return(...) propagates through nested blocks (ocaml)');
    is($out, "blue\n", 'nested return value is preserved (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
return("x")
SIMP
    ok($status != 0, 'return(...) outside sub fails (ocaml)');
    like($out, qr/return outside sub/, 'outside-sub return has clear error (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set $who = "Ben"
print("line1\nline2")
print("quote: \" slash: \\ literal: \$who interp: $who")
SIMP
    my $expected = "line1\nline2\nquote: \" slash: \\ literal: \$who interp: Ben\n";
    is($status, 0, 'escaped strings execute (ocaml)');
    is($out, $expected, 'string escapes and interpolation work (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set %d = {"api-key": "x", "space key": "y", plain: "z"}
print(dict-get(%d, "api-key"))
print(dict-get(%d, "space key"))
print(dict-get(%d, "plain"))
SIMP
    is($status, 0, 'quoted dict keys execute (ocaml)');
    is($out, "x\ny\nz\n", 'quoted and bare dict keys can coexist (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set $k = "dyn"
set %d = {$k: 7}
print(dict-get(%d, "dyn"))
SIMP
    is($status, 0, 'dict key expression executes (ocaml)');
    is($out, "7\n", 'dict key expression resolves (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
print(concat("a\",b", "X"))
SIMP
    is($status, 0, 'escaped quote in arg list executes (ocaml)');
    is($out, "a\",bX\n", 'escaped quote keeps comma inside argument (ocaml)');
}

{
    my $count = 2000;
    my $ones = join(',', (1) x $count);
    my $program = <<"SIMP";
set \@seed = [$ones]
set \@xs = []
foreach \$n \@seed
  push(\@xs, \$n)
end
print(len(\@xs))
print(get(\@xs, 1999))
print(pop(\@xs))
print(len(\@xs))
SIMP
    my ($status, $out) = run_simp($program);
    is($status, 0, 'large push loop executes (ocaml)');
    is($out, "2000\n1\n1\n1999\n", 'push/get/pop/len stay consistent under load (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
if eq("a", "a")
  print("x")
SIMP
    ok($status != 0, 'missing end in if fails (ocaml)');
    like($out, qr/if without matching end/, 'missing if end has clear error (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
sub bad($x)
  print($x)
SIMP
    ok($status != 0, 'missing end in sub fails (ocaml)');
    like($out, qr/sub without matching end/, 'missing sub end has clear error (ocaml)');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set @x = [1, 2]
foreach $v @x
  print($v)
SIMP
    ok($status != 0, 'missing end in foreach fails (ocaml)');
    like($out, qr/foreach without matching end/, 'missing foreach end has clear error (ocaml)');
}

{
    my ($status, $out) = run_file($demo);
    is($status, 0, 'demo.simp executes (ocaml)');
    like($out, qr/-- demo complete --\n/, 'demo.simp reaches completion (ocaml)');
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
    is($status, 0, 'qualified and unqualified function calls execute (ocaml)');
    is($out, "main:A\nalpha:B\n", 'main stays unqualified, module requires qualification (ocaml)');
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
    ok($status != 0, 'module-only function is not available unqualified (ocaml)');
    like($out, qr/Unknown function: only_alpha/, 'unqualified module-only function fails clearly (ocaml)');
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
    is($status, 0, 'global variables are shared across modules (ocaml)');
    is($out, "from-module\nfrom-module\n", 'global value is shared and visible in both main and module (ocaml)');
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
    is($status, 0, 'qualified var/array/dict access executes (ocaml)');
    is($out, "main-local\nmodule-local\n3\nv\n", 'module symbols stay scoped and are reachable through module/name (ocaml)');
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
    is($status, 0, 'relative load from inside a module executes (ocaml)');
    is($out, "pong\n", 'module load resolves relative paths from caller module directory (ocaml)');
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
    is($status, 0, 'module can resolve main symbols unqualified (ocaml)');
    is($out, "main:z\n", 'main module remains an unqualified fallback namespace (ocaml)');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set @ARGS = ["a", "b"]
print(len(@ARGS))
print(gt(len(dict-keys(%ENV)), 0))
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'uppercase @ARGS and %ENV are globals (ocaml)');
    is($out, "2\n1\n", 'global arrays/dicts are accessible without namespacing (ocaml)');
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
    is($status, 0, 'dash, arrow, and question-mark symbols execute (ocaml)');
    is($out, "ben\n200\nyes\nOK\n", 'symbol names support -, ->, and ? (ocaml)');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
set $Bad = "x"
SIMP
    my ($status, $out) = run_file(File::Spec->catfile($dir, 'main.simp'));
    ok($status != 0, 'mixed-case local variable names are rejected (ocaml)');
    like($out, qr/locals must be lowercase/, 'mixed-case local rejection is clear (ocaml)');
}
