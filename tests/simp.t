#!/usr/bin/env perl
use strict;
use warnings;
use Test::More tests => 32;
use File::Temp qw(tempfile tempdir);
use FindBin qw($Bin);
use File::Spec;

my $simp = File::Spec->catfile($Bin, '..', 'simp.pl');
my $demo = File::Spec->catfile($Bin, '..', 'demo.simp');

sub run_simp {
    my ($program) = @_;
    my ($fh, $path) = tempfile(SUFFIX => '.simp', UNLINK => 1);
    print {$fh} $program;
    close $fh;

    my $cmd = qq{perl "$simp" "$path" 2>&1};
    my $out = `$cmd`;
    my $status = $? >> 8;
    return ($status, $out);
}

sub run_file {
    my ($path) = @_;
    my $cmd = qq{perl "$simp" "$path" 2>&1};
    my $out = `$cmd`;
    my $status = $? >> 8;
    return ($status, $out);
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set $sum = add(2, 3)
print($sum)
SIMP
    is($status, 0, 'basic arithmetic exits successfully');
    is($out, "5\n", 'basic arithmetic output');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
sub double($n)
  return(mul($n, 2))
  print("dead code")
end
print(double(4))
SIMP
    is($status, 0, 'return(...) in sub exits successfully');
    is($out, "8\n", 'return(...) exits sub body early');
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
    is($status, 0, 'return(...) propagates through nested blocks');
    is($out, "blue\n", 'nested return value is preserved');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
return("x")
SIMP
    ok($status != 0, 'return(...) outside sub fails');
    like($out, qr/return outside sub/, 'outside-sub return has clear error');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set $who = "Ben"
print("line1\nline2")
print("quote: \" slash: \\ literal: \$who interp: $who")
SIMP
    my $expected = "line1\nline2\nquote: \" slash: \\ literal: \$who interp: Ben\n";
    is($status, 0, 'escaped strings execute');
    is($out, $expected, 'string escapes and interpolation work');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set %d = {"api-key": "x", "space key": "y", plain: "z"}
print(dict-get(%d, "api-key"))
print(dict-get(%d, "space key"))
print(dict-get(%d, "plain"))
SIMP
    is($status, 0, 'quoted dict keys execute');
    is($out, "x\ny\nz\n", 'quoted and bare dict keys can coexist');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set $k = "dyn"
set %d = {$k: 7}
print(dict-get(%d, "dyn"))
SIMP
    is($status, 0, 'dict key expression executes');
    is($out, "7\n", 'dict key expression resolves');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
print(concat("a\",b", "X"))
SIMP
    is($status, 0, 'escaped quote in arg list executes');
    is($out, "a\",bX\n", 'escaped quote keeps comma inside argument');
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
    is($status, 0, 'large push loop executes');
    is($out, "2000\n1\n1\n1999\n", 'push/get/pop/len stay consistent under load');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
$MYGLOBAL = "E3E"
sub show()
  print($MYGLOBAL)
  $MYGLOBAL = "UPDATED"
end
show()
print($MYGLOBAL)
SIMP
    is($status, 0, 'global assignment syntax executes');
    is($out, "E3E\nUPDATED\n", 'global vars are visible and mutable across subs');
}

{
    my $dir = tempdir(CLEANUP => 1);
    my $mod = File::Spec->catfile($dir, 'moda.simp');
    my $main = File::Spec->catfile($dir, 'main.simp');

    open my $mfh, '>', $mod or die "cannot write $mod: $!";
    print {$mfh} <<'SIMP';
set $m = "mod"
sub who($x)
  return(concat(concat($m, ":"), $x))
end
$SHARED = "module"
SIMP
    close $mfh;

    open my $fh, '>', $main or die "cannot write $main: $!";
    print {$fh} <<'SIMP';
set $m = "main"
sub who($x)
  return(concat(concat($m, ":"), $x))
end
$SHARED = "main"
load("moda")
print(who("A"))
print(moda/who("B"))
print($SHARED)
print($moda/m)
SIMP
    close $fh;

    my ($status, $out) = run_file($main);
    is($status, 0, 'module scoping and qualified symbol lookup execute');
    is($out, "main:A\nmod:B\nmodule\nmod\n", 'main symbols stay unqualified, module symbols are qualified, globals are shared');
}

{
    my $dir = tempdir(CLEANUP => 1);
    my $mod = File::Spec->catfile($dir, 'modb.simp');
    my $main = File::Spec->catfile($dir, 'main.simp');

    open my $mfh, '>', $mod or die "cannot write $mod: $!";
    print {$mfh} <<'SIMP';
sub only_mod()
  return("x")
end
SIMP
    close $mfh;

    open my $fh, '>', $main or die "cannot write $main: $!";
    print {$fh} <<'SIMP';
load("modb")
print(only_mod())
SIMP
    close $fh;

    my ($status, $out) = run_file($main);
    ok($status != 0, 'module-local functions are not visible unqualified');
    like($out, qr/Unknown function: only_mod/, 'unqualified lookup fails for module-local function');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
if eq("a", "a")
  print("x")
SIMP
    ok($status != 0, 'missing end in if fails');
    like($out, qr/if without matching end/, 'missing if end has clear error');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
sub bad($x)
  print($x)
SIMP
    ok($status != 0, 'missing end in sub fails');
    like($out, qr/sub without matching end/, 'missing sub end has clear error');
}

{
    my ($status, $out) = run_simp(<<'SIMP');
set @x = [1, 2]
foreach $v @x
  print($v)
SIMP
    ok($status != 0, 'missing end in foreach fails');
    like($out, qr/foreach without matching end/, 'missing foreach end has clear error');
}

{
    my ($status, $out) = run_file($demo);
    is($status, 0, 'demo.simp executes');
    like($out, qr/-- demo complete --\n/, 'demo.simp reaches completion');
}
