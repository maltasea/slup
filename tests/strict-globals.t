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

sub run_strict {
    my ($path) = @_;
    my $cmd = qq{perl "$simp" --strict-globals "$path" 2>&1};
    my $out = `$cmd`;
    my $status = $? >> 8;
    return ($status, $out);
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
global $DB_HOST required
$DB_HOST = "localhost"
print($DB_HOST)
SIMP
    my ($status, $out) = run_strict(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'strict mode allows declared global assignment');
    is($out, "localhost\n", 'declared global reads successfully in strict mode');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
$UNDECLARED = "x"
SIMP
    my ($status, $out) = run_strict(File::Spec->catfile($dir, 'main.simp'));
    ok($status != 0, 'strict mode rejects undeclared global assignment');
    like($out, qr/undeclared global assignment/, 'strict assignment error is clear');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
global $DECLARED default("ok")
print($MISSING)
SIMP
    my ($status, $out) = run_strict(File::Spec->catfile($dir, 'main.simp'));
    ok($status != 0, 'strict mode rejects undeclared global read');
    like($out, qr/undeclared global read/, 'strict read error is clear');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
global $DB_PORT default("5432")
print($DB_PORT)
SIMP
    my ($status, $out) = run_strict(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'strict mode applies default for declared global');
    is($out, "5432\n", 'declared default is available at runtime');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
print(len(@ARGS))
print(gt(len(dict-keys(%ENV)), 0))
SIMP
    my ($status, $out) = run_strict(File::Spec->catfile($dir, 'main.simp'));
    is($status, 0, 'strict mode predeclares @ARGS and %ENV');
    is($out, "0\n1\n", 'built-in globals remain usable in strict mode');
}

{
    my $dir = tempdir(CLEANUP => 1);
    write_text(File::Spec->catfile($dir, 'main.simp'), <<'SIMP');
global $DB_HOST required
set $path = "mod"
load($path)
SIMP
    my ($status, $out) = run_strict(File::Spec->catfile($dir, 'main.simp'));
    ok($status != 0, 'strict mode uses startup static precheck');
    like($out, qr/static check requires load\("literal"\)/, 'strict mode reports non-static load in precheck');
}

done_testing();
