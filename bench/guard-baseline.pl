#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use JSON::PP qw(decode_json);
use File::Spec;

my $dir = 'bench/history';
my $max_rec = 10.0;
my $max_mod = 10.0;
my $max_strict = 10.0;

GetOptions(
    'dir=s'        => \$dir,
    'max-rec=f'    => \$max_rec,
    'max-mod=f'    => \$max_mod,
    'max-strict=f' => \$max_strict,
) or die "Usage: perl bench/guard-baseline.pl [--dir bench/history] [--max-rec 10] [--max-mod 10] [--max-strict 10]\n";

my @files = sort glob(File::Spec->catfile($dir, '*.json'));
if (@files < 2) {
    print "not enough JSON snapshots in $dir (need at least 2)\n";
    exit 0;
}

my $prev = read_json($files[-2]);
my $curr = read_json($files[-1]);

my @checks = (
    { key => 'recursion_over_loop',   label => 'recursion / loop', max_pct => $max_rec },
    { key => 'module_over_local',     label => 'module/sub / local-sub', max_pct => $max_mod },
    { key => 'strict_over_nonstrict', label => 'strict / no-strict', max_pct => $max_strict },
);

my $prev_label = $prev->{timestamp_utc} // $files[-2];
my $curr_label = $curr->{timestamp_utc} // $files[-1];
print "baseline guard: $prev_label -> $curr_label\n";
printf "%-26s %10s %10s %10s %10s %8s\n", 'metric', 'prev', 'curr', 'delta', 'limit', 'status';

my @failed;
for my $c (@checks) {
    my $a = $prev->{ratios}{ $c->{key} };
    my $b = $curr->{ratios}{ $c->{key} };
    if (!defined $a || !defined $b) {
        printf "%-26s %10s %10s %10s %10s %8s\n", $c->{label}, '-', '-', '-', '-', 'skip';
        next;
    }
    my $pct = $a == 0 ? 0 : (($b - $a) / $a) * 100;
    my $status = $pct > $c->{max_pct} ? 'FAIL' : 'ok';
    printf "%-26s %10.3fx %10.3fx %9.2f%% %9.2f%% %8s\n",
      $c->{label}, $a, $b, $pct, $c->{max_pct}, $status;
    push @failed, [$c->{label}, $pct, $c->{max_pct}] if $status eq 'FAIL';
}

if (@failed) {
    print "\nregression guard failed:\n";
    for my $f (@failed) {
        printf "  %s regressed by %.2f%% (limit %.2f%%)\n", $f->[0], $f->[1], $f->[2];
    }
    exit 1;
}

print "\nregression guard passed\n";
exit 0;

sub read_json {
    my ($path) = @_;
    open my $fh, '<', $path or die "cannot read $path: $!\n";
    local $/;
    my $raw = <$fh>;
    close $fh;
    my $data = decode_json($raw);
    die "invalid JSON snapshot in $path\n" unless ref($data) eq 'HASH';
    return $data;
}
