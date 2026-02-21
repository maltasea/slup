#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use JSON::PP qw(decode_json);
use File::Spec;

my $dir = 'bench/history';

GetOptions(
    'dir=s' => \$dir,
) or die "Usage: perl bench/compare-baseline.pl [--dir bench/history]\n";

my @files = sort glob(File::Spec->catfile($dir, '*.json'));
if (@files < 2) {
    print "not enough JSON snapshots in $dir (need at least 2)\n";
    exit 0;
}

my $prev = read_json($files[-2]);
my $curr = read_json($files[-1]);

my @metrics = (
    { key => 'recursion_over_loop',   label => 'recursion / loop' },
    { key => 'module_over_local',     label => 'module/sub / local-sub' },
    { key => 'strict_over_nonstrict', label => 'strict / no-strict' },
);

my $prev_label = $prev->{timestamp_utc} // $files[-2];
my $curr_label = $curr->{timestamp_utc} // $files[-1];
print "baseline compare: $prev_label -> $curr_label\n";
printf "%-26s %10s %10s %10s %9s\n", 'metric', 'prev', 'curr', 'delta', 'trend';

for my $m (@metrics) {
    my $a = $prev->{ratios}{ $m->{key} };
    my $b = $curr->{ratios}{ $m->{key} };
    if (!defined $a || !defined $b) {
        printf "%-26s %10s %10s %10s %9s\n", $m->{label}, '-', '-', '-', 'n/a';
        next;
    }
    my $pct = $a == 0 ? 0 : (($b - $a) / $a) * 100;
    my $trend = abs($pct) < 1 ? 'flat' : ($pct < 0 ? 'better' : 'worse');
    printf "%-26s %10.3fx %10.3fx %9.2f%% %9s\n", $m->{label}, $a, $b, $pct, $trend;
}

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
