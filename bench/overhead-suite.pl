#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use Time::HiRes qw(time);
use POSIX qw(strftime);
use File::Spec;
use File::Temp qw(tempdir);
use JSON::PP ();

my $calls = 20_000;
my $depth = 400;
my $iters = 5;
my $warmup = 1;
my $json_out;

GetOptions(
    'calls=i'  => \$calls,
    'depth=i'  => \$depth,
    'iters=i'  => \$iters,
    'warmup=i' => \$warmup,
    'json-out=s' => \$json_out,
) or die "Usage: perl bench/overhead-suite.pl [--calls N] [--depth N] [--iters N] [--warmup N] [--json-out FILE]\n";

die "--calls must be > 0\n" unless $calls > 0;
die "--depth must be > 0\n" unless $depth > 0;
die "--iters must be > 0\n" unless $iters > 0;
die "--warmup must be >= 0\n" unless $warmup >= 0;

my $root = File::Spec->rel2abs(File::Spec->catdir(File::Spec->curdir()));
my $simp = File::Spec->catfile($root, 'simp.pl');
die "Cannot find simp.pl at $simp\n" unless -f $simp;

my $tmp = tempdir(CLEANUP => 1);

my $ones_calls = join(',', (1) x $calls);
my $ones_depth = join(',', (1) x $depth);

# ----------------------------------------------------------------------
# Recursion vs loop
# ----------------------------------------------------------------------
my $rec_iter = File::Spec->catfile($tmp, 'rec-iter.simp');
my $rec_fn = File::Spec->catfile($tmp, 'rec-fn.simp');

write_program(
    $rec_iter,
    <<"SIMP"
set \@nums = [$ones_depth]
set \$sum = 0
foreach \$n \@nums
  set \$sum = add(\$sum, \$n)
end
print(\$sum)
SIMP
);

write_program(
    $rec_fn,
    <<"SIMP"
sub rec(\$n)
  if lt(\$n, 1)
    return(0)
  end
  return(add(1, rec(sub(\$n, 1))))
end
print(rec($depth))
SIMP
);

my $rec_base = bench_case('rec-baseline(loop)', $simp, [], $rec_iter, $warmup, $iters);
my $rec_with = bench_case('recursion(calls)', $simp, [], $rec_fn, $warmup, $iters);

# ----------------------------------------------------------------------
# Local sub call vs module-qualified call
# ----------------------------------------------------------------------
my $mod_file = File::Spec->catfile($tmp, 'benchmod.simp');
my $mod_local = File::Spec->catfile($tmp, 'mod-local.simp');
my $mod_qual = File::Spec->catfile($tmp, 'mod-qualified.simp');

write_program(
    $mod_file,
    <<"SIMP"
sub inc(\$x)
  return(add(\$x, 1))
end
SIMP
);

write_program(
    $mod_local,
    <<"SIMP"
sub inc(\$x)
  return(add(\$x, 1))
end
set \@nums = [$ones_calls]
set \$sum = 0
foreach \$n \@nums
  set \$sum = add(\$sum, inc(\$n))
end
print(\$sum)
SIMP
);

write_program(
    $mod_qual,
    <<"SIMP"
load("benchmod")
set \@nums = [$ones_calls]
set \$sum = 0
foreach \$n \@nums
  set \$sum = add(\$sum, benchmod/inc(\$n))
end
print(\$sum)
SIMP
);

my $mod_base = bench_case('local-sub-calls', $simp, [], $mod_local, $warmup, $iters);
my $mod_with = bench_case('module/sub-calls', $simp, [], $mod_qual, $warmup, $iters);

# ----------------------------------------------------------------------
# strict-globals overhead
# ----------------------------------------------------------------------
my $strict_prog = File::Spec->catfile($tmp, 'strict-globals.simp');

write_program(
    $strict_prog,
    <<"SIMP"
global \$SUM default("0")
set \@nums = [$ones_calls]
foreach \$n \@nums
  set \$SUM = add(\$SUM, \$n)
end
print(\$SUM)
SIMP
);

my $strict_base = bench_case('globals(no-strict)', $simp, [], $strict_prog, $warmup, $iters);
my $strict_with = bench_case('globals(strict)', $simp, ['--strict-globals'], $strict_prog, $warmup, $iters);

print "\nrecursion benchmark (depth=$depth)\n";
print_table($rec_base, $rec_with, $depth);
printf "overhead ratio (recursion / loop): %.3fx\n", $rec_with->{avg_s} / $rec_base->{avg_s};

print "\nmodule-qualified benchmark (calls=$calls)\n";
print_table($mod_base, $mod_with, $calls);
printf "overhead ratio (module/sub / local-sub): %.3fx\n", $mod_with->{avg_s} / $mod_base->{avg_s};

print "\nstrict-globals benchmark (calls=$calls)\n";
print_table($strict_base, $strict_with, $calls);
printf "overhead ratio (strict / no-strict): %.3fx\n", $strict_with->{avg_s} / $strict_base->{avg_s};

if (defined $json_out) {
    write_json(
        $json_out,
        build_result(
            $calls, $depth, $iters, $warmup,
            $rec_base, $rec_with,
            $mod_base, $mod_with,
            $strict_base, $strict_with,
        ),
    );
}

sub write_program {
    my ($path, $content) = @_;
    open my $fh, '>', $path or die "cannot write $path: $!\n";
    print {$fh} $content;
    close $fh;
}

sub bench_case {
    my ($name, $simp_path, $flags, $path, $warmup_runs, $runs) = @_;
    for (1 .. $warmup_runs) {
        run_program($simp_path, $flags, $path);
    }
    my $sum = 0;
    for (1 .. $runs) {
        my $t0 = time();
        run_program($simp_path, $flags, $path);
        $sum += (time() - $t0);
    }
    return {
        name => $name,
        avg_s => $sum / $runs,
    };
}

sub run_program {
    my ($simp_path, $flags, $path) = @_;
    my @cmd = ('perl', $simp_path, @$flags, $path);
    my $pid = fork();
    die "fork failed: $!\n" unless defined $pid;
    if ($pid == 0) {
        open STDOUT, '>', File::Spec->devnull() or die "cannot open devnull: $!";
        open STDERR, '>', File::Spec->devnull() or die "cannot open devnull: $!";
        exec @cmd or die "exec failed: $!\n";
    }
    waitpid($pid, 0);
    my $status = $? >> 8;
    die "benchmark run failed for $path\n" if $status != 0;
}

sub print_table {
    my ($a, $b, $ops) = @_;
    printf "%-22s %10s %10s\n", 'case', 'avg(s)', 'ops/s';
    printf "%-22s %10.4f %10.0f\n", $a->{name}, $a->{avg_s}, $ops / $a->{avg_s};
    printf "%-22s %10.4f %10.0f\n", $b->{name}, $b->{avg_s}, $ops / $b->{avg_s};
}

sub build_result {
    my (
        $calls_v, $depth_v, $iters_v, $warmup_v,
        $rec_b, $rec_w,
        $mod_b, $mod_w,
        $str_b, $str_w,
    ) = @_;

    my $rec_ratio = $rec_w->{avg_s} / $rec_b->{avg_s};
    my $mod_ratio = $mod_w->{avg_s} / $mod_b->{avg_s};
    my $str_ratio = $str_w->{avg_s} / $str_b->{avg_s};

    return {
        timestamp_utc => strftime('%Y-%m-%dT%H:%M:%SZ', gmtime()),
        args => {
            calls  => $calls_v,
            depth  => $depth_v,
            iters  => $iters_v,
            warmup => $warmup_v,
        },
        ratios => {
            recursion_over_loop  => $rec_ratio,
            module_over_local    => $mod_ratio,
            strict_over_nonstrict => $str_ratio,
        },
        cases => {
            recursion => {
                ops => $depth_v,
                baseline => case_entry($rec_b, $depth_v),
                with_feature => case_entry($rec_w, $depth_v),
            },
            module_qualified => {
                ops => $calls_v,
                baseline => case_entry($mod_b, $calls_v),
                with_feature => case_entry($mod_w, $calls_v),
            },
            strict_globals => {
                ops => $calls_v,
                baseline => case_entry($str_b, $calls_v),
                with_feature => case_entry($str_w, $calls_v),
            },
        },
    };
}

sub case_entry {
    my ($case, $ops) = @_;
    return {
        name  => $case->{name},
        avg_s => $case->{avg_s},
        ops_s => ($ops / $case->{avg_s}),
    };
}

sub write_json {
    my ($path, $data) = @_;
    my $json = JSON::PP->new->canonical->pretty->encode($data);
    open my $fh, '>', $path or die "cannot write $path: $!\n";
    print {$fh} $json;
    close $fh;
}
