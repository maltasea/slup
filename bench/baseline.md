# simp benchmark baseline

This file tracks a reference run for `bench/overhead-suite.pl`.

## Baseline Run

- Date (UTC): `2026-02-21T09:07:12Z`
- OS: `Darwin 24.6.0 x86_64`
- Perl: `v5.34.1`
- Command:

```sh
perl bench/overhead-suite.pl --calls 10000 --depth 300 --iters 3 --warmup 1
```

## Results

### recursion benchmark (depth=300)

| case | avg(s) | ops/s |
|---|---:|---:|
| rec-baseline(loop) | 0.0237 | 12650 |
| recursion(calls) | 0.0432 | 6950 |

- overhead ratio (recursion / loop): `1.820x`

### module-qualified benchmark (calls=10000)

| case | avg(s) | ops/s |
|---|---:|---:|
| local-sub-calls | 0.6500 | 15385 |
| module/sub-calls | 0.7116 | 14052 |

- overhead ratio (module/sub / local-sub): `1.095x`

### strict-globals benchmark (calls=10000)

| case | avg(s) | ops/s |
|---|---:|---:|
| globals(no-strict) | 0.2948 | 33919 |
| globals(strict) | 0.3038 | 32918 |

- overhead ratio (strict / no-strict): `1.030x`

## Notes

- Treat these as trend indicators, not exact constants.
- Keep command/flags unchanged when comparing runs.

## Baseline Run (2026-02-21T09:08:35Z)

- Date (UTC): `2026-02-21T09:08:35Z`
- OS: `Darwin 24.6.0 x86_64`
- Perl: `v5.34.1`
- Command:

```sh
perl bench/overhead-suite.pl --calls 10000 --depth 300 --iters 3 --warmup 1
```

```text

recursion benchmark (depth=300)
case                       avg(s)      ops/s
rec-baseline(loop)         0.0237      12649
recursion(calls)           0.0431       6966
overhead ratio (recursion / loop): 1.816x

module-qualified benchmark (calls=10000)
case                       avg(s)      ops/s
local-sub-calls            0.6579      15199
module/sub-calls           0.7133      14019
overhead ratio (module/sub / local-sub): 1.084x

strict-globals benchmark (calls=10000)
case                       avg(s)      ops/s
globals(no-strict)         0.2978      33578
globals(strict)            0.3014      33175
overhead ratio (strict / no-strict): 1.012x
```

## Baseline Run (2026-02-21T09:12:02Z)

- Date (UTC): `2026-02-21T09:12:02Z`
- OS: `Darwin 24.6.0 x86_64`
- Perl: `v5.34.1`
- Command:

```sh
perl bench/overhead-suite.pl --calls 10000 --depth 300 --iters 3 --warmup 1
```

```text

recursion benchmark (depth=300)
case                       avg(s)      ops/s
rec-baseline(loop)         0.0236      12712
recursion(calls)           0.0429       6989
overhead ratio (recursion / loop): 1.819x

module-qualified benchmark (calls=10000)
case                       avg(s)      ops/s
local-sub-calls            0.6612      15124
module/sub-calls           0.7072      14141
overhead ratio (module/sub / local-sub): 1.070x

strict-globals benchmark (calls=10000)
case                       avg(s)      ops/s
globals(no-strict)         0.2948      33924
globals(strict)            0.2968      33689
overhead ratio (strict / no-strict): 1.007x
```

## Baseline Run (2026-02-21T09:16:41Z)

- Date (UTC): `2026-02-21T09:16:41Z`
- OS: `Darwin 24.6.0 x86_64`
- Perl: `v5.34.1`
- JSON snapshot: `bench/history/20260221T091641Z.json`
- Command:

```sh
perl bench/overhead-suite.pl --calls 3000 --depth 200 --iters 2 --warmup 1
```

```text

recursion benchmark (depth=200)
case                       avg(s)      ops/s
rec-baseline(loop)         0.0203       9862
recursion(calls)           0.0331       6044
overhead ratio (recursion / loop): 1.632x

module-qualified benchmark (calls=3000)
case                       avg(s)      ops/s
local-sub-calls            0.2019      14860
module/sub-calls           0.2250      13332
overhead ratio (module/sub / local-sub): 1.115x

strict-globals benchmark (calls=3000)
case                       avg(s)      ops/s
globals(no-strict)         0.0974      30799
globals(strict)            0.1002      29928
overhead ratio (strict / no-strict): 1.029x
```

## Baseline Run (2026-02-21T09:16:46Z)

- Date (UTC): `2026-02-21T09:16:46Z`
- OS: `Darwin 24.6.0 x86_64`
- Perl: `v5.34.1`
- JSON snapshot: `bench/history/20260221T091646Z.json`
- Command:

```sh
perl bench/overhead-suite.pl --calls 3000 --depth 200 --iters 2 --warmup 1
```

```text

recursion benchmark (depth=200)
case                       avg(s)      ops/s
rec-baseline(loop)         0.0207       9680
recursion(calls)           0.0337       5927
overhead ratio (recursion / loop): 1.633x

module-qualified benchmark (calls=3000)
case                       avg(s)      ops/s
local-sub-calls            0.2025      14814
module/sub-calls           0.2395      12529
overhead ratio (module/sub / local-sub): 1.182x

strict-globals benchmark (calls=3000)
case                       avg(s)      ops/s
globals(no-strict)         0.0979      30649
globals(strict)            0.1002      29927
overhead ratio (strict / no-strict): 1.024x
```
