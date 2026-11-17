# Abort on epiweeks that don't exist.

A year has 52 or 53 MMWR weeks.
[`MMWRweek::MMWRweek2Date()`](https://rdrr.io/pkg/MMWRweek/man/MMWRweek2Date.html)
silently wraps a week past the end of the year into the next year
(`202453` is 2025w01) and fails cryptically on week 0 or 54+. No-op for
anything that isn't a YYYYWW value or an
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
of them.

## Usage

``` r
assert_valid_epiweeks(name, value)
```

## Arguments

- name:

  argument name for error messages.

- value:

  a timeset.
