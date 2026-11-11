# Timeset formats for specifying dates

Many API calls accept timesets to specify the time ranges of data being
requested. Timesets can be specified with
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md),
as `Date` objects, or with wildcards.

## Details

Timesets are not special R types; the term simply describes any value
that is accepted by epidatr to specify the time value of an epidata
query:

- Dates: `Date` instances.

- Date strings or integers: Strings or integers in the format YYYYMMDD.

- Epiweeks: Strings or integers in the format YYYYWW, where WW is the
  epiweek number.

- EpiRanges: A range returned by
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).

- Wildcard: The string `"*"`, which requests all available time values.

Refer to the specific endpoint documentation for guidance on using dates
vs weeks. Most endpoints support only one or the other. Some (less
commonly used) endpoints may not accept the `"*"` wildcard, but this can
be simulated with a large
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).

## Examples

``` r
# A single day, as a Date, an integer, or a string (all mean 2021-03-04)
as.Date("2021-03-04")
#> [1] "2021-03-04"
20210304
#> [1] 20210304
"20210304"
#> [1] "20210304"

# Several days: a vector of dates
as.Date(c("2021-03-04", "2021-03-06"))
#> [1] "2021-03-04" "2021-03-06"

# Every day from 2021-03-01 to 2021-03-07, inclusive
epirange(20210301, 20210307)
#> 
#> ── <EpiRange> object: ──────────────────────────────────────────────────────────
#> Days from 2021-03-01 to 2021-03-07

# A single epiweek (week 9 of 2021), and a vector of epiweeks
202109
#> [1] 202109
c(202109, 202111)
#> [1] 202109 202111

# Epiweeks 9 through 12 of 2021, inclusive
epirange(202109, 202112)
#> 
#> ── <EpiRange> object: ──────────────────────────────────────────────────────────
#> Epiweeks from 2021w09 to 2021w12

# All available times (not accepted by every endpoint)
"*"
#> [1] "*"

# Use one kind (days or weeks) per parameter; most endpoints accept only one.
# To convert dates to epiweeks, use `MMWRweek::MMWRweek()`.
```
