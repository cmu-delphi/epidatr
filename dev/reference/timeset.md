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
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md),
  or a list of multiple ranges.

- Wildcard: The string `"*"`, which requests all available time values.

Refer to the specific endpoint documentation for guidance on using dates
vs weeks. Most endpoints support only one or the other. Some (less
commonly used) endpoints may not accept the `"*"` wildcard, but this can
be simulated with a large
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).
