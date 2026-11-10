# Helper that checks whether a call is actually cachable

A call is cachable when it asks for data as of a fixed point in the
past, so later reports can't change the result.

## Usage

``` r
check_is_cachable(epidata_call, fetch_args)
```

## Details

V3/V4 endpoints with an `as_of` or `issues` parameter (other than
`"*"`):

- pub_covidcast

- pub_covid_hosp_state_timeseries

- pub_ecdc_ili

- pub_flusurv

- pub_fluview_clinical

- pub_fluview

- pub_kcdc_ili

- pub_nidss_flu

- pub_paho_dengue

V5 endpoints, when the version is bounded by a date at least two days
ago (UTC):

- [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  and
  [`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md)
  with `snapshot_date`

- [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  and
  [`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md)
  with `report_time` given as an
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  or `"<date"` (not `">date"`, `"*"`, or omitted, which are open-ended)

V5 calls with a row `limit` are never cached, since the rows returned
are not stable.
