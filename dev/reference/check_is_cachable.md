# Helper that checks whether a call is actually cachable

The cacheable endpoints are those with `as_of` or `issues` parameters:

- pub_covidcast

- pub_covid_hosp_state_timeseries

- pub_ecdc_ili

- pub_flusurv

- pub_fluview_clinical

- pub_fluview

- pub_kcdc_ili

- pub_nidss_flu

- pub_paho_dengue

## Usage

``` r
check_is_cachable(epidata_call, fetch_args)
```
