# Creates the COVIDcast Epidata autocomplete helper

Creates a helper object that can use auto-complete to help find
COVIDcast sources and signals. The [COVIDcast
endpoint](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html)
of the Epidata API contains many separate data sources and signals. It
can be difficult to find the name of the signal you're looking for, so
you can use `covidcast_epidata` to get help with finding sources and
functions without leaving R.

The `covidcast_epidata()` function fetches a list of all signals, and
returns an object containing fields for every signal:

    epidata <- covidcast_epidata()
    epidata$signals
    #> # A tibble: 538 x 3
    #>    source        signal                        short_description
    #>    <chr>         <chr>                         <chr>
    #>  1 chng          smoothed_outpatient_cli       Estimated percentage of outpatie~
    #>  2 chng          smoothed_adj_outpatient_cli   Estimated percentage of outpatie~
    #>  3 chng          smoothed_outpatient_covid     COVID-Confirmed Doctor Visits
    #>  4 chng          smoothed_adj_outpatient_covid COVID-Confirmed Doctor Visits
    #>  5 chng          smoothed_outpatient_flu       Estimated percentage of outpatie~
    #>  6 chng          smoothed_adj_outpatient_flu   Estimated percentage of outpatie~
    #>  7 chng          7dav_inpatient_covid          Ratio of inpatient hospitalizati~
    #>  8 chng          7dav_outpatient_covid         Ratio of outpatient doctor visit~
    #>  9 covid-act-now pcr_specimen_positivity_rate  Proportion of PCR specimens test~
    #> 10 covid-act-now pcr_specimen_total_tests      Total number of PCR specimens te~
    #> # i 528 more rows

If you use an editor that supports tab completion, such as RStudio, type
`epidata$signals$` and wait for the tab completion popup. You will be
able to type the name of signals and have the autocomplete feature
select them from the list for you. Note that some signal names have
dashes in them, so to access them we rely on the backtick operator:

    epidata$signals$`fb-survey:smoothed_cli`
    #> [1] "COVID-Like Symptoms (Unweighted 7-day average)"
    #> [1] "fb-survey:smoothed_cli"
    #> [1] "Estimated percentage of people with COVID-like illness "

These objects can be used directly to fetch data, without requiring us
to use the
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.md)
function. Simply use the `$call` attribute of the object:

    epidata$signals$`fb-survey:smoothed_cli`$call("state", "pa",
                                                  epirange(20210405, 20210410))
    #> Warning: `pub_covidcast()` uses the V4 Epidata API.
    #> i Starting in October 2026, V4 is tentatively deprecated in favor of the V5
    #>   API.
    #> i See `vignette("migration-guide")` (or
    #>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
    #>   V5 endpoints and how to move to them. Old data will remain available for at
    #>   least a year, however new ingestion will end.
    #> This warning is displayed once every 8 hours.
    #> # A tibble: 6 x 15
    #>   geo_value signal     source geo_type time_type time_value direction issue
    #>   <chr>     <chr>      <chr>  <fct>    <fct>     <date>         <dbl> <date>
    #> 1 pa        smoothed_~ fb-su~ state    day       2021-04-05        NA 2021-04-10
    #> 2 pa        smoothed_~ fb-su~ state    day       2021-04-06        NA 2021-04-11
    #> 3 pa        smoothed_~ fb-su~ state    day       2021-04-07        NA 2021-04-12
    #> 4 pa        smoothed_~ fb-su~ state    day       2021-04-08        NA 2021-04-13
    #> 5 pa        smoothed_~ fb-su~ state    day       2021-04-09        NA 2021-04-14
    #> 6 pa        smoothed_~ fb-su~ state    day       2021-04-10        NA 2021-04-15
    #> # i 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
    #> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>

## Usage

``` r
covidcast_epidata(base_url = global_base_url, timeout_seconds = 30)
```

## Arguments

- base_url:

  optional alternative API base url

- timeout_seconds:

  the maximum amount of time to wait for a response

## Value

An instance of `covidcast_epidata`
