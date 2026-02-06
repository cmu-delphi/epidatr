# Finding data sources and signals of interest

The Epidata API includes numerous data streams – medical claims data,
cases and deaths, mobility, and many others – covering different
geographic regions. This can make it a challenge to find the data stream
that you are most interested in.

Example queries with all the endpoint functions available in this
package are given [below](#example-queries).

## Using the documentation

The Epidata documentation lists all the data sources and signals
available through the API for
[COVID-19](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
and for [other
diseases](https://cmu-delphi.github.io/delphi-epidata/api/README.html#source-specific-parameters).
The site also includes a search tool if you have a keyword
(e.g. “Taiwan”) in mind.

## Signal metadata

Some endpoints have partner metadata available that provides information
about the signals that are available, for example, what time ranges they
are available for, and when they have been updated.

| Endpoint             | Description                         |
|:---------------------|:------------------------------------|
| pub_covidcast_meta() | Metadata for the COVIDcast endpoint |
| pub_fluview_meta()   | Metadata for the FluView endpoint   |
| pub_meta()           | Metadata for the Delphi Epidata API |

## Interactive tooling

We provide a couple `epidatr` functions to help find data sources and
signals.

The
[`avail_endpoints()`](https://cmu-delphi.github.io/epidatr/dev/reference/avail_endpoints.md)
function lists endpoints, each of which, except for COVIDcast,
corresponds to a single data source.
[`avail_endpoints()`](https://cmu-delphi.github.io/epidatr/dev/reference/avail_endpoints.md)
outputs a `tibble` of endpoints and brief descriptions, which explicitly
state when they cover non-US locations:

``` r
avail_endpoints()
```

| Endpoint                          | Description                                                        |
|:----------------------------------|:-------------------------------------------------------------------|
| pub_covid_hosp_facility()         | COVID hospitalizations by facility                                 |
| pub_covid_hosp_facility_lookup()  | Helper for finding COVID hospitalization facilities                |
| pub_covid_hosp_state_timeseries() | COVID hospitalizations by state                                    |
| pub_covidcast()                   | Various COVID and flu signals via the COVIDcast endpoint           |
| pub_covidcast_meta()              | Metadata for the COVIDcast endpoint                                |
| pub_delphi()                      | Delphi’s ILINet outpatient doctor visits forecasts                 |
| pub_dengue_nowcast()              | Delphi’s PAHO dengue nowcasts (North and South America)            |
| pub_ecdc_ili()                    | ECDC ILI incidence (Europe)                                        |
| pub_flusurv()                     | CDC FluSurv flu hospitalizations                                   |
| pub_fluview()                     | CDC FluView ILINet outpatient doctor visits                        |
| pub_fluview_clinical()            | CDC FluView flu tests from clinical labs                           |
| pub_fluview_meta()                | Metadata for the FluView endpoint                                  |
| pub_gft()                         | Google Flu Trends flu search volume                                |
| pub_kcdc_ili()                    | KCDC ILI incidence (Korea)                                         |
| pub_meta()                        | Metadata for the Delphi Epidata API                                |
| pub_nidss_dengue()                | NIDSS dengue cases (Taiwan)                                        |
| pub_nidss_flu()                   | NIDSS flu doctor visits (Taiwan)                                   |
| pub_nowcast()                     | Delphi’s ILI Nearby nowcasts                                       |
| pub_paho_dengue()                 | PAHO dengue data (North and South America)                         |
| pub_wiki()                        | Wikipedia webpage counts by article                                |
| pvt_cdc()                         | CDC total and by topic webpage visits                              |
| pvt_dengue_sensors()              | PAHO dengue digital surveillance sensors (North and South America) |
| pvt_ght()                         | Google Health Trends health topics search volume                   |
| pvt_meta_norostat()               | Metadata for the NoroSTAT endpoint                                 |
| pvt_norostat()                    | CDC NoroSTAT norovirus outbreaks                                   |
| pvt_quidel()                      | Quidel COVID-19 and influenza testing data                         |
| pvt_sensors()                     | Influenza and dengue digital surveillance sensors                  |
| pvt_twitter()                     | HealthTweets total and influenza-related tweets                    |

The
[`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md)
function lets you look more in-depth at the data sources available
through the COVIDcast endpoint. The function describes all available
data sources and signals:

``` r
covid_sources <- covidcast_epidata()
head(covid_sources$sources, n = 2)
#> $chng
#> [1] "Change Healthcare"
#> [1] "chng"
#> [1] "Change Healthcare is a healthcare technology company that aggregates medical claims data from many healthcare providers. This source includes aggregated counts of claims with confirmed COVID-19 or COVID-related symptoms. All claims data has been de-identified in accordance with HIPAA privacy regulations. "
#> # A tibble: 8 × 2
#>   signal                        short_description                               
#>   <chr>                         <chr>                                           
#> 1 smoothed_outpatient_cli       Estimated percentage of outpatient doctor visit…
#> 2 smoothed_adj_outpatient_cli   Estimated percentage of outpatient doctor visit…
#> 3 smoothed_outpatient_covid     COVID-Confirmed Doctor Visits                   
#> 4 smoothed_adj_outpatient_covid COVID-Confirmed Doctor Visits                   
#> # ℹ 4 more rows
#> 
#> $`covid-act-now`
#> [1] "Covid Act Now (CAN)"
#> [1] "covid-act-now"
#> [1] "COVID Act Now (CAN) tracks COVID-19 testing statistics, such as positivity rates and total tests performed. This source only includes CAN data from the CDC's COVID-19 Integrated County View."
#> # A tibble: 2 × 2
#>   signal                       short_description                                
#>   <chr>                        <chr>                                            
#> 1 pcr_specimen_positivity_rate Proportion of PCR specimens tested that have a p…
#> 2 pcr_specimen_total_tests     Total number of PCR specimens tested
```

Each source is included as an entry in the `covid_sources$sources` list,
associated with a `tibble` describing included signals.

If you use an editor that supports tab completion, such as RStudio, type
`covid_sources$source$` and wait for the tab completion popup. You will
be able to browse the list of data sources.

``` r
covid_sources$signals
#> # A tibble: 520 × 3
#>   source signal                        short_description                        
#>   <chr>  <chr>                         <chr>                                    
#> 1 chng   smoothed_outpatient_cli       Estimated percentage of outpatient docto…
#> 2 chng   smoothed_adj_outpatient_cli   Estimated percentage of outpatient docto…
#> 3 chng   smoothed_outpatient_covid     COVID-Confirmed Doctor Visits            
#> 4 chng   smoothed_adj_outpatient_covid COVID-Confirmed Doctor Visits            
#> # ℹ 516 more rows
```

If you use an editor that supports tab completion, type
`covid_sources$signals$` and wait for the tab completion popup. You will
be able to type the name of signals and have the autocomplete feature
select them from the list for you. In the tab-completion popup, signal
names are prefixed with the name of the data source for filtering
convenience.

*Note* that some signal names have dashes in them, so to access them we
rely on the backtick operator:

``` r
covid_sources$signals$`fb-survey:smoothed_cli`
#> [1] "COVID-Like Symptoms (Unweighted 7-day average)"
#> [1] "fb-survey:smoothed_cli"
#> [1] "Estimated percentage of people with COVID-like illness "
```

These signal objects can be used directly to fetch data, without
requiring us to use the
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
function. Simply use the `$call` attribute of the object:

``` r
epidata <- covid_sources$signals$`fb-survey:smoothed_cli`$call(
  "state", "pa", epirange(20210405, 20210410)
)
knitr::kable(epidata)
```

| geo_value | signal       | source    | geo_type | time_type | time_value | direction | issue      | lag | missing_value | missing_stderr | missing_sample_size |     value |    stderr | sample_size |
|:----------|:-------------|:----------|:---------|:----------|:-----------|----------:|:-----------|----:|--------------:|---------------:|--------------------:|----------:|----------:|------------:|
| pa        | smoothed_cli | fb-survey | state    | day       | 2021-04-05 |        NA | 2021-04-10 |   5 |             0 |              0 |                   0 | 0.7157576 | 0.0729992 |    10894.01 |
| pa        | smoothed_cli | fb-survey | state    | day       | 2021-04-06 |        NA | 2021-04-11 |   5 |             0 |              0 |                   0 | 0.6932097 | 0.0708692 |    10862.01 |
| pa        | smoothed_cli | fb-survey | state    | day       | 2021-04-07 |        NA | 2021-04-12 |   5 |             0 |              0 |                   0 | 0.6859343 | 0.0706536 |    10790.01 |
| pa        | smoothed_cli | fb-survey | state    | day       | 2021-04-08 |        NA | 2021-04-13 |   5 |             0 |              0 |                   0 | 0.6815110 | 0.0713939 |    10731.00 |
| pa        | smoothed_cli | fb-survey | state    | day       | 2021-04-09 |        NA | 2021-04-14 |   5 |             0 |              0 |                   0 | 0.7094162 | 0.0721616 |    10590.00 |
| pa        | smoothed_cli | fb-survey | state    | day       | 2021-04-10 |        NA | 2021-04-15 |   5 |             0 |              0 |                   0 | 0.7762399 | 0.0760370 |    10492.01 |

## Example Queries

### COVIDcast Main Endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>

County geo_values are [FIPS
codes](https://en.wikipedia.org/wiki/List_of_United_States_FIPS_codes_by_county)
and are discussed in the API docs
[here](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html).
The example below is for Orange County, California.

``` r
pub_covidcast(
  source = "fb-survey",
  signals = "smoothed_accept_covid_vaccine",
  geo_type = "county",
  time_type = "day",
  time_values = epirange(20201221, 20201225),
  geo_values = "06059"
)
#> # A tibble: 5 × 15
#>   geo_value signal     source geo_type time_type time_value direction issue     
#>   <chr>     <chr>      <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#> 1 06059     smoothed_… fb-su… county   day       2020-12-21        NA 2020-12-22
#> 2 06059     smoothed_… fb-su… county   day       2020-12-22        NA 2020-12-23
#> 3 06059     smoothed_… fb-su… county   day       2020-12-23        NA 2020-12-24
#> 4 06059     smoothed_… fb-su… county   day       2020-12-24        NA 2020-12-25
#> # ℹ 1 more row
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```

The `covidcast` endpoint supports `*` in its time and geo fields:

``` r
pub_covidcast(
  source = "fb-survey",
  signals = "smoothed_accept_covid_vaccine",
  geo_type = "county",
  time_type = "day",
  time_values = epirange(20201221, 20201225),
  geo_values = "*"
)
#> # A tibble: 2,025 × 15
#>   geo_value signal     source geo_type time_type time_value direction issue     
#>   <chr>     <chr>      <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#> 1 01000     smoothed_… fb-su… county   day       2020-12-21        NA 2020-12-22
#> 2 01073     smoothed_… fb-su… county   day       2020-12-21        NA 2020-12-22
#> 3 01089     smoothed_… fb-su… county   day       2020-12-21        NA 2020-12-22
#> 4 01097     smoothed_… fb-su… county   day       2020-12-21        NA 2020-12-22
#> # ℹ 2,021 more rows
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```

### Other Covid Endpoints

#### COVID-19 Hospitalization: Facility Lookup

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html>

``` r
pub_covid_hosp_facility_lookup(city = "southlake")
pub_covid_hosp_facility_lookup(state = "WY")
# A non-example (there is no city called New York in Wyoming)
pub_covid_hosp_facility_lookup(state = "WY", city = "New York")
```

#### COVID-19 Hospitalization by Facility

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility.html>

``` r
pub_covid_hosp_facility(
  hospital_pks = "100075",
  collection_weeks = epirange(20200101, 20200501)
)
```

#### COVID-19 Hospitalization by State

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp.html>

``` r
pub_covid_hosp_state_timeseries(states = "MA", dates = "20200510")
```

### Flu Endpoints

#### Delphi’s ILINet forecasts

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/delphi.html>

``` r
del <- pub_delphi(system = "ec", epiweek = 201501)
names(del[[1L]]$forecast)
```

#### FluSurv hospitalization data

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/flusurv.html>

``` r
pub_flusurv(locations = "ca", epiweeks = 202001)
```

#### Fluview data

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/fluview.html>

``` r
pub_fluview(regions = "nat", epiweeks = epirange(201201, 202001))
```

#### Fluview virological data from clinical labs

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html>

``` r
pub_fluview_clinical(regions = "nat", epiweeks = epirange(201601, 201701))
```

#### Fluview metadata

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview_meta.html>

``` r
pub_fluview_meta()
```

#### Google Flu Trends data

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/gft.html>

``` r
pub_gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
```

#### ECDC ILI

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/ecdc_ili.html>

``` r
pub_ecdc_ili(regions = "Armenia", epiweeks = 201840)
```

#### KCDC ILI

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/kcdc_ili.html>

``` r
pub_kcdc_ili(regions = "ROK", epiweeks = 200436)
```

#### NIDSS Flu

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nidss_flu.html>

``` r
pub_nidss_flu(regions = "taipei", epiweeks = epirange(200901, 201301))
```

#### ILI Nearby Nowcast

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/nowcast.html>

``` r
pub_nowcast(locations = "ca", epiweeks = epirange(202201, 202319))
```

### Dengue Endpoints

#### Delphi’s Dengue Nowcast

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html>

``` r
pub_dengue_nowcast(locations = "pr", epiweeks = epirange(201401, 202301))
```

#### NIDSS dengue

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nidss_dengue.html>

``` r
pub_nidss_dengue(locations = "taipei", epiweeks = epirange(200301, 201301))
```

### PAHO Dengue

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/paho_dengue.html>

``` r
pub_paho_dengue(regions = "ca", epiweeks = epirange(200201, 202319))
```

### Other Endpoints

#### Wikipedia Access

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/wiki.html>

``` r
pub_wiki(
  language = "en",
  articles = "influenza",
  time_type = "week",
  time_values = epirange(202001, 202319)
)
```

### Private methods

These require private access keys to use (separate from the Delphi
Epidata API key). To actually run these locally, you will need to store
these secrets in your `.Reviron` file, or set them as environmental
variables.

Usage of private endpoints

#### CDC

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/cdc.html>

``` r
pvt_cdc(auth = Sys.getenv("SECRET_API_AUTH_CDC"), epiweeks = epirange(202003, 202304), locations = "ma")
```

#### Dengue Digital Surveillance Sensors

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html>

``` r
pvt_dengue_sensors(
  auth = Sys.getenv("SECRET_API_AUTH_SENSORS"),
  names = "ght",
  locations = "ag",
  epiweeks = epirange(201404, 202004)
)
```

#### Google Health Trends

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/ght.html>

``` r
pvt_ght(
  auth = Sys.getenv("SECRET_API_AUTH_GHT"),
  epiweeks = epirange(199301, 202304),
  locations = "ma",
  query = "how to get over the flu"
)
```

#### NoroSTAT metadata

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html>

``` r
pvt_meta_norostat(auth = Sys.getenv("SECRET_API_AUTH_NOROSTAT"))
```

#### NoroSTAT data

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/norostat.html>

``` r
pvt_norostat(auth = Sys.getenv("SECRET_API_AUTH_NOROSTAT"), locations = "1", epiweeks = 201233)
```

#### Quidel Influenza testing

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/quidel.html>

``` r
pvt_quidel(auth = Sys.getenv("SECRET_API_AUTH_QUIDEL"), locations = "hhs1", epiweeks = epirange(200301, 202105))
```

#### Sensors

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/sensors.html>

``` r
pvt_sensors(
  auth = Sys.getenv("SECRET_API_AUTH_SENSORS"),
  names = "sar3",
  locations = "nat",
  epiweeks = epirange(200301, 202105)
)
```

#### Twitter

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/twitter.html>

``` r
pvt_twitter(
  auth = Sys.getenv("SECRET_API_AUTH_TWITTER"),
  locations = "nat",
  time_type = "week",
  time_values = epirange(200301, 202105)
)
```
