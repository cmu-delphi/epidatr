# List all available Epidata API endpoints

Fetches a data frame of all Epidata API endpoints that can be accessed
using this package, with a brief description.

## Usage

``` r
avail_endpoints()
```

## Value

A [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
of endpoints, with two columns:

- Endpoint:

  Name of the function for accessing this API endpoint.

- Description:

  One-sentence description of the data available at the endpoint.

## Examples

``` r
avail_endpoints()
#> ℹ Data is available for the US only, unless otherwise specified
#> # A tibble: 30 × 2
#>    Endpoint                          Description                                
#>    <chr>                             <chr>                                      
#>  1 cast_api_queries()                cast-API snapshot and archive queries      
#>  2 epidata_meta()                    Get cast-API source metadata               
#>  3 pub_covid_hosp_facility()         COVID hospitalizations by facility         
#>  4 pub_covid_hosp_facility_lookup()  Helper for finding COVID hospitalization f…
#>  5 pub_covid_hosp_state_timeseries() COVID hospitalizations by state            
#>  6 pub_covidcast()                   Various COVID and flu signals via the COVI…
#>  7 pub_covidcast_meta()              Metadata for the COVIDcast endpoint        
#>  8 pub_delphi()                      Delphi's ILINet outpatient doctor visits f…
#>  9 pub_dengue_nowcast()              Delphi's PAHO dengue nowcasts (North and S…
#> 10 pub_ecdc_ili()                    ECDC ILI incidence (Europe)                
#> 11 pub_flusurv()                     CDC FluSurv flu hospitalizations           
#> 12 pub_fluview()                     CDC FluView ILINet outpatient doctor visits
#> 13 pub_fluview_clinical()            CDC FluView flu tests from clinical labs   
#> 14 pub_fluview_meta()                Metadata for the FluView endpoint          
#> 15 pub_gft()                         Google Flu Trends flu search volume        
#> 16 pub_kcdc_ili()                    KCDC ILI incidence (Korea)                 
#> 17 pub_meta()                        Metadata for the Delphi Epidata API        
#> 18 pub_nidss_dengue()                NIDSS dengue cases (Taiwan)                
#> 19 pub_nidss_flu()                   NIDSS flu doctor visits (Taiwan)           
#> 20 pub_nowcast()                     Delphi's ILI Nearby nowcasts               
#> 21 pub_paho_dengue()                 PAHO dengue data (North and South America) 
#> 22 pub_wiki()                        Wikipedia webpage counts by article        
#> 23 pvt_cdc()                         CDC total and by topic webpage visits      
#> 24 pvt_dengue_sensors()              PAHO dengue digital surveillance sensors (…
#> 25 pvt_ght()                         Google Health Trends health topics search …
#> 26 pvt_meta_norostat()               Metadata for the NoroSTAT endpoint         
#> 27 pvt_norostat()                    CDC NoroSTAT norovirus outbreaks           
#> 28 pvt_quidel()                      Quidel COVID-19 and influenza testing data 
#> 29 pvt_sensors()                     Influenza and dengue digital surveillance …
#> 30 pvt_twitter()                     HealthTweets total and influenza-related t…
```
