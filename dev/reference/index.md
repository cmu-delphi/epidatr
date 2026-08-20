# Package index

## Query the new Epidata API (v5)

The current API. Sources are moving here from the covidcast endpoint;
see
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/dev/articles/migration-guide.md)
for how to update covidcast queries.

- [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
  : Get cast-API source metadata
- [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  [`epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  : cast-API snapshot and archive queries
- [`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md)
  : Fetch V5 auxiliary data

## Query the covidcast endpoint (v4)

The previous main endpoint. Still carries the sources that have not
moved to v5 yet.

- [`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
  : Various COVID and flu signals via the COVIDcast endpoint
- [`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast_meta.md)
  : Metadata for the COVIDcast endpoint
- [`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md)
  : Creates the COVIDcast Epidata autocomplete helper

## Query legacy endpoints (v3)

Older endpoints, each with its own dataset. Most are static or no longer
updated.

- [`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_facility.md)
  : COVID hospitalizations by facility
- [`pub_covid_hosp_facility_lookup()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_facility_lookup.md)
  : Helper for finding COVID hospitalization facilities
- [`pub_covid_hosp_state_timeseries()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_state_timeseries.md)
  : COVID hospitalizations by state
- [`pub_delphi()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_delphi.md)
  : Delphi's ILINet outpatient doctor visits forecasts
- [`pub_dengue_nowcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_dengue_nowcast.md)
  : Delphi's PAHO dengue nowcasts (North and South America)
- [`pub_ecdc_ili()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_ecdc_ili.md)
  : ECDC ILI incidence (Europe)
- [`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_flusurv.md)
  : CDC FluSurv flu hospitalizations
- [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)
  : CDC FluView ILINet outpatient doctor visits
- [`pub_fluview_clinical()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview_clinical.md)
  : CDC FluView flu tests from clinical labs
- [`pub_fluview_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview_meta.md)
  : Metadata for the FluView endpoint
- [`pub_gft()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_gft.md)
  : Google Flu Trends flu search volume
- [`pub_kcdc_ili()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_kcdc_ili.md)
  : KCDC ILI incidence (Korea)
- [`pub_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_meta.md)
  : Metadata for the Delphi Epidata API
- [`pub_nidss_dengue()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_nidss_dengue.md)
  : NIDSS dengue cases (Taiwan)
- [`pub_nidss_flu()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_nidss_flu.md)
  : NIDSS flu doctor visits (Taiwan)
- [`pub_nowcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_nowcast.md)
  : Delphi's ILI Nearby nowcasts
- [`pub_paho_dengue()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_paho_dengue.md)
  : PAHO dengue data (North and South America)
- [`pub_wiki()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_wiki.md)
  : Wikipedia webpage counts by article

## Make API requests

Discover endpoints and control how queries are built and fetched

- [`avail_endpoints()`](https://cmu-delphi.github.io/epidatr/dev/reference/avail_endpoints.md)
  : List all available Epidata API endpoints
- [`create_epidata_call()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md)
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md)
  : An abstraction that holds information needed to make an epidata
  request
- [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  : Set custom API request parameters

## Configuration and utilities

Set API keys and handle API data types

- [`get_api_key()`](https://cmu-delphi.github.io/epidatr/dev/reference/get_api_key.md)
  [`save_api_key()`](https://cmu-delphi.github.io/epidatr/dev/reference/get_api_key.md)
  : Get and set API keys
- [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  : Specify a range of days or weeks for API requests
- [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md)
  : Timeset formats for specifying dates

## Control caching behavior

Configure an optional persistent cache

- [`set_cache()`](https://cmu-delphi.github.io/epidatr/dev/reference/set_cache.md)
  : Create or renew a cache for this session
- [`clear_cache()`](https://cmu-delphi.github.io/epidatr/dev/reference/clear_cache.md)
  : Manually reset the cache, deleting all currently saved data and
  starting afresh
- [`disable_cache()`](https://cmu-delphi.github.io/epidatr/dev/reference/disable_cache.md)
  : Turn off the caching for this session
- [`cache_info()`](https://cmu-delphi.github.io/epidatr/dev/reference/cache_info.md)
  : Describe current cache

## Make requests to private API endpoints

These endpoints require additional authorization to use

- [`pvt_cdc()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_cdc.md)
  : CDC total and by topic webpage visits
- [`pvt_dengue_sensors()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_dengue_sensors.md)
  : PAHO dengue digital surveillance sensors (North and South America)
- [`pvt_ght()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_ght.md)
  : Google Health Trends health topics search volume
- [`pvt_meta_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_meta_norostat.md)
  : Metadata for the NoroSTAT endpoint
- [`pvt_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_norostat.md)
  : CDC NoroSTAT norovirus outbreaks
- [`pvt_quidel()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_quidel.md)
  : Quidel COVID-19 and influenza testing data
- [`pvt_sensors()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_sensors.md)
  : Influenza and dengue digital surveillance sensors
- [`pvt_twitter()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_twitter.md)
  : HealthTweets total and influenza-related tweets
