From 1.0.0 release:
- Calls in the following files call an API which if queried too frequently without a key would result in CRAN being temporarily locked out of running API calls (so a soft-API key problem)
  - [`epidatr/R/epidatacall.R`]
  - [`epidatr/R/request.R`]
  - [`epidatr/R/endpoints.R`]

