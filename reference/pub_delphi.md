# Delphi's ILINet outpatient doctor visits forecasts

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/delphi.html>

## Usage

``` r
pub_delphi(system, epiweek, fetch_args = fetch_args_list())
```

## Arguments

- system:

  character. System name to fetch. See the [available forecasting
  systems](https://cmu-delphi.github.io/delphi-epidata/api/delphi.html#forecasting-systems)
  \# nolint for details.

- epiweek:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweek to fetch. Does not support multiple dates. Make separate calls
  to fetch data for multiple epiweeks.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

## Value

[`list`](https://rdrr.io/r/base/list.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## Examples

``` r

pub_delphi(system = "ec", epiweek = 201501)
#> [[1]]
#> [[1]]$epiweek
#> [1] 201501
#> 
#> [[1]]$forecast
#> [[1]]$forecast$`_version`
#> [1] 1
#> 
#> [[1]]$forecast$baselines
#> [[1]]$forecast$baselines$hhs1
#> [1] 1.2
#> 
#> [[1]]$forecast$baselines$hhs10
#> [1] 1.1
#> 
#> [[1]]$forecast$baselines$hhs2
#> [1] 2.3
#> 
#> [[1]]$forecast$baselines$hhs3
#> [1] 2
#> 
#> [[1]]$forecast$baselines$hhs4
#> [1] 1.9
#> 
#> [[1]]$forecast$baselines$hhs5
#> [1] 1.7
#> 
#> [[1]]$forecast$baselines$hhs6
#> [1] 3.3
#> 
#> [[1]]$forecast$baselines$hhs7
#> [1] 1.7
#> 
#> [[1]]$forecast$baselines$hhs8
#> [1] 1.3
#> 
#> [[1]]$forecast$baselines$hhs9
#> [1] 2.7
#> 
#> [[1]]$forecast$baselines$nat
#> [1] 2
#> 
#> 
#> [[1]]$forecast$data
#> [[1]]$forecast$data$hhs1
#> [[1]]$forecast$data$hhs1$onset
#> [[1]]$forecast$data$hhs1$onset$dist
#> [[1]]$forecast$data$hhs1$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[10]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$onset$point
#> [1] 49
#> 
#> 
#> [[1]]$forecast$data$hhs1$peak
#> [[1]]$forecast$data$hhs1$peak$dist
#> [[1]]$forecast$data$hhs1$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[3]]
#> [1] 0.606623
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[4]]
#> [1] 0.389349
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[5]]
#> [1] 0.003301
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$peak$point
#> [1] 2.68261
#> 
#> 
#> [[1]]$forecast$data$hhs1$peakweek
#> [[1]]$forecast$data$hhs1$peakweek$dist
#> [[1]]$forecast$data$hhs1$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[15]]
#> [1] 0.141845
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[16]]
#> [1] 0.166269
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[17]]
#> [1] 0.17314
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[18]]
#> [1] 0.160168
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[19]]
#> [1] 0.131628
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[20]]
#> [1] 0.096099
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[21]]
#> [1] 0.062331
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[22]]
#> [1] 0.03592
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[23]]
#> [1] 0.018396
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[24]]
#> [1] 0.008379
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[25]]
#> [1] 0.003401
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[26]]
#> [1] 0.001238
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[27]]
#> [1] 0.000414
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[28]]
#> [1] 0.000138
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[29]]
#> [1] 5.6e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[30]]
#> [1] 3.5e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[31]]
#> [1] 3e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs1$peakweek$point
#> [1] 2
#> 
#> 
#> [[1]]$forecast$data$hhs1$x1
#> [[1]]$forecast$data$hhs1$x1$dist
#> [[1]]$forecast$data$hhs1$x1$dist[[1]]
#> [1] 0.000299
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[2]]
#> [1] 0.142162
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[3]]
#> [1] 0.774656
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[4]]
#> [1] 0.082188
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[5]]
#> [1] 0.00015
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$x1$point
#> [1] 2.57828
#> 
#> 
#> [[1]]$forecast$data$hhs1$x2
#> [[1]]$forecast$data$hhs1$x2$dist
#> [[1]]$forecast$data$hhs1$x2$dist[[1]]
#> [1] 0.008214
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[2]]
#> [1] 0.284314
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[3]]
#> [1] 0.611941
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[4]]
#> [1] 0.094128
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[5]]
#> [1] 0.000857
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$x2$point
#> [1] 2.2778
#> 
#> 
#> [[1]]$forecast$data$hhs1$x3
#> [[1]]$forecast$data$hhs1$x3$dist
#> [[1]]$forecast$data$hhs1$x3$dist[[1]]
#> [1] 0.009499
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[2]]
#> [1] 0.316014
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[3]]
#> [1] 0.599539
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[4]]
#> [1] 0.073896
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[5]]
#> [1] 0.000508
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$x3$point
#> [1] 2.2778
#> 
#> 
#> [[1]]$forecast$data$hhs1$x4
#> [[1]]$forecast$data$hhs1$x4$dist
#> [[1]]$forecast$data$hhs1$x4$dist[[1]]
#> [1] 0.012442
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[2]]
#> [1] 0.28382
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[3]]
#> [1] 0.583112
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[4]]
#> [1] 0.118032
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[5]]
#> [1] 0.002046
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[6]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs1$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs1$x4$point
#> [1] 2.26591
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs10
#> [[1]]$forecast$data$hhs10$onset
#> [[1]]$forecast$data$hhs10$onset$dist
#> [[1]]$forecast$data$hhs10$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[10]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs10$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$onset$point
#> [1] 49
#> 
#> 
#> [[1]]$forecast$data$hhs10$peak
#> [[1]]$forecast$data$hhs10$peak$dist
#> [[1]]$forecast$data$hhs10$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[4]]
#> [1] 0.265469
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[5]]
#> [1] 0.327005
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[6]]
#> [1] 0.248962
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[7]]
#> [1] 0.11714
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[8]]
#> [1] 0.034076
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[9]]
#> [1] 0.006176
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[10]]
#> [1] 0.000762
#> 
#> [[1]]$forecast$data$hhs10$peak$dist[[11]]
#> [1] 0.000138
#> 
#> 
#> [[1]]$forecast$data$hhs10$peak$point
#> [1] 3.827
#> 
#> 
#> [[1]]$forecast$data$hhs10$peakweek
#> [[1]]$forecast$data$hhs10$peakweek$dist
#> [[1]]$forecast$data$hhs10$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[15]]
#> [1] 0.059974
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[16]]
#> [1] 0.085468
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[17]]
#> [1] 0.110244
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[18]]
#> [1] 0.128706
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[19]]
#> [1] 0.135998
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[20]]
#> [1] 0.130064
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[21]]
#> [1] 0.112583
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[22]]
#> [1] 0.088203
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[23]]
#> [1] 0.062546
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[24]]
#> [1] 0.040146
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[25]]
#> [1] 0.023328
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[26]]
#> [1] 0.012275
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[27]]
#> [1] 0.005855
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[28]]
#> [1] 0.002537
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[29]]
#> [1] 0.001006
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[30]]
#> [1] 0.000373
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[31]]
#> [1] 0.000139
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[32]]
#> [1] 6e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[33]]
#> [1] 3.7e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$dist[[34]]
#> [1] 3.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs10$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs10$peakweek$point
#> [1] 5
#> 
#> 
#> [[1]]$forecast$data$hhs10$x1
#> [[1]]$forecast$data$hhs10$x1$dist
#> [[1]]$forecast$data$hhs10$x1$dist[[1]]
#> [1] 0.000239
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[2]]
#> [1] 0.022839
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[3]]
#> [1] 0.330283
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[4]]
#> [1] 0.539504
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[5]]
#> [1] 0.10451
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[6]]
#> [1] 0.002166
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[7]]
#> [1] 9.5e-05
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs10$x1$point
#> [1] 3.09921
#> 
#> 
#> [[1]]$forecast$data$hhs10$x2
#> [[1]]$forecast$data$hhs10$x2$dist
#> [[1]]$forecast$data$hhs10$x2$dist[[1]]
#> [1] 0.003541
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[2]]
#> [1] 0.052104
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[3]]
#> [1] 0.255792
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[4]]
#> [1] 0.417205
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[5]]
#> [1] 0.227338
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[6]]
#> [1] 0.041111
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[7]]
#> [1] 0.0025
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[8]]
#> [1] 0.000136
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs10$x2$point
#> [1] 3.12097
#> 
#> 
#> [[1]]$forecast$data$hhs10$x3
#> [[1]]$forecast$data$hhs10$x3$dist
#> [[1]]$forecast$data$hhs10$x3$dist[[1]]
#> [1] 0.007661
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[2]]
#> [1] 0.066554
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[3]]
#> [1] 0.242833
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[4]]
#> [1] 0.371571
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[5]]
#> [1] 0.238944
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[6]]
#> [1] 0.064435
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[7]]
#> [1] 0.0073
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[8]]
#> [1] 0.000424
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[9]]
#> [1] 9.7e-05
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs10$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs10$x3$point
#> [1] 3.12097
#> 
#> 
#> [[1]]$forecast$data$hhs10$x4
#> [[1]]$forecast$data$hhs10$x4$dist
#> [[1]]$forecast$data$hhs10$x4$dist[[1]]
#> [1] 0.012907
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[2]]
#> [1] 0.074889
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[3]]
#> [1] 0.220362
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[4]]
#> [1] 0.328373
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[5]]
#> [1] 0.247992
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[6]]
#> [1] 0.094864
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[7]]
#> [1] 0.018384
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[8]]
#> [1] 0.001867
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[9]]
#> [1] 0.000177
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[10]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs10$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs10$x4$point
#> [1] 2.976
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs2
#> [[1]]$forecast$data$hhs2$onset
#> [[1]]$forecast$data$hhs2$onset$dist
#> [[1]]$forecast$data$hhs2$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[6]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$onset$point
#> [1] 45
#> 
#> 
#> [[1]]$forecast$data$hhs2$peak
#> [[1]]$forecast$data$hhs2$peak$dist
#> [[1]]$forecast$data$hhs2$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[6]]
#> [1] 0.999091
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$peak$point
#> [1] 5.836
#> 
#> 
#> [[1]]$forecast$data$hhs2$peakweek
#> [[1]]$forecast$data$hhs2$peakweek$dist
#> [[1]]$forecast$data$hhs2$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[13]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs2$peakweek$point
#> [1] 52
#> 
#> 
#> [[1]]$forecast$data$hhs2$x1
#> [[1]]$forecast$data$hhs2$x1$dist
#> [[1]]$forecast$data$hhs2$x1$dist[[1]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[2]]
#> [1] 0.000729
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[3]]
#> [1] 0.030231
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[4]]
#> [1] 0.271193
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[5]]
#> [1] 0.494915
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[6]]
#> [1] 0.187903
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[7]]
#> [1] 0.014368
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[8]]
#> [1] 0.000294
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$x1$point
#> [1] 4.471
#> 
#> 
#> [[1]]$forecast$data$hhs2$x2
#> [[1]]$forecast$data$hhs2$x2$dist
#> [[1]]$forecast$data$hhs2$x2$dist[[1]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[2]]
#> [1] 0.000968
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[3]]
#> [1] 0.051853
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[4]]
#> [1] 0.403065
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[5]]
#> [1] 0.462591
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[6]]
#> [1] 0.079156
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[7]]
#> [1] 0.001905
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[8]]
#> [1] 9.6e-05
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$x2$point
#> [1] 4.143
#> 
#> 
#> [[1]]$forecast$data$hhs2$x3
#> [[1]]$forecast$data$hhs2$x3$dist
#> [[1]]$forecast$data$hhs2$x3$dist[[1]]
#> [1] 0.000106
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[2]]
#> [1] 0.004776
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[3]]
#> [1] 0.149634
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[4]]
#> [1] 0.55783
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[5]]
#> [1] 0.270959
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[6]]
#> [1] 0.01614
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[7]]
#> [1] 0.000191
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$x3$point
#> [1] 4.143
#> 
#> 
#> [[1]]$forecast$data$hhs2$x4
#> [[1]]$forecast$data$hhs2$x4$dist
#> [[1]]$forecast$data$hhs2$x4$dist[[1]]
#> [1] 0.00019
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[2]]
#> [1] 0.012741
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[3]]
#> [1] 0.215321
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[4]]
#> [1] 0.542862
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[5]]
#> [1] 0.215562
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[6]]
#> [1] 0.01277
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[7]]
#> [1] 0.00019
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs2$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs2$x4$point
#> [1] 3.612
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs3
#> [[1]]$forecast$data$hhs3$onset
#> [[1]]$forecast$data$hhs3$onset$dist
#> [[1]]$forecast$data$hhs3$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[9]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$onset$point
#> [1] 48
#> 
#> 
#> [[1]]$forecast$data$hhs3$peak
#> [[1]]$forecast$data$hhs3$peak$dist
#> [[1]]$forecast$data$hhs3$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[7]]
#> [1] 0.404385
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[8]]
#> [1] 0.573085
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[9]]
#> [1] 0.021793
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[10]]
#> [1] 0.000101
#> 
#> [[1]]$forecast$data$hhs3$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$peak$point
#> [1] 6.991
#> 
#> 
#> [[1]]$forecast$data$hhs3$peakweek
#> [[1]]$forecast$data$hhs3$peakweek$dist
#> [[1]]$forecast$data$hhs3$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[13]]
#> [1] 0.490594
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[16]]
#> [1] 0.278137
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[17]]
#> [1] 0.144772
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[18]]
#> [1] 0.059761
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[19]]
#> [1] 0.019573
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[20]]
#> [1] 0.005098
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[21]]
#> [1] 0.001071
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[22]]
#> [1] 0.000198
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[23]]
#> [1] 5.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[24]]
#> [1] 3.1e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs3$peakweek$point
#> [1] 52
#> 
#> 
#> [[1]]$forecast$data$hhs3$x1
#> [[1]]$forecast$data$hhs3$x1$dist
#> [[1]]$forecast$data$hhs3$x1$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[3]]
#> [1] 0.000434
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[4]]
#> [1] 0.081742
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[5]]
#> [1] 0.647287
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[6]]
#> [1] 0.265462
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[7]]
#> [1] 0.004528
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[8]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$x1$point
#> [1] 4.59632
#> 
#> 
#> [[1]]$forecast$data$hhs3$x2
#> [[1]]$forecast$data$hhs3$x2$dist
#> [[1]]$forecast$data$hhs3$x2$dist[[1]]
#> [1] 0.000147
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[2]]
#> [1] 0.004844
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[3]]
#> [1] 0.089483
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[4]]
#> [1] 0.389223
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[5]]
#> [1] 0.407192
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[6]]
#> [1] 0.102588
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[7]]
#> [1] 0.006081
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[8]]
#> [1] 0.000168
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$x2$point
#> [1] 3.94511
#> 
#> 
#> [[1]]$forecast$data$hhs3$x3
#> [[1]]$forecast$data$hhs3$x3$dist
#> [[1]]$forecast$data$hhs3$x3$dist[[1]]
#> [1] 0.000578
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[2]]
#> [1] 0.014584
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[3]]
#> [1] 0.132912
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[4]]
#> [1] 0.383475
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[5]]
#> [1] 0.353809
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[6]]
#> [1] 0.104275
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[7]]
#> [1] 0.009727
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[8]]
#> [1] 0.000364
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[9]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs3$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$x3$point
#> [1] 3.94511
#> 
#> 
#> [[1]]$forecast$data$hhs3$x4
#> [[1]]$forecast$data$hhs3$x4$dist
#> [[1]]$forecast$data$hhs3$x4$dist[[1]]
#> [1] 0.004678
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[2]]
#> [1] 0.042022
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[3]]
#> [1] 0.175175
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[4]]
#> [1] 0.335853
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[5]]
#> [1] 0.296666
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[6]]
#> [1] 0.12068
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[7]]
#> [1] 0.022576
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[8]]
#> [1] 0.002002
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[9]]
#> [1] 0.000164
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[10]]
#> [1] 9.2e-05
#> 
#> [[1]]$forecast$data$hhs3$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs3$x4$point
#> [1] 3.72394
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs4
#> [[1]]$forecast$data$hhs4$onset
#> [[1]]$forecast$data$hhs4$onset$dist
#> [[1]]$forecast$data$hhs4$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[8]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$onset$point
#> [1] 47
#> 
#> 
#> [[1]]$forecast$data$hhs4$peak
#> [[1]]$forecast$data$hhs4$peak$dist
#> [[1]]$forecast$data$hhs4$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[8]]
#> [1] 0.999091
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$peak$point
#> [1] 7.517
#> 
#> 
#> [[1]]$forecast$data$hhs4$peakweek
#> [[1]]$forecast$data$hhs4$peakweek$dist
#> [[1]]$forecast$data$hhs4$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[13]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs4$peakweek$point
#> [1] 52
#> 
#> 
#> [[1]]$forecast$data$hhs4$x1
#> [[1]]$forecast$data$hhs4$x1$dist
#> [[1]]$forecast$data$hhs4$x1$dist[[1]]
#> [1] 0.011206
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[2]]
#> [1] 0.077571
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[3]]
#> [1] 0.245094
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[4]]
#> [1] 0.353247
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[5]]
#> [1] 0.232533
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[6]]
#> [1] 0.069814
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[7]]
#> [1] 0.009574
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[8]]
#> [1] 0.000672
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[9]]
#> [1] 0.000107
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$x1$point
#> [1] 3.069
#> 
#> 
#> [[1]]$forecast$data$hhs4$x2
#> [[1]]$forecast$data$hhs4$x2$dist
#> [[1]]$forecast$data$hhs4$x2$dist[[1]]
#> [1] 0.020822
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[2]]
#> [1] 0.109403
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[3]]
#> [1] 0.276134
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[4]]
#> [1] 0.335043
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[5]]
#> [1] 0.195526
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[6]]
#> [1] 0.054814
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[7]]
#> [1] 0.007413
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[8]]
#> [1] 0.000557
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[9]]
#> [1] 0.000105
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$x2$point
#> [1] 2.76
#> 
#> 
#> [[1]]$forecast$data$hhs4$x3
#> [[1]]$forecast$data$hhs4$x3$dist
#> [[1]]$forecast$data$hhs4$x3$dist[[1]]
#> [1] 0.029399
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[2]]
#> [1] 0.129114
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[3]]
#> [1] 0.285572
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[4]]
#> [1] 0.318314
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[5]]
#> [1] 0.178853
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[6]]
#> [1] 0.05061
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[7]]
#> [1] 0.007249
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[8]]
#> [1] 0.000597
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[9]]
#> [1] 0.000109
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$x3$point
#> [1] 2.76
#> 
#> 
#> [[1]]$forecast$data$hhs4$x4
#> [[1]]$forecast$data$hhs4$x4$dist
#> [[1]]$forecast$data$hhs4$x4$dist[[1]]
#> [1] 0.028264
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[2]]
#> [1] 0.153558
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[3]]
#> [1] 0.345858
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[4]]
#> [1] 0.323968
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[5]]
#> [1] 0.126162
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[6]]
#> [1] 0.020365
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[7]]
#> [1] 0.001425
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[8]]
#> [1] 0.000126
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs4$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs4$x4$point
#> [1] 2.516
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs5
#> [[1]]$forecast$data$hhs5$onset
#> [[1]]$forecast$data$hhs5$onset$dist
#> [[1]]$forecast$data$hhs5$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[9]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$onset$point
#> [1] 48
#> 
#> 
#> [[1]]$forecast$data$hhs5$peak
#> [[1]]$forecast$data$hhs5$peak$dist
#> [[1]]$forecast$data$hhs5$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[7]]
#> [1] 0.999091
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$peak$point
#> [1] 6.559
#> 
#> 
#> [[1]]$forecast$data$hhs5$peakweek
#> [[1]]$forecast$data$hhs5$peakweek$dist
#> [[1]]$forecast$data$hhs5$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[13]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs5$peakweek$point
#> [1] 52
#> 
#> 
#> [[1]]$forecast$data$hhs5$x1
#> [[1]]$forecast$data$hhs5$x1$dist
#> [[1]]$forecast$data$hhs5$x1$dist[[1]]
#> [1] 0.000212
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[2]]
#> [1] 0.023922
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[3]]
#> [1] 0.363184
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[4]]
#> [1] 0.532228
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[5]]
#> [1] 0.078934
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[6]]
#> [1] 0.001065
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[7]]
#> [1] 9.2e-05
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$x1$point
#> [1] 2.93489
#> 
#> 
#> [[1]]$forecast$data$hhs5$x2
#> [[1]]$forecast$data$hhs5$x2$dist
#> [[1]]$forecast$data$hhs5$x2$dist[[1]]
#> [1] 0.000118
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[2]]
#> [1] 0.023279
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[3]]
#> [1] 0.499287
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[4]]
#> [1] 0.459099
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[5]]
#> [1] 0.017656
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[6]]
#> [1] 0.000107
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$x2$point
#> [1] 2.84025
#> 
#> 
#> [[1]]$forecast$data$hhs5$x3
#> [[1]]$forecast$data$hhs5$x3$dist
#> [[1]]$forecast$data$hhs5$x3$dist[[1]]
#> [1] 0.015845
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[2]]
#> [1] 0.122716
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[3]]
#> [1] 0.346468
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[4]]
#> [1] 0.358614
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[5]]
#> [1] 0.136122
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[6]]
#> [1] 0.018838
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[7]]
#> [1] 0.001016
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[8]]
#> [1] 0.000107
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$x3$point
#> [1] 2.84025
#> 
#> 
#> [[1]]$forecast$data$hhs5$x4
#> [[1]]$forecast$data$hhs5$x4$dist
#> [[1]]$forecast$data$hhs5$x4$dist[[1]]
#> [1] 0.039492
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[2]]
#> [1] 0.154966
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[3]]
#> [1] 0.305849
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[4]]
#> [1] 0.303889
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[5]]
#> [1] 0.152003
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[6]]
#> [1] 0.038242
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[7]]
#> [1] 0.004885
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[8]]
#> [1] 0.000391
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[9]]
#> [1] 1e-04
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs5$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs5$x4$point
#> [1] 2.48465
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs6
#> [[1]]$forecast$data$hhs6$onset
#> [[1]]$forecast$data$hhs6$onset$dist
#> [[1]]$forecast$data$hhs6$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[8]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs6$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$onset$point
#> [1] 47
#> 
#> 
#> [[1]]$forecast$data$hhs6$peak
#> [[1]]$forecast$data$hhs6$peak$dist
#> [[1]]$forecast$data$hhs6$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$peak$dist[[11]]
#> [1] 0.999091
#> 
#> 
#> [[1]]$forecast$data$hhs6$peak$point
#> [1] 11.31
#> 
#> 
#> [[1]]$forecast$data$hhs6$peakweek
#> [[1]]$forecast$data$hhs6$peakweek$dist
#> [[1]]$forecast$data$hhs6$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[12]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs6$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs6$peakweek$point
#> [1] 51
#> 
#> 
#> [[1]]$forecast$data$hhs6$x1
#> [[1]]$forecast$data$hhs6$x1$dist
#> [[1]]$forecast$data$hhs6$x1$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[4]]
#> [1] 9.4e-05
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[5]]
#> [1] 0.001179
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[6]]
#> [1] 0.049718
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[7]]
#> [1] 0.365378
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[8]]
#> [1] 0.471076
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[9]]
#> [1] 0.107979
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[10]]
#> [1] 0.004188
#> 
#> [[1]]$forecast$data$hhs6$x1$dist[[11]]
#> [1] 0.000115
#> 
#> 
#> [[1]]$forecast$data$hhs6$x1$point
#> [1] 6.96399
#> 
#> 
#> [[1]]$forecast$data$hhs6$x2
#> [[1]]$forecast$data$hhs6$x2$dist
#> [[1]]$forecast$data$hhs6$x2$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[2]]
#> [1] 0.000115
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[3]]
#> [1] 0.00073
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[4]]
#> [1] 0.008599
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[5]]
#> [1] 0.056821
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[6]]
#> [1] 0.190236
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[7]]
#> [1] 0.32153
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[8]]
#> [1] 0.274588
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[9]]
#> [1] 0.118447
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[10]]
#> [1] 0.025795
#> 
#> [[1]]$forecast$data$hhs6$x2$dist[[11]]
#> [1] 0.003048
#> 
#> 
#> [[1]]$forecast$data$hhs6$x2$point
#> [1] 6.481
#> 
#> 
#> [[1]]$forecast$data$hhs6$x3
#> [[1]]$forecast$data$hhs6$x3$dist
#> [[1]]$forecast$data$hhs6$x3$dist[[1]]
#> [1] 0.000843
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[2]]
#> [1] 0.004469
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[3]]
#> [1] 0.018511
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[4]]
#> [1] 0.056139
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[5]]
#> [1] 0.123467
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[6]]
#> [1] 0.196609
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[7]]
#> [1] 0.226625
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[8]]
#> [1] 0.189082
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[9]]
#> [1] 0.114196
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[10]]
#> [1] 0.04994
#> 
#> [[1]]$forecast$data$hhs6$x3$dist[[11]]
#> [1] 0.020118
#> 
#> 
#> [[1]]$forecast$data$hhs6$x3$point
#> [1] 6.481
#> 
#> 
#> [[1]]$forecast$data$hhs6$x4
#> [[1]]$forecast$data$hhs6$x4$dist
#> [[1]]$forecast$data$hhs6$x4$dist[[1]]
#> [1] 0.003679
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[2]]
#> [1] 0.012878
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[3]]
#> [1] 0.035858
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[4]]
#> [1] 0.078626
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[5]]
#> [1] 0.135475
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[6]]
#> [1] 0.18333
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[7]]
#> [1] 0.194822
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[8]]
#> [1] 0.162579
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[9]]
#> [1] 0.106546
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[10]]
#> [1] 0.054849
#> 
#> [[1]]$forecast$data$hhs6$x4$dist[[11]]
#> [1] 0.031356
#> 
#> 
#> [[1]]$forecast$data$hhs6$x4$point
#> [1] 5.83978
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs7
#> [[1]]$forecast$data$hhs7$onset
#> [[1]]$forecast$data$hhs7$onset$dist
#> [[1]]$forecast$data$hhs7$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[9]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$onset$point
#> [1] 48
#> 
#> 
#> [[1]]$forecast$data$hhs7$peak
#> [[1]]$forecast$data$hhs7$peak$dist
#> [[1]]$forecast$data$hhs7$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[7]]
#> [1] 0.999091
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$peak$point
#> [1] 6.538
#> 
#> 
#> [[1]]$forecast$data$hhs7$peakweek
#> [[1]]$forecast$data$hhs7$peakweek$dist
#> [[1]]$forecast$data$hhs7$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[13]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs7$peakweek$point
#> [1] 52
#> 
#> 
#> [[1]]$forecast$data$hhs7$x1
#> [[1]]$forecast$data$hhs7$x1$dist
#> [[1]]$forecast$data$hhs7$x1$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[2]]
#> [1] 0.000104
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[3]]
#> [1] 0.003659
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[4]]
#> [1] 0.117105
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[5]]
#> [1] 0.514204
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[6]]
#> [1] 0.333217
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[7]]
#> [1] 0.030895
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[8]]
#> [1] 0.000451
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$x1$point
#> [1] 4.86797
#> 
#> 
#> [[1]]$forecast$data$hhs7$x2
#> [[1]]$forecast$data$hhs7$x2$dist
#> [[1]]$forecast$data$hhs7$x2$dist[[1]]
#> [1] 0.000122
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[2]]
#> [1] 0.002243
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[3]]
#> [1] 0.042668
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[4]]
#> [1] 0.247951
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[5]]
#> [1] 0.435037
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[6]]
#> [1] 0.232378
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[7]]
#> [1] 0.037447
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[8]]
#> [1] 0.001856
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[9]]
#> [1] 0.000115
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$x2$point
#> [1] 4.71607
#> 
#> 
#> [[1]]$forecast$data$hhs7$x3
#> [[1]]$forecast$data$hhs7$x3$dist
#> [[1]]$forecast$data$hhs7$x3$dist[[1]]
#> [1] 0.000158
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[2]]
#> [1] 0.003827
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[3]]
#> [1] 0.060307
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[4]]
#> [1] 0.289898
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[5]]
#> [1] 0.425797
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[6]]
#> [1] 0.192192
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[7]]
#> [1] 0.02638
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[8]]
#> [1] 0.001155
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[9]]
#> [1] 0.000103
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$x3$point
#> [1] 4.71607
#> 
#> 
#> [[1]]$forecast$data$hhs7$x4
#> [[1]]$forecast$data$hhs7$x4$dist
#> [[1]]$forecast$data$hhs7$x4$dist[[1]]
#> [1] 0.000465
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[2]]
#> [1] 0.013008
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[3]]
#> [1] 0.129955
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[4]]
#> [1] 0.390634
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[5]]
#> [1] 0.357538
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[6]]
#> [1] 0.099506
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[7]]
#> [1] 0.008331
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[8]]
#> [1] 0.000289
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[9]]
#> [1] 9.2e-05
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs7$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs7$x4$point
#> [1] 3.80664
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs8
#> [[1]]$forecast$data$hhs8$onset
#> [[1]]$forecast$data$hhs8$onset$dist
#> [[1]]$forecast$data$hhs8$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[10]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$onset$point
#> [1] 49
#> 
#> 
#> [[1]]$forecast$data$hhs8$peak
#> [[1]]$forecast$data$hhs8$peak$dist
#> [[1]]$forecast$data$hhs8$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[5]]
#> [1] 0.999091
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$peak$point
#> [1] 4.449
#> 
#> 
#> [[1]]$forecast$data$hhs8$peakweek
#> [[1]]$forecast$data$hhs8$peakweek$dist
#> [[1]]$forecast$data$hhs8$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[14]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs8$peakweek$point
#> [1] 53
#> 
#> 
#> [[1]]$forecast$data$hhs8$x1
#> [[1]]$forecast$data$hhs8$x1$dist
#> [[1]]$forecast$data$hhs8$x1$dist[[1]]
#> [1] 0.000122
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[2]]
#> [1] 0.0125
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[3]]
#> [1] 0.30124
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[4]]
#> [1] 0.584134
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[5]]
#> [1] 0.100241
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[6]]
#> [1] 0.001306
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[7]]
#> [1] 9.2e-05
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$x1$point
#> [1] 3.30456
#> 
#> 
#> [[1]]$forecast$data$hhs8$x2
#> [[1]]$forecast$data$hhs8$x2$dist
#> [[1]]$forecast$data$hhs8$x2$dist[[1]]
#> [1] 0.000444
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[2]]
#> [1] 0.045118
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[3]]
#> [1] 0.456115
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[4]]
#> [1] 0.45321
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[5]]
#> [1] 0.044226
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[6]]
#> [1] 0.000432
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$x2$point
#> [1] 3.06773
#> 
#> 
#> [[1]]$forecast$data$hhs8$x3
#> [[1]]$forecast$data$hhs8$x3$dist
#> [[1]]$forecast$data$hhs8$x3$dist[[1]]
#> [1] 0.00038
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[2]]
#> [1] 0.06721
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[3]]
#> [1] 0.60625
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[4]]
#> [1] 0.317294
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[5]]
#> [1] 0.008314
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[6]]
#> [1] 9.8e-05
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$x3$point
#> [1] 3.06773
#> 
#> 
#> [[1]]$forecast$data$hhs8$x4
#> [[1]]$forecast$data$hhs8$x4$dist
#> [[1]]$forecast$data$hhs8$x4$dist[[1]]
#> [1] 0.001436
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[2]]
#> [1] 0.149783
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[3]]
#> [1] 0.674095
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[4]]
#> [1] 0.172228
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[5]]
#> [1] 0.001912
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs8$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs8$x4$point
#> [1] 2.602
#> 
#> 
#> 
#> [[1]]$forecast$data$hhs9
#> [[1]]$forecast$data$hhs9$onset
#> [[1]]$forecast$data$hhs9$onset$dist
#> [[1]]$forecast$data$hhs9$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[12]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$onset$point
#> [1] 51
#> 
#> 
#> [[1]]$forecast$data$hhs9$peak
#> [[1]]$forecast$data$hhs9$peak$dist
#> [[1]]$forecast$data$hhs9$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[5]]
#> [1] 0.572685
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[6]]
#> [1] 0.42446
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[7]]
#> [1] 0.002128
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$peak$point
#> [1] 4.702
#> 
#> 
#> [[1]]$forecast$data$hhs9$peakweek
#> [[1]]$forecast$data$hhs9$peakweek$dist
#> [[1]]$forecast$data$hhs9$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[14]]
#> [1] 0.251052
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[16]]
#> [1] 0.289579
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[17]]
#> [1] 0.22157
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[18]]
#> [1] 0.135234
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[19]]
#> [1] 0.065842
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[20]]
#> [1] 0.025579
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[21]]
#> [1] 0.007939
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[22]]
#> [1] 0.001981
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[23]]
#> [1] 0.000413
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[24]]
#> [1] 8.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[25]]
#> [1] 3.6e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$hhs9$peakweek$point
#> [1] 53
#> 
#> 
#> [[1]]$forecast$data$hhs9$x1
#> [[1]]$forecast$data$hhs9$x1$dist
#> [[1]]$forecast$data$hhs9$x1$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[2]]
#> [1] 0.000139
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[3]]
#> [1] 0.023828
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[4]]
#> [1] 0.450986
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[5]]
#> [1] 0.492724
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[6]]
#> [1] 0.031697
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[7]]
#> [1] 0.000171
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$x1$point
#> [1] 3.958
#> 
#> 
#> [[1]]$forecast$data$hhs9$x2
#> [[1]]$forecast$data$hhs9$x2$dist
#> [[1]]$forecast$data$hhs9$x2$dist[[1]]
#> [1] 0.000119
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[2]]
#> [1] 0.002981
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[3]]
#> [1] 0.0662
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[4]]
#> [1] 0.348026
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[5]]
#> [1] 0.438913
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[6]]
#> [1] 0.133656
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[7]]
#> [1] 0.00959
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[8]]
#> [1] 0.000241
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$x2$point
#> [1] 4.055
#> 
#> 
#> [[1]]$forecast$data$hhs9$x3
#> [[1]]$forecast$data$hhs9$x3$dist
#> [[1]]$forecast$data$hhs9$x3$dist[[1]]
#> [1] 0.000205
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[2]]
#> [1] 0.006095
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[3]]
#> [1] 0.086272
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[4]]
#> [1] 0.348737
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[5]]
#> [1] 0.407087
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[6]]
#> [1] 0.137609
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[7]]
#> [1] 0.01328
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[8]]
#> [1] 0.000439
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[9]]
#> [1] 9.3e-05
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$x3$point
#> [1] 4.055
#> 
#> 
#> [[1]]$forecast$data$hhs9$x4
#> [[1]]$forecast$data$hhs9$x4$dist
#> [[1]]$forecast$data$hhs9$x4$dist[[1]]
#> [1] 0.000131
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[2]]
#> [1] 0.004162
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[3]]
#> [1] 0.085448
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[4]]
#> [1] 0.392373
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[5]]
#> [1] 0.412594
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[6]]
#> [1] 0.099508
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[7]]
#> [1] 0.005362
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[8]]
#> [1] 0.000149
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$hhs9$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$hhs9$x4$point
#> [1] 3.871
#> 
#> 
#> 
#> [[1]]$forecast$data$nat
#> [[1]]$forecast$data$nat$onset
#> [[1]]$forecast$data$nat$onset$dist
#> [[1]]$forecast$data$nat$onset$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[8]]
#> [1] 0.999029
#> 
#> [[1]]$forecast$data$nat$onset$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[13]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[16]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[17]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[18]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[19]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[20]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[21]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[22]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[23]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[24]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[25]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[26]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[27]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[28]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[29]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[30]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[31]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$nat$onset$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$onset$point
#> [1] 47
#> 
#> 
#> [[1]]$forecast$data$nat$peak
#> [[1]]$forecast$data$nat$peak$dist
#> [[1]]$forecast$data$nat$peak$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[3]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[4]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[5]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[6]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[7]]
#> [1] 0.999091
#> 
#> [[1]]$forecast$data$nat$peak$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$peak$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$nat$peak$point
#> [1] 6.08
#> 
#> 
#> [[1]]$forecast$data$nat$peakweek
#> [[1]]$forecast$data$nat$peakweek$dist
#> [[1]]$forecast$data$nat$peakweek$dist[[1]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[2]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[3]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[4]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[5]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[6]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[7]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[8]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[9]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[10]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[11]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[12]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[13]]
#> [1] 0.266381
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[14]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[15]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[16]]
#> [1] 0.222786
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[17]]
#> [1] 0.177498
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[18]]
#> [1] 0.130045
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[19]]
#> [1] 0.087619
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[20]]
#> [1] 0.05429
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[21]]
#> [1] 0.030939
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[22]]
#> [1] 0.016221
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[23]]
#> [1] 0.007828
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[24]]
#> [1] 0.003483
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[25]]
#> [1] 0.001436
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[26]]
#> [1] 0.000556
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[27]]
#> [1] 0.00021
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[28]]
#> [1] 8.6e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[29]]
#> [1] 4.5e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[30]]
#> [1] 3.3e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[31]]
#> [1] 3e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[32]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[33]]
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$dist[[34]]
#> [1] 2.9e-05
#> 
#> 
#> [[1]]$forecast$data$nat$peakweek$none
#> [1] 2.9e-05
#> 
#> [[1]]$forecast$data$nat$peakweek$point
#> [1] 52
#> 
#> 
#> [[1]]$forecast$data$nat$x1
#> [[1]]$forecast$data$nat$x1$dist
#> [[1]]$forecast$data$nat$x1$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x1$dist[[2]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x1$dist[[3]]
#> [1] 0.005016
#> 
#> [[1]]$forecast$data$nat$x1$dist[[4]]
#> [1] 0.448752
#> 
#> [[1]]$forecast$data$nat$x1$dist[[5]]
#> [1] 0.536121
#> 
#> [[1]]$forecast$data$nat$x1$dist[[6]]
#> [1] 0.009474
#> 
#> [[1]]$forecast$data$nat$x1$dist[[7]]
#> [1] 9.2e-05
#> 
#> [[1]]$forecast$data$nat$x1$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x1$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x1$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x1$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$nat$x1$point
#> [1] 4.05598
#> 
#> 
#> [[1]]$forecast$data$nat$x2
#> [[1]]$forecast$data$nat$x2$dist
#> [[1]]$forecast$data$nat$x2$dist[[1]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x2$dist[[2]]
#> [1] 1e-04
#> 
#> [[1]]$forecast$data$nat$x2$dist[[3]]
#> [1] 0.027235
#> 
#> [[1]]$forecast$data$nat$x2$dist[[4]]
#> [1] 0.639516
#> 
#> [[1]]$forecast$data$nat$x2$dist[[5]]
#> [1] 0.329868
#> 
#> [[1]]$forecast$data$nat$x2$dist[[6]]
#> [1] 0.002735
#> 
#> [[1]]$forecast$data$nat$x2$dist[[7]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x2$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x2$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x2$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x2$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$nat$x2$point
#> [1] 3.864
#> 
#> 
#> [[1]]$forecast$data$nat$x3
#> [[1]]$forecast$data$nat$x3$dist
#> [[1]]$forecast$data$nat$x3$dist[[1]]
#> [1] 9.2e-05
#> 
#> [[1]]$forecast$data$nat$x3$dist[[2]]
#> [1] 0.001238
#> 
#> [[1]]$forecast$data$nat$x3$dist[[3]]
#> [1] 0.097953
#> 
#> [[1]]$forecast$data$nat$x3$dist[[4]]
#> [1] 0.583159
#> 
#> [[1]]$forecast$data$nat$x3$dist[[5]]
#> [1] 0.304399
#> 
#> [[1]]$forecast$data$nat$x3$dist[[6]]
#> [1] 0.012673
#> 
#> [[1]]$forecast$data$nat$x3$dist[[7]]
#> [1] 0.000123
#> 
#> [[1]]$forecast$data$nat$x3$dist[[8]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x3$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x3$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x3$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$nat$x3$point
#> [1] 3.864
#> 
#> 
#> [[1]]$forecast$data$nat$x4
#> [[1]]$forecast$data$nat$x4$dist
#> [[1]]$forecast$data$nat$x4$dist[[1]]
#> [1] 0.000862
#> 
#> [[1]]$forecast$data$nat$x4$dist[[2]]
#> [1] 0.022667
#> 
#> [[1]]$forecast$data$nat$x4$dist[[3]]
#> [1] 0.184564
#> 
#> [[1]]$forecast$data$nat$x4$dist[[4]]
#> [1] 0.432946
#> 
#> [[1]]$forecast$data$nat$x4$dist[[5]]
#> [1] 0.296359
#> 
#> [[1]]$forecast$data$nat$x4$dist[[6]]
#> [1] 0.058811
#> 
#> [[1]]$forecast$data$nat$x4$dist[[7]]
#> [1] 0.003376
#> 
#> [[1]]$forecast$data$nat$x4$dist[[8]]
#> [1] 0.000141
#> 
#> [[1]]$forecast$data$nat$x4$dist[[9]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x4$dist[[10]]
#> [1] 9.1e-05
#> 
#> [[1]]$forecast$data$nat$x4$dist[[11]]
#> [1] 9.1e-05
#> 
#> 
#> [[1]]$forecast$data$nat$x4$point
#> [1] 3.476
#> 
#> 
#> 
#> 
#> [[1]]$forecast$epiweek
#> [1] 201501
#> 
#> [[1]]$forecast$ili_bin_size
#> [1] 1
#> 
#> [[1]]$forecast$ili_bins
#> [1] 11
#> 
#> [[1]]$forecast$name
#> [1] "DELPHI-Epicast-(Carnegie-Mellon-University)"
#> 
#> [[1]]$forecast$season
#> [1] 2014
#> 
#> [[1]]$forecast$season_weeks
#> [1] 34
#> 
#> [[1]]$forecast$year_weeks
#> [1] 53
#> 
#> 
#> [[1]]$system
#> [1] "ec"
#> 
#> 
```
