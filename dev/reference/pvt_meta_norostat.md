# Metadata for the NoroSTAT endpoint

**\[deprecated\]**

This endpoint was previously deprecated and remains available for
historical reference. The underlying data source is no longer updated
and no new data is being ingested. For more details, refer to the
"Endpoints kept for historical reference" section of
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/dev/articles/migration-guide.md),
and visit the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html).

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html>

## Usage

``` r
pvt_meta_norostat(auth, fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Your restricted access key (not the same as API key).

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`list`](https://rdrr.io/r/base/list.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## See also

[`pvt_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_norostat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_meta_norostat(auth = Sys.getenv("DELPHI_EPIDATA_KEY"))
} # }
```
