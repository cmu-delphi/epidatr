# Get and set API keys

Get and set the API key used to make requests to the Epidata API.
Without a key, requests may be subject to rate limits and other
limitations.

## Usage

``` r
get_api_key()

save_api_key()
```

## Value

For `get_api_key()`, returns the current API key as a string, or `""` if
none is set.

## Details

We recommend you register for an API key. While most endpoints are
available without one, there are [limits on API usage for anonymous
users](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html),
including a rate limit. If you regularly request large amounts of data,
please consider [registering for an API
key](https://api.delphi.cmu.edu/epidata/admin/registration_form).

API keys are strings read from the environment variable
`DELPHI_EPIDATA_KEY`. We recommend setting your key with
`save_api_key()`, which will modify an applicable `.Renviron` file,
which will be read in automatically when you start future R sessions
(see [`?Startup`](https://rdrr.io/r/base/Startup.html) for details on
`.Renviron` files). Alternatively, you can modify the environment
variable at the command line before/while launching R, or inside an R
session with [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html),
but these will not persist across sessions.

Once an API key is set, it is automatically used for all requests made
by functions in this package.

## References

- [Delphi Epidata API Keys
  documentation](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html).

- [Delphi Epidata API Registration
  Form](https://api.delphi.cmu.edu/epidata/admin/registration_form).
