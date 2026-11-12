# Split comma-joined values (`"sig1, sig2"`) into a clean character vector.

Trims whitespace, drops empty items (such as from a trailing comma) and
duplicates, and aborts if nothing is left.

## Usage

``` r
split_comma_param(name, value)
```

## Arguments

- name:

  argument name for error messages.
