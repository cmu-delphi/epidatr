# fetch_args

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["fields", "disable_date_parsing", "disable_data_frame_parsing", "return_empty", "timeout_seconds", "base_url", "dry_run", "refresh_cache", "reference_week_day"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["fetch_args"]
        }
      },
      "value": [
        {
          "type": "NULL"
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [900]
        },
        {
          "type": "NULL"
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["fields", "disable_date_parsing", "disable_data_frame_parsing", "return_empty", "timeout_seconds", "base_url", "dry_run", "refresh_cache", "reference_week_day"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["fetch_args"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["a", "b"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [10]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["https://example.com"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1]
        }
      ]
    }

# fetch non-classic passes along api warnings

    Code
      epidata_call %>% fetch()
    Condition
      Warning:
      epidata warning: `* This is a warning with a leading asterisk and {braces} to make sure we don't have bulleting/glue bugs.`
    Output
      # A tibble: 1 x 15
        source   signal geo_type time_type geo_value time_value issue        lag value
        <chr>    <chr>  <fct>    <fct>     <chr>     <date>     <date>     <dbl> <dbl>
      1 jhu-csse confi~ state    day       ca        2020-06-01 2020-06-02     1   1.5
      # i 6 more variables: stderr <dbl>, sample_size <dbl>, direction <dbl>,
      #   missing_value <dbl>, missing_stderr <dbl>, missing_sample_size <dbl>

# fetch classic works

    {
      "type": "list",
      "attributes": {},
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["epiweek", "forecast"]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [201501]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["_version", "baselines", "data"]
                }
              },
              "value": [
                {
                  "type": "integer",
                  "attributes": {},
                  "value": [1]
                },
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["nat"]
                    }
                  },
                  "value": [
                    {
                      "type": "integer",
                      "attributes": {},
                      "value": [2]
                    }
                  ]
                },
                {
                  "type": "list",
    
                  "value": []
                }
              ]
            }
          ]
        }
      ]
    }

