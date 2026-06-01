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
                  "attributes": {},
                  "value": []
                }
              ]
            }
          ]
        }
      ]
    }

