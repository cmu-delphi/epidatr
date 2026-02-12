# parse_data_frame warns when df contains fields not listed in meta

    Code
      parse_data_frame(epidata_call, mock_df_extra)
    Condition
      Warning:
      Not all return columns are specified as expected epidata fields
      i Unspecified fields extra may need to be manually converted to more appropriate classes
    Output
        val extra
      1   1     5

# parse_data_frame warns when df contains int values with decimal component

    Code
      parse_data_frame(epidata_call, mock_df)
    Condition
      Warning:
      Values in val were expected to be integers but contain a decimal component
      i Decimal components are returned as-is
    Output
        val
      1 4.3

