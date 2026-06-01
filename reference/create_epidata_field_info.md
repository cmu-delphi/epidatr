# Add metadata to an epidata_field

This function annotates the type of the returned API field. This is used
by `parse_value` downstream to determine how to convert the returned
data.

## Usage

``` r
create_epidata_field_info(name, type, description = "", categories = c())
```

## Arguments

- name:

  The name of the field.

- type:

  The type of the field ("text", "int", "float", etc.).

- description:

  A description of the field's content.

- categories:

  Categories for the field, if applicable.
