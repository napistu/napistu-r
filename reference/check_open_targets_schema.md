# Check Open Targets Schema Compatibility

Runs a GraphQL introspection query and compares the fields we actually
use against what the API currently exposes. Call this when queries start
failing with unexpected errors — it will identify removed or renamed
fields before you have to dig through cryptic HTTP 400 messages.

## Usage

``` r
check_open_targets_schema()
```

## Value

invisibly, a named list of logical vectors (one per type) indicating
which expected fields are present

## Details

The expected fields are defined in \[OT_EXPECTED_FIELDS\] and correspond
directly to the sections of \[build_targets_query()\].
