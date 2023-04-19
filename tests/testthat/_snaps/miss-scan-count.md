# miss_scan_count returns an error when no search is provided

    Code
      miss_scan_count(dat_ms)
    Error <simpleError>
      argument "search" is missing, with no default

# miss_scan_count returns an error when no data is provided

    Code
      miss_scan_count(search = -99)
    Error <simpleError>
      argument "data" is missing, with no default

