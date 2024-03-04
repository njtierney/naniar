## Test environments
* local OS X install, R 4.2.2
* github actions testing for devel, release, and ubuntu, windows, and macOSX

## R CMD check results
0 errors | 0 warnings | 1 note

There were no ERRORs or WARNINGs, and one NOTE:

```
Found the following (possibly) invalid URLs:
  URL: https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily
    From: inst/doc/special-missing-values.html
    Status: Error
    Message: Empty reply from server
```

Navigating to the website works on my machine, but I note that the NCEI website states in a banner:

> Please note: Due to a system outage, many NCEI systems are currently unavailable. We are working to resolve these issues as soon as possible. We apologize for any inconvenience.

Which makes me think that perhaps this is an issue related to that?

## Reverse dependencies

We checked 6 reverse dependencies (5 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

