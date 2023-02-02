## Test environments
* local OS X install, R 4.2.2
* github actions testing for devel, release, and ubuntu, windows, and macOSX

## R CMD check results
0 errors | 0 warnings | 1 note

There were no ERRORs or WARNINGs, and one NOTE:

```
Found the following (possibly) invalid URLs:
  URL: https://web.archive.org/web/20201120030409/https://stats-bayes.com/post/2020/08/14/r-function-for-little-s-test-for-data-missing-completely-at-random/
    From: man/mcar_test.Rd
    Status: Error
    Message: Failed to connect to web.archive.org port 443 after 21108 ms: Timed out
  URL: https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily
    From: inst/doc/special-missing-values.html
    Status: Error
    Message: Empty reply from server
  URL: https://www.researchgate.net/publication/2758672_Missing_Data_in_Interactive_High-Dimensional_Data_Visualization
    From: man/as_shadow.Rd
          inst/doc/getting-started-w-naniar.html
          inst/doc/special-missing-values.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v105.i07
    From: DESCRIPTION
          inst/CITATION
    Status: 404
    Message: Not Found
```

The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.

The other links all work locally for me on my machine, and checking with `wget` locally they also work. I'm not sure how to best proceed with these?

## Reverse dependencies

We checked 3 reverse dependencies (2 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

