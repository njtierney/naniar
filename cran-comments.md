## Test environments
* local OS X install, R 4.2.2
* github actions testing for devel, release, and ubuntu, windows, and macOSX

## R CMD check results
0 errors | 0 warnings | 1 note

There were no ERRORs or WARNINGs, and one NOTE:

```
Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v105.i07
    From: DESCRIPTION
          inst/CITATION
    Status: 404
    Message: Not Found
```

The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.

## Reverse dependencies

We checked 3 reverse dependencies (2 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

