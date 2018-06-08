## Test environments
* local OS X install, R 3.5.0
* ubuntu 12.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 1 note

There were no ERRORs or WARNINGs and one NOTE:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Nicholas Tierney <nicholas.tierney@gmail.com>'

Days since last update: 1
```

`naniar` has been resubmitted again soon after recent with a patch to fix the
issues identified from a CRAN email below:

```
Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_naniar.html>.

Please correct before 2018-06-21 to safely retain your package on CRAN.

Best,
-k
```

The fix removed importing the `tidyselect` package, as it is no longer used.

## Reverse dependencies

There are no reverse dependencies.

