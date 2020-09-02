## Test environments
* local OS X install, R 4.0.2
* ubuntu 12.04 (on GitHub Actions), R 4.0.2
* Could not run win-builder (devel and release) due to an error with winbuilder.
  This has been discussed with Uwe Ligges, Kurt Hornik, and Jeroen Ooms and this
  is an issue with winbuilder, and I have been encouraged to submit to CRAN.

## R CMD check results
0 errors | 0 warnings | 1 note

There were no ERRORs or WARNINGs, and one NOTE:

```
checking for future file timestamps ... NOTE
  unable to verify current time
```

My understanding of this note is that a website whose API was used to check
time is currently down, and the NOTE is unrelated to my package.

## Reverse dependencies

There are four reverse dependencies, these have been checked.

