## Test environments
* local OS X install, R 4.0.5
* github actions testing for devel, release, and ubuntu, windows, and macOSX

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

