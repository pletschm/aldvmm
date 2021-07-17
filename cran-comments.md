## Re-submission
This is a re-submission. 

It includes two bugfixes and an update of the vignette
* Bugfix in summary.aldvmm(): AIC was displayed instead of BIC in summary table.
* Bugfix in predict.aldvmm(): Fitted values from aldvmm object were supplied instead of predictions from predict.aldvmm().
* New unit tests for predict.aldvmm().
* Updated vignette: Added example code for calculation of standard errors of average treatment effects on the treated.

Maintainer: 'Mark Pletscher <pletscher.mark@gmail.com>'

## Test environments
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* local Windows 10 64 bit (R 3.6.3)
* rhub (ubuntu-gcc-release)
* rhub (debian-gcc-devel)
* github windows-latest (release)
* github macOS-latest (release)
* github ubuntu-20.04 (release)
* github ubuntu-20.04 (devel)

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new release.

## Downstream dependencies
There are currently no downstream dependencies for this package