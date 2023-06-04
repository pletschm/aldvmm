## Re-submission
This is a re-submission. 

It includes the following updates.
* New methods for generic functions print(), summary(), stats::predict(), stats::coef(), stats::nobs(), stats::vcov(), stats::model.matrix() and sandwich::estfun() are available.
* New workflow using the function Formula::formula() to handle models with two right-hand sides.
* Default optimzation method was changed to "BFGS".
* Objects of class "aldvmm" include new elements.

Maintainer: 'Mark Pletscher <pletscher.mark@gmail.com>'

## Test environments
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* rhub (ubuntu-gcc-release)
* rhub (debian-gcc-devel)
* github windows-latest (release)
* github macOS-latest (release)
* github ubuntu-20.04 (release)
* github ubuntu-20.04 (devel)

## R CMD check results
Status: OK

## Downstream dependencies
There are currently no downstream dependencies for this package