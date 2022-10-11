## Re-submission
This is a re-submission. 

It includes two updates
* Update in validate_aldvmm(): Checking for class type of model formula using base::inherits() instead of if(class(obj) == "formula").
* Update in vignette: Include figures as .eps files to avoid loading ggplot objects from previous versions of ggplot2.

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