## Re-submission
This is a re-submission. 

It includes the following updates.
* The functions aldvmm.ll() and aldvmm.sc() were vectorized to increase readability and computation speed.
* The functions aldvmm.ll() and aldvmm.sc() include a numerically stabilized multinomial logit density to improve convergence.
* The pdf vignette was removed.
* The html vignette was updated to discuss the numerical properties and results of the stabilized likelihood and gradient functions.

Maintainer: 'Mark Pletscher <pletscher.mark@gmail.com>'

## Test environments
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* rhub (linux)
* rhub (m1-san)
* rhub (macos)
* rhub (windows)
* rhub (ubuntu-next)
* github windows-latest (release)
* github macOS-latest (release)
* github ubuntu-latest (release)
* github ubuntu-latest (devel)
* github ubuntu-latest (oldrel-1)

## R CMD check results
Status: OK

## Downstream dependencies
There are currently no downstream dependencies for this package