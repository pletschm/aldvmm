
# aldvmm

The goal of ‘aldvmm’ is to fit adjusted limited dependent variable
mixture models of health state utilities. Adjusted limited dependent
variable mixture models are finite mixtures of normal distributions with
an accumulation of density mass at the limits, and a gap between 100%
quality of life and the next smaller utility value. The package ‘aldvmm’
uses the likelihood and expected value functions proposed by Hernandez
Alava and Wailoo (2015) using normal component distributions and a
multinomial logit model of probabilities of component membership.

## Installation

<!-- You can install the released version of aldvmm from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("aldvmm") -->
<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pletschm/aldvmm", ref = "main")
```

## Example

The package ‘aldvmm’ includes a simulated sample data set to run simple
examples.

``` r
library("aldvmm")

data(utility)

fit <- aldvmm(data = utility,
              formula = eq5d ~ age + female | 1,
              psi = c(0.883, -0.594),
              ncmp = 2,
              init.method = "zero",
              optim.method = "Nelder-Mead")

summary(fit)
#>                                                                         
#>  --------- ----------- ----------- ----------- ------- ----- -----------
#>                           Estimate   Std. Err.       z P>|z| [95% Conf. 
#>  --------- ----------- ----------- ----------- ------- ----- -----------
#>  E[Y|X, c]                                                              
#>  --------- ----------- ----------- ----------- ------- ----- -----------
#>      Comp1 (Intercept)       0.077       0.184   0.417 0.338      -0.284
#>                    age       0.002       0.001   1.186 0.118      -0.001
#>                 female       0.456       0.180   2.537 0.006       0.104
#>                lnsigma      -1.559       0.350  -4.459 1.000      -2.244
#>      Comp2 (Intercept)       0.448       0.073   6.116 0.000       0.305
#>                    age       0.001       0.001   0.768 0.221      -0.002
#>                 female      -0.603       0.051 -11.763 1.000      -0.704
#>                lnsigma      -2.075       0.219  -9.491 1.000      -2.504
#>  --------- ----------- ----------- ----------- ------- ----- -----------
#>     P[c|X]                                                              
#>  --------- ----------- ----------- ----------- ------- ----- -----------
#>      Comp1 (Intercept)       0.744       0.632   1.177 0.120      -0.495
#>  --------- ----------- ----------- ----------- ------- ----- -----------
#>    N = 200 ll = -31.38 AIC = 80.76 BIC = 80.76                          
#>           
#>  ---------
#>  Interval]
#>  ---------
#>           
#>  ---------
#>      0.437
#>      0.004
#>      0.809
#>     -0.874
#>      0.592
#>      0.004
#>     -0.503
#>     -1.647
#>  ---------
#>           
#>  ---------
#>      1.984
#>  ---------
#> 
yhat <- predict(fit,
                newdata = utility)
```

## Vignette

Please consult the [vignette](vignettes/aldvmm_vignette.pdf) for further
details on the methods and the usage of the package.

## References

Hernández Alava, Mónica, and Allan Wailoo. 2015. “Fitting Adjusted
Limited Dependent Variable Mixture Models to EQ-5D.” The Stata Journal
15 (3): 737–50. doi: 10.1177/1536867X1501500307
