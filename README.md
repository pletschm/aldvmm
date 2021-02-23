
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
#>  --------- ----------- ----------- ----------- -------- -------- -----------
#>                           Estimate   Std. Err.        z    P>|z| [95% Conf. 
#>  --------- ----------- ----------- ----------- -------- -------- -----------
#>  E[y|c, X]                                                                  
#>  --------- ----------- ----------- ----------- -------- -------- -----------
#>      Comp1 (Intercept)      0.0768      0.1840   0.4173   0.3382     -0.2838
#>                    age      0.0017      0.0014   1.1859   0.1178     -0.0011
#>                 female      0.4562      0.1799   2.5366   0.0056      0.1037
#>                lnsigma     -1.5587      0.3496  -4.4586   1.0000     -2.2439
#>      Comp2 (Intercept)      0.4485      0.0733   6.1162   0.0000      0.3048
#>                    age      0.0011      0.0014   0.7680   0.2213     -0.0017
#>                 female     -0.6030      0.0513 -11.7632   1.0000     -0.7035
#>                lnsigma     -2.0753      0.2187  -9.4910   1.0000     -2.5038
#>  --------- ----------- ----------- ----------- -------- -------- -----------
#>     P[c|X]                                                                  
#>  --------- ----------- ----------- ----------- -------- -------- -----------
#>      Comp1 (Intercept)      0.7444      0.6322   1.1774   0.1195     -0.4948
#>  --------- ----------- ----------- ----------- -------- -------- -----------
#>    N = 200 ll = -31.38 AIC = 80.76 BIC = 80.76                              
#>           
#>  ---------
#>  Interval]
#>  ---------
#>           
#>  ---------
#>     0.4373
#>     0.0044
#>     0.8088
#>    -0.8735
#>     0.5922
#>     0.0039
#>    -0.5026
#>    -1.6467
#>  ---------
#>           
#>  ---------
#>     1.9835
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
