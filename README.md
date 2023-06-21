
# aldvmm

The goal of 'aldvmm' is to fit adjusted limited dependent variable
mixture models of health state utilities. Adjusted limited dependent
variable mixture models are finite mixtures of normal distributions with
an accumulation of density mass at the limits, and a gap between 100%
quality of life and the next smaller utility value. The package 'aldvmm'
uses the likelihood and expected value functions proposed by Hernandez
Alava and Wailoo (2015) using normal component distributions and a
multinomial logit model of probabilities of component membership.

## Installation

You can install the released version of aldvmm from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("aldvmm")
```

<!-- You can install the development version from [GitHub](https://github.com/) with: -->
<!-- ``` r -->
<!-- # install.packages("devtools") -->
<!-- devtools::install_github("pletschm/aldvmm", ref = "main") -->
<!-- ``` -->

## Example

The package 'aldvmm' includes a simulated sample data set to run simple
examples.

``` r
library("aldvmm")

data(utility)

fit <- aldvmm(eq5d ~ age + female | 1,
              data = utility,
              psi = c(0.883, -0.594),
              ncmp = 2,
              init.method = "constant",
              optim.method = "Nelder-Mead")

summary(fit)

yhat <- predict(fit,
                newdata = utility)
```

## Vignette

Please consult the
[html](https://htmlpreview.github.io/?https://github.com/pletschm/aldvmm/blob/main/vignettes/html_vignette.html) or [pdf](https://github.com/pletschm/aldvmm/blob/main/vignettes/pdf_vignette.pdf) vignettes for further details on the methods and the usage of the package.

## References

Hernández Alava, Mónica, and Allan Wailoo. 2015. “Fitting Adjusted
Limited Dependent Variable Mixture Models to EQ-5D.” The Stata Journal
15 (3): 737–50. doi: 10.1177/1536867X1501500307
