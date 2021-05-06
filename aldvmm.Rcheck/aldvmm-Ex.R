pkgname <- "aldvmm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "aldvmm-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('aldvmm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aldvmm-package")
### * aldvmm-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aldvmm-package
### Title: aldvmm: Adjusted Limited Dependent Variable Mixture Models
### Aliases: aldvmm-package

### ** Examples

data(utility)

 fit <- aldvmm(data = utility,
               formula = eq5d ~ age + female | 1,
               psi = c(0.883, -0.594),
               ncmp = 2)

 summary(fit)

 yhat <- predict(fit,
                 newdata = utility)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aldvmm-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aldvmm")
### * aldvmm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aldvmm
### Title: Fitting Adjusted Limited Dependent Variable Mixture Models
### Aliases: aldvmm

### ** Examples

data(utility)

 fit <- aldvmm(eq5d ~ age + female | 1,
               data = utility,
               psi = c(0.883, -0.594),
               ncmp = 2)

 summary(fit)

 yhat <- predict(fit,
                 newdata = utility)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aldvmm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("utility")
### * utility

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: utility
### Title: Simulated Example Data of Health State Utilities.
### Aliases: utility
### Keywords: datasets

### ** Examples

set.seed(101010101)
utility                 <- data.frame(female = rbinom(size = 1,
                                      n      = 200,
                                      p      = 0.6))
utility[, 'age']        <- stats::rnorm(n    = 200,
                                        mean = 50 + utility$female*10,
                                        sd   = 15)
utility[1:50, 'eq5d']   <- stats::rnorm(n    = 50,
                                        mean = 0 - 0.1 *
                                              utility[1:50, 'female'] +
                                              0.0005 * utility[1:50, 'age'],
                                        sd   = 0.1)
utility[51:200, 'eq5d'] <- stats::rnorm(n    = 150,
                                        mean = 0.5 +
                                           0.1 * utility[51:200, 'female'] +
                                           0.0001*utility[51:200, 'age'],
                                        sd   = 0.2)
utility[utility$eq5d<(-0.594), 'eq5d'] <- -0.594
utility[utility$eq5d>0.883, 'eq5d'] <- 1
hist(utility$eq5d, breaks = 50)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("utility", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
