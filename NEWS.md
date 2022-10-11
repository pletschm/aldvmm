# aldvmm 0.8.3

* Initial release on cran.

# aldvmm 0.8.4

* Bugfix in summary.aldvmm(): AIC was displayed instead of BIC in summary table.
* Bugfix in predict.aldvmm(): Fitted values from aldvmm object were supplied instead of predictions from predict.aldvmm().
* Updated vignette: Added example code for calculation of standard errors of average treatment effects on the treated.

# aldvmm 0.8.5

* Update of validate_aldvmm(): Checking for class type of model formula using base::inherits() instead of if(class(obj) == "formula").
* Update of vignette: Include figures as .eps files to avoid loading ggplot objects from previous versions of ggplot2