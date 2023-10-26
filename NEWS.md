# aldvmm 0.8.3

* Initial release on cran.

# aldvmm 0.8.4

* Bugfix in summary.aldvmm(): AIC was displayed instead of BIC in summary table.
* Bugfix in predict.aldvmm(): Fitted values from aldvmm object were supplied instead of predictions from predict.aldvmm().
* Updated vignette: Added example code for calculation of standard errors of average treatment effects on the treated.

# aldvmm 0.8.5

* Update of validate_aldvmm(): Checking for class type of model formula using base::inherits() instead of if(class(obj) == "formula").
* Update of vignette: Include figures as .eps files to avoid loading ggplot objects from previous versions of ggplot2

# aldvmm 0.8.6
* Default optimization method was changed to "BFGS".
* New methods for generic functions print(), summary(), stats::predict(), stats::coef(), stats::nobs(), stats::vcov(), stats::model.matrix() and sandwich::estfun() are available. Objects of class "aldvmm" can now be supplied to sandwich::sandwich(), sandwich::vcovCL(), lmtest::coeftest(), lmtest::coefci() and other functions.
* New workflow using the function Formula::formula() to handle models with two right-hand sides.
* Objects of class "aldvmm" include new elements:
  * n: The number of complete observations.
  * df.null: Degrees of freedom of null model.
  * df.residual: Degrees of freedom of fitted model.
  * iter: The number of iterations during optimization.
  * convergence: An indicator of successful completion of optimization.
  * call: A character value of the model call.
  * terms: A list of terms objects for the models.
  * data: A data frame of the estimation data.
  * contrasts: A nested list of character values of contrasts.
  * na.action: An object indicating the na.action used in stats::model.frame()
  
# aldvmm 0.8.7
* The package "aldvmm" now uses analytical gradients instead of numerical approximations during optimization and in methods used for estimators from the "sandwich" package.
* New methods for generic functions stats::formula(), stats::residuals() and stats::update(). Objects of class "aldvmm" can now be supplied to sandwich::sandwich(), sandwich::vcovCL(), sandwich::vcovPL(), sandwich::vcovHAC() and sandwich::vcovBS(). sandwich::vcovBS() allows re-estimating the covariance matrix using bootstrapping with and without clustering.
* Objects of class "aldvmm" now include predicted probabilities of component membership for all observations in the estimation data.
* New html vignette.

# aldvmm 0.8.8
* The optimizer package was changed from "optimr" to "optimx". The functionality remains identical.