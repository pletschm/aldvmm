# Set up github actions
#----------------------

# See https://github.com/r-lib/actions/blob/v2/examples/README.md to pick the 
# latest .yaml file.

# run usethis::use_github_action("check-standard") in the main branch.

# Add line "- uses: r-lib/actions/setup-tinytex@v2" to the .yaml file.
# Add line "error-on: '"error"'" below to prevent fail on warnings.
# "- uses: r-lib/actions/check-r-package@v2".
#    with:
#      upload-snapshots: true
#      error-on: '"error"'

# Commit and push to the main branch.

# Change version
#---------------

# check pdf manual
#-----------------

devtools::build_manual(pkg = ".", path = NULL)

# Spell check
#------------

spelling::spell_check_package()

# Test coverage
#--------------

#usethis::use_testthat()
detach("package:aldvmm", unload = TRUE)
ct <- covr::package_coverage()
covr::report(ct)
library("aldvmm")

# Register qpdf and ghostscript
#------------------------------

Sys.setenv('PATH' = paste0('C:/Users/phm1/qpdf-11.1.1/bin;', Sys.getenv('PATH')))
Sys.setenv(R_GSCMD = "C:/Users/phm1/gs10.00.0/bin/gswin64c.exe")

# Save pdf vignette in vignettes folder
#--------------------------------------

# Local check
#------------

devtools::check(pkg = ".", cran = TRUE)

# Build package
#--------------

devtools::build(args = c('--compact-vignettes=both'))

# Source check in shell
#----------------------

# R CMD check --as-cran ../aldvmm_0.8.6.tar.gz

# Winbuilder
#------------

# upload via webpage https://win-builder.r-project.org/upload.aspx

# Note about possibly invalid DOI in DESCRIPTION is fine.

# rhub checks
#------------

rhub::check("../aldvmm_0.8.6.tar.gz", platform = "ubuntu-gcc-release")
rhub::check("../aldvmm_0.8.6.tar.gz", platform = "debian-gcc-devel")
