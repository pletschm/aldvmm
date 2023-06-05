# Setup github actions in main branch
#------------------------------------

# Replace the check action from usethis::use_github_action("check-standard")

#- uses: r-lib/actions/check-r-package@v2
#  with:
#    upload-snapshots: true
#    error-on: '"error"'

# with

# - name: Check
# env:
#   _R_CHECK_CRAN_INCOMING_REMOTE_: false
# run: |
#   options(crayon.enabled = TRUE)
# rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), build_args = c("--compact-vignettes=both"), error_on = "warning", check_dir = "check")
# shell: Rscript {0}
# 
# - name: Upload check results
# if: failure()
# uses: actions/upload-artifact@main
# with:
#   name: ${{ runner.os }}-r${{ matrix.config.r }}-results
# path: check

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
