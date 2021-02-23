# Important instructions
#-----------------------

# https://github.com/ThinkR-open/prepare-for-cran
# https://cran.r-project.org/web/packages/submission_checklist.html

# Issues
#-------

# When knitr::rmarkdown (vignette.Rmd) is used for making vignette with engine knitr 
# (DESCRIPTION) check throws an error. If knit is used in both, html is created

# Spell check
#------------

spelling::spell_check_package()

# Register qpdf and ghostscript
#------------------------------

Sys.setenv('PATH' = paste0('C:/Users/phm1/qpdf-10.1.0/bin;', Sys.getenv('PATH')))
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.53.3/bin/gswin64c.exe")

# Render vignette
#----------------

# Local check
#------------

devtools::check(pkg = ".", cran = TRUE)

# Build package
#--------------

devtools::build()


# Winbuilder
#------------

# upload via webpage https://win-builder.r-project.org/upload.aspx

# rhub checks
#------------

rhub::check("../aldvmm_0.0.0.9000.tar.gz", platform = "ubuntu-gcc-release")
rhub::check("../aldvmm_0.0.0.9000.tar.gz", platform = "debian-gcc-devel")
