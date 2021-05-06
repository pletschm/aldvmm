# Spell check
#------------

spelling::spell_check_package()

# Test coverage
#--------------

#usethis::use_testthat()
detach("package:aldvmm", unload = TRUE)
ct <- covr::package_coverage()
covr::report(ct)

# Register qpdf and ghostscript
#------------------------------

Sys.setenv('PATH' = paste0('C:/Users/phm1/qpdf-10.1.0/bin;', Sys.getenv('PATH')))
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.53.3/bin/gswin64c.exe")

# Save pdf vignette in vignettes folder
#--------------------------------------

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

rhub::check("../aldvmm_0.8.0.9000.tar.gz", platform = "ubuntu-gcc-release")
rhub::check("../aldvmm_0.8.0.9000.tar.gz", platform = "debian-gcc-devel")
