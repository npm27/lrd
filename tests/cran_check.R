# Prepare for CRAN ----

# rebuild any time you update
library(roxygen2)
roxygenize()

# test examples
devtools::run_examples()

# Check package as CRAN
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
# remember one note ok

# check through devtools
devtools::check()
# no error/note/warns

# Check spelling
spelling::spell_check_package()
# these are all intended

# anything you fix above you will need to rerun
# roxygenize()

# Check URLs are correct
# remotes::install_github("r-lib/urlchecker")
urlchecker::url_check()

# check on other distributions
# you can change this to you
# very slow only run once
devtools::check_rhub(email = "buchananlab@gmail.com")
devtools::check_win_devel(email = "buchananlab@gmail.com")
# this will email you results from three big platforms
# no macOS, but erin is a macOS
# got back a note about knitr namespace but has to be
  # vignette builder so there's that

# build tar for release
# https://cran.r-project.org/submit.html
devtools::build()

# rebuild github stuff
tools::buildVignettes(dir = ".", tangle=TRUE)
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)

# rebuild site
pkgdown::build_site()

# update on git and reinstall
devtools::install_github("npm27/lrd")
