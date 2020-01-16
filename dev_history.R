# --- use_git
usethis::use_git()

# --- build ignore

# --- description
usethis::use_description()

usethis::use_mit_license("ND")

usethis::use_vignette("tt_2020-01-14_Passwords")

# --- packages needed
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("forcats")


# --- deploy on Github : https://pkgdown.r-lib.org/articles/pkgdown.html
# Run once to configure package to use pkgdown
usethis::use_pkgdown()
# Run to build the website
pkgdown::build_site()
