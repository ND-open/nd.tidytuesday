# --- use_git
usethis::use_git()

# --- git ignore

# --- build ignore
usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("README.Rmd")
usethis::use_build_ignore("Reports")

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
usethis::use_package("purrr")
usethis::use_package("tibble")

usethis::use_package("pander")
usethis::use_package("Rmarkdown")
usethis::use_package("knitr")

usethis::use_package("stringr")
usethis::use_package("lubridate")
usethis::use_package("naniar")
usethis::use_package("readr")
usethis::use_package("visdat")

# --- set up tests
usethis::use_testthat()
usethis::use_test("test_base_summary")
usethis::use_test(name = "test_qual_fun", open = FALSE)
usethis::use_test(name = "test_tidy_qual", open = FALSE)

# --- deploy on Github : https://pkgdown.r-lib.org/articles/pkgdown.html
# Run once to configure package to use pkgdown
usethis::use_pkgdown()
# Run to build the website -- manual
pkgdown::build_site()


# --- set up github actions -- automate workflow check pkg + build site
remotes::install_github("r-lib/ghactions")

ghactions::use_ghactions(workflow = ghactions::website())
