library("testthat")
library("Vicus")

options(testthat.use_colours = FALSE)

# Basic usage
test_file("testthat/test_LEM.R")
test_file("testthat/test_HLLE.R")
test_file("testthat/test_Vicus.R")
