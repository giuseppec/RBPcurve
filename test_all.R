library(devtools)
library(testthat)
library(mlr)

load_all(".")
source("tests/testthat/helper_objects.R")
test_dir("tests/testthat")