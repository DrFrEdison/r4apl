# Here’s a step-by-step guide to create your R package called "r4apl":

# Step 1: Install Required Packages
install.packages("devtools")
install.packages("roxygen2")

# Step 2: Set Up Package Directory
setwd( "D:/")
devtools::create("r4apl")

# Step 3: Edit the DESCRIPTION File

# Step 4: Add Functions

# Step 5: Document Your Package
devtools::document()

# Step 6: Build and Install the Package
devtools::build()
devtools::install()

# Step 7: Test Your Package
library(r4apl)
hello_world()

install.packages("testthat")
library(testthat)
test_that("hello_world works", {
  expect_output(r4apl::hello_world(), "Hello, World!")
})
devtools::test()

usethis::use_github()

