# Renv functions ####
renv::status()
renv::update()
# renv::record("renv@1.1.1")
renv::snapshot()

# Here is a step-by-step guide to create your R package called "r4apl":

# Step 1: Install Required Packages
install.packages("devtools")
install.packages("roxygen2")
install.packages("this.path")

# Step 2: Set Up Package Directory
setwd(this.path::this.dir())

# Step 3: Edit the DESCRIPTION File
file.edit("DESCRIPTION")
usethis::use_version("minor")  # Options: "patch", "minor", "major", "dev"

# Step 4: Add Functions

# Step 5: Document Your Package
# unlink("NAMESPACE")  # Deletes old NAMESPACE
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

