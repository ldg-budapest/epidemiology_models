#############################################################
# Main runner for test scripts                              #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(testthat)

filter <- dplyr::filter

# Load a dataset the will be shared within tests 
example_dataset <- "example_data/crc_lung_mortality.csv" %>%
  read.csv(check.names=FALSE) %>%
  pivot_longer(
    one_of("Colorectal (C18)", "Lung (C33-34)"),
    names_to = "Diagnosis", values_to = "N_cases"
  ) %>%
  mutate(
    Age = as.character(Age)
  )

# Run each set of tests in the folder
for (fn in dir("tests")) {
  if (fn != "test_main.R") {
    cat("Running tests in ", fn, "\n")
    source(file.path("tests", fn), chdir=TRUE)
    cat("Completed tests in ", fn, "\n")
  }
}
