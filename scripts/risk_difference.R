#############################################################
# Calculate risk difference based on expected and observed  #
# case numbers; primarily intented to assess COVID impact.  #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)
library(tibble)
library(epitools)

source("shared_wrappers.R")

#' A helper function to calculate RR layer-wise; it is intended to be used within a pmap wrapper.
#'
#' @param N_cases Number of observed cases.
#' @param Population Population on which the observations are made.
#' @param Expected_numbers Number of expected cases.
#' @param Population_base Base population used to calculate expectetions. Needs to be declared, even if same as Population.
#' @return A dataframe with one single row, containing input data and risk difference.
#' @examples
#' .errorprone_rr_for_stratum(10, 100, 25, 200)
.errorprone_rr_for_stratum <- function(N_cases, Population, Expected_numbers, Population_base, ...){
  
  tryCatch(
    expr = {
      out_df <- data.frame(...)
      in_data <- matrix(
        c(
          Population-N_cases, Population_base-Expected_numbers,
          N_cases, Expected_numbers
        ),
        nrow=2
      )
      
      rr_res <- in_data %>%
        epitools::riskratio() %>%
        .$measure %>%
        .[2,]
      
      out_df %>%
        mutate(
          RR    = 1 - rr_res["estimate"],
          lower = 1 - rr_res["upper"],
          upper = 1 - rr_res["lower"]
        )
    },
    error = function(e){
      data.frame(...)
    }
  )
}


#' Calculate the change in disease (or dectection) risk (chance), 
#' based on a dataframe containing expected numbers, observed numbers and
#' the population(s) these numbers should be projected to.
#'
#' @param in_tab The input dataframe containg case numbers and base population size.
#' @return A dataframe with risk difference and 95%CI.
#' @examples
#' calculate_risk_difference(expectations_table)
calculate_risk_difference <- function(in_tab){
  
  if(! "Population_base" %in% colnames(in_tab)) {
    in_tab$Population_base <- in_tab$Population
  }

  in_tab %>%
    mutate(
      N_cases           = round(N_cases, 0),
      Population        = round(Population, 0),
      Expected_numbers = round(Expected_numbers, 0),
      Population_base   = round(Population_base, 0)
    ) %>%
    purrr::pmap_dfr(.errorprone_rr_for_stratum)
}
