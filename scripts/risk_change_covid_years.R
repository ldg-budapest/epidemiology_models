#############################################################
# Calculate risk difference based on expected and observed  #
# case numbers; primarily intented to assess COVID impact.  #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)


#' Calculate the change in disease (or dectection) risk (chance), 
#' based on a dataframe containing expected numbers, observed numbers and
#' the population(s) these numbers should be projected to
#'
#' @param in_tab The input dataframe containg case numbers and base population size.
#' @return A dataframe
#' @examples
#' calculate_risk_difference()
calculate_risk_difference <- function(N_cases, Population, Predicted_numbers, Population_base, ...){
  tryCatch(
    expr = {
      out_df <- data.frame(...)
      in_data <- matrix(
        c(
          Population-N_cases, Population_base-Predicted_numbers,
          N_cases, Predicted_numbers
        ),
        nrow=2
      )
      
      rr_res <- in_data %>%
        riskratio() %>%
        .$measure %>%
        .[2,]
      
      out_df %>%
        mutate(
          RR=rr_res["estimate"],
          lower=rr_res["lower"],
          upper=rr_res["upper"]
        )
    },
    error = function(e){
      data.frame(...)
    }
  )
}
