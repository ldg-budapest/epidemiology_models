#############################################################
# Calculate expected case numbers based on previous years   #
# using a Poisson-regression model                          #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)

calculate_poisson_expectation <- function(
    in_tab, grouping_vars, collapse_ages=TRUE,
    base_min=2011, base_max=2019, ...
) {
  model_input <- in_tab %>%
    mutate(
      N_cases  = round(N_cases, 0),
      Period   = factor(Period),
      Age      = as.numeric(factor(Age)),
      logpop   = log(Population)
    )
  
  model <- model_input %>%
    filter(Period %in% seq(base_min, base_max)) %>%
    mutate(
      Period = as.numeric(Period)
    )
  
  if (collapse_ages) {
    model <- model %>%
      glm(
        # N_cases ~ Period + offset(Age) + offset(logpop),
        N_cases ~ Period + Age + offset(logpop),
        family = poisson(link = "log"), data = ., ...
      )
  } else {
    model <- model %>%
      glm(
        N_cases ~ Period + offset(logpop),
        family = poisson(link = "log"), data = ., ...
      )
  }
  
  predicted_vector <- model_input %>%
    mutate(
      Period = as.numeric(Period)
    ) %>%
    select(Period, Age, logpop) %>%
    predict(model, newdata=.)
  
  in_tab$Predicted_numbers <- exp(predicted_vector)
  
  in_tab %>%
    group_by(across(all_of(c(grouping_vars, "Period")))) %>%
    summarise(
      Population        = sum(Population, na.rm=TRUE),
      N_cases           = sum(N_cases, na.rm=TRUE),
      Predicted_numbers = sum(Predicted_numbers, na.rm=TRUE)
    ) %>%
    ungroup()
  
}

.calc_errorprone_rr <- function(N_cases, Population, Predicted_numbers, Population_base, ...){
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
