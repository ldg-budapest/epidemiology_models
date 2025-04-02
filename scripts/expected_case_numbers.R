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
      Raw_rate = round(Raw_rate, 0),
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
        Raw_rate ~ Period + offset(Age) + offset(logpop),
        family = poisson(link = "log"), data = ., ...
      )
  } else {
    model <- model %>%
      glm(
        Raw_rate ~ Period + offset(logpop),
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
  
  in_tab
  
}

.calc_errorprone_rr <- function(Raw_rate, Population, Predicted_numbers, Population_base, ...){
  tryCatch(
    expr = {
      out_df <- data.frame(...)
      in_data <- matrix(
        c(
          Population-Raw_rate, Population_base-Predicted_numbers,
          Raw_rate, Predicted_numbers
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
