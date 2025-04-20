#############################################################
# Shared utility functions that facilitate                  #
# layer-wise execution of calculations                      #
#############################################################

# Importing tidyverse packages, in case it has not been initialized yet
library(tidyr)
library(dplyr)
library(purrr)


# Error handling for strata where statistic cannot be calculated
execute_model_safely <- function(
    in_tab, stat_fun, grouping_vars, err_fun=NULL, debug_mode=FALSE, ...
){
  
  if(is.null(err_fun)) {
    err_fun <- function() {
      in_tab %>%
        select(one_of(grouping_vars)) %>%
        distinct()
    }
  }
  
  if (debug_mode) {
    tryCatch(
      expr = {
        stat_fun(in_tab, grouping_vars=grouping_vars, ...)
      },
      error = err_fun
    )
  } else {
    stat_fun(in_tab, grouping_vars=grouping_vars, ...)
  }
}

# Convenience function to apply the statistical calculation to individual strata
calculate_model_across_layers <- function(in_tab, stat_fun, grouping_vars, ...) {
  in_tab %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_split() %>%
    purrr::map_dfr(
      execute_model_safely, stat_fun=stat_fun, grouping_vars=grouping_vars, ...
    )
}
