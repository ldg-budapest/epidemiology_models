# epidemiology_models

Calculate standardized rates, trends and expected rates

## Aim

The repo contains R scripts that can be reused by multiple epidemiological studies of our research group.  
We hope that publishing the code increases reproducibility and transparence of our methods.  

## Usage

Each script contains a main function that calculates an epidemiological indicator from a dataframe with expected columns: `Age`, `Sex`, `Period`, `Diagnosis`. These functions will be available after sourcing the scripts, but no data manipulatin is executed during sourcing (scripts are only for storing functions, not to execute them).  
The functions are designed to be used inside dplyr chains and equipped with sensible defaults. Please refer to the (Roxygen-style) comments above each function for more information.  
A showcase notebook is included in the [notebooks] folder that demonstrates basic usage. Example data shown in the notebook is downloaded from the Central Statistical Office of Hungary, but emulates 1-year granularity of the age parameter, instead of the actual 5-year granularity in official reports. Thus, shown data **does not represent real mortality rates** in Hungary!
