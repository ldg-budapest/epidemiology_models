# Setup

## Library imports

## Example dataset

``` r
example_dataset <- "../example_data/crc_lung_mortality.csv" %>%
  read.csv(check.names=FALSE) %>%
  pivot_longer(
    one_of("Colorectal (C18)", "Lung (C33-34)"),
    names_to = "Diagnosis", values_to = "N_cases"
  ) %>%
  mutate(
    Age = as.character(Age)
  )

# Commonly, numbers for both sexes merged are also present
example_dataset <- example_dataset %>%
  group_by(Age, Period, Diagnosis) %>%
  summarise(
    N_cases = sum(N_cases, na.rm=TRUE),
    Population = sum(Population, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Sex = "Total"
  ) %>%
  bind_rows(example_dataset)

# Adding the total age group to show it does not interfere with models
example_dataset <- example_dataset %>%
  group_by(Sex, Period, Diagnosis) %>%
  summarise(
    N_cases = sum(N_cases, na.rm=TRUE),
    Population = sum(Population, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Age = "Total"
  ) %>%
  bind_rows(example_dataset)

head(example_dataset)
```

    ## # A tibble: 6 × 6
    ##   Sex    Period Diagnosis        N_cases Population Age  
    ##   <chr>   <int> <chr>              <int>      <dbl> <chr>
    ## 1 Female   2011 Colorectal (C18)    1511   5233914  Total
    ## 2 Female   2011 Lung (C33-34)       2974   5233914  Total
    ## 3 Female   2012 Colorectal (C18)    1570   5200052  Total
    ## 4 Female   2012 Lung (C33-34)       3132   5200052  Total
    ## 5 Female   2013 Colorectal (C18)    1539   5183410. Total
    ## 6 Female   2013 Lung (C33-34)       3173   5183410. Total

``` r
age_10_dataset <- example_dataset %>%
  filter(Age != "Total") %>%
  mutate(
    Age = as.numeric(Age),
    Age_c = cut(Age, seq(-1, 100, 10)),
    Age_d = gsub(",.*", "", Age_c),
    Age_d = gsub("\\(", "", Age_d),
    Age_c = gsub(".*,", "", Age_c),
    Age_c = gsub("\\]", "", Age_c),
    Age = paste(as.numeric(Age_d)+1, "-", Age_c, sep=""),
    Age = ifelse(Age == "90-99", "90-x", Age)
  ) %>%
  group_by(Age, Sex, Period, Diagnosis) %>%
  summarise(
    Population = sum(Population, na.rm=TRUE),
    N_cases = sum(N_cases, na.rm=TRUE)
  ) %>%
  ungroup()

# Re-adding the total age group
age_10_dataset <- age_10_dataset %>%
  group_by(Sex, Period, Diagnosis) %>%
  summarise(
    N_cases = sum(N_cases, na.rm=TRUE),
    Population = sum(Population, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Age = "Total"
  ) %>%
  bind_rows(age_10_dataset)

head(age_10_dataset)
```

    ## # A tibble: 6 × 6
    ##   Sex    Period Diagnosis        N_cases Population Age  
    ##   <chr>   <int> <chr>              <int>      <dbl> <chr>
    ## 1 Female   2011 Colorectal (C18)    1511   5233914  Total
    ## 2 Female   2011 Lung (C33-34)       2974   5233914  Total
    ## 3 Female   2012 Colorectal (C18)    1570   5200052  Total
    ## 4 Female   2012 Lung (C33-34)       3132   5200052  Total
    ## 5 Female   2013 Colorectal (C18)    1539   5183410. Total
    ## 6 Female   2013 Lung (C33-34)       3173   5183410. Total

# Functionality

## Standardized rates

``` r
source("../scripts/standardized_rates.R")
```

By default, rate is standardized as the number of cases in 100,000
persons.

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(extra_grouping_cols="Diagnosis")

standardized_table %>%
  filter(Period == "2021", Sex == "Total") %>%
  select(Age, name=Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider() %>%
  tail(8)
```

    ## # A tibble: 8 × 3
    ##   Age   `Colorectal (C18)` `Lung (C33-34)`
    ##   <chr>              <dbl>           <dbl>
    ## 1 30-39               1.2             0.88
    ## 2 40-49               6.51            8.41
    ## 3 50-59              20.8            74.5 
    ## 4 60-69              69.6           246.  
    ## 5 70-79             129.            303.  
    ## 6 80-89             206.            248.  
    ## 7 90-x              225.            142.  
    ## 8 Total              34.2            81.0

The same function can be used to calculate direct standardized rates

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis", standard_population="esp2013"
  )

standardized_table %>%
  select(Period, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Diagnosis", "Sex")) %>%
  arrange(desc(Period)) %>%
  head()
```

    ## # A tibble: 6 × 8
    ##   Period `Colorectal (C18)_Female` Colorectal (C18)_Mal…¹ Colorectal (C18)_Tot…²
    ##    <int>                     <dbl>                  <dbl>                  <dbl>
    ## 1   2023                      21.6                   44.7                   30.7
    ## 2   2022                      22.9                   44.1                   31.2
    ## 3   2021                      23.5                   44.8                   31.8
    ## 4   2020                      22.8                   42.8                   30.6
    ## 5   2019                      22.5                   44.4                   31.1
    ## 6   2018                      23.2                   44.7                   31.5
    ## # ℹ abbreviated names: ¹​`Colorectal (C18)_Male`, ²​`Colorectal (C18)_Total`
    ## # ℹ 4 more variables: `Lung (C33-34)_Female` <dbl>, `Lung (C33-34)_Male` <dbl>,
    ## #   `Lung (C33-34)_Total` <dbl>, NA_NA <dbl>

The 10-year granularity should not differ much from single-year
granularity:

``` r
standardized_table <- example_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis", standard_population="esp2013"
  )

standardized_table %>%
  select(Period, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Diagnosis", "Sex")) %>%
  arrange(desc(Period)) %>%
  head()
```

    ## # A tibble: 6 × 7
    ##   Period `Colorectal (C18)_Female` Colorectal (C18)_Mal…¹ Colorectal (C18)_Tot…²
    ##    <int>                     <dbl>                  <dbl>                  <dbl>
    ## 1   2023                      23.6                   49.3                   33.2
    ## 2   2022                      24.9                   48.2                   33.7
    ## 3   2021                      25.7                   48.6                   34.4
    ## 4   2020                      24.8                   47.4                   33.2
    ## 5   2019                      24.8                   49.1                   34.0
    ## 6   2018                      25.5                   49.4                   34.4
    ## # ℹ abbreviated names: ¹​`Colorectal (C18)_Male`, ²​`Colorectal (C18)_Total`
    ## # ℹ 3 more variables: `Lung (C33-34)_Female` <dbl>, `Lung (C33-34)_Male` <dbl>,
    ## #   `Lung (C33-34)_Total` <dbl>

A convenience option is to standardize to one of the years also present
in the data by simply supplying that year as a parameter.

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(
    extra_grouping_cols="Diagnosis", standard_population="2011"
  )

standardized_table %>%
  select(Period, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Diagnosis", "Sex")) %>%
  arrange(desc(Period)) %>%
  head()
```

    ## # A tibble: 6 × 7
    ##   Period `Colorectal (C18)_Female` Colorectal (C18)_Mal…¹ Colorectal (C18)_Tot…²
    ##    <int>                     <dbl>                  <dbl>                  <dbl>
    ## 1   2023                      24.7                   33.3                   29.2
    ## 2   2022                      25.9                   33.3                   29.7
    ## 3   2021                      26.7                   33.6                   30.3
    ## 4   2020                      25.9                   32.0                   29.0
    ## 5   2019                      25.8                   33.5                   29.7
    ## 6   2018                      26.6                   33.4                   30.0
    ## # ℹ abbreviated names: ¹​`Colorectal (C18)_Male`, ²​`Colorectal (C18)_Total`
    ## # ℹ 3 more variables: `Lung (C33-34)_Female` <dbl>, `Lung (C33-34)_Male` <dbl>,
    ## #   `Lung (C33-34)_Total` <dbl>

Please note, however that DSRs are applied to the total population and
not to separate age groups. Rates could be projected onto the population
of a single year, by explicitly providing weight tables for as bands as
below:

``` r
standardized_table <- purrr::map_dfr(
  list(c(50, 59), c(60, 69), c(70, 79)),
  function(x) {
    example_dataset %>%
    calculate_standardized_rate(
      extra_grouping_cols="Diagnosis",
      standard_population = .create_weight_table_from_population(
        filter(example_dataset, Age %in% seq(x[1], x[2])), "2011"
      )
    )%>%
    mutate(
      Age = paste(x[1], x[2], sep="-")
    )
  }
)

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8)
```

    ## # A tibble: 3 × 5
    ##   Age   `Female_Colorectal (C18)` `Male_Colorectal (C18)` `Female_Lung (C33-34)`
    ##   <chr>                     <dbl>                   <dbl>                  <dbl>
    ## 1 50-59                      18.2                    25.6                   59.7
    ## 2 60-69                      46.4                    94.6                  177. 
    ## 3 70-79                      86.7                   196.                   202. 
    ## # ℹ 1 more variable: `Male_Lung (C33-34)` <dbl>

As a comparison, the raw rates in the above age groups in 2021 was:

``` r
standardized_table <- age_10_dataset %>%
  calculate_standardized_rate(extra_grouping_cols="Diagnosis")

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age, Sex, Diagnosis, value=Std_rate) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8)
```

    ## # A tibble: 8 × 5
    ##   Age   `Female_Colorectal (C18)` `Male_Colorectal (C18)` `Female_Lung (C33-34)`
    ##   <chr>                     <dbl>                   <dbl>                  <dbl>
    ## 1 30-39                      1.64                    0.78                   0.66
    ## 2 40-49                      6.52                    6.5                    6.27
    ## 3 50-59                     17.6                    24.1                   55.9 
    ## 4 60-69                     47.5                    97.5                  181.  
    ## 5 70-79                     86.0                   195.                   203.  
    ## 6 80-89                    169.                    290.                   166.  
    ## 7 90-x                     199.                    301.                   135.  
    ## 8 Total                     29.9                    38.9                   64.7 
    ## # ℹ 1 more variable: `Male_Lung (C33-34)` <dbl>

## Average yearly change

``` r
source("../scripts/annual_rate_change.R")
```

A simple case if we want to see change for the total population and put
age and sex as co-variates.

``` r
standardized_table <- age_10_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Age != "Total", Sex != "Total") %>%
  calculate_poisson_rate(
    c("Diagnosis")
  )

head(standardized_table)
```

    ## # A tibble: 2 × 5
    ##   Diagnosis        estimate   CI_lo     CI_hi    p_value
    ##   <chr>               <dbl>   <dbl>     <dbl>      <dbl>
    ## 1 Colorectal (C18)  -0.0146 -0.0206 -0.00863  0.00000168
    ## 2 Lung (C33-34)     -0.0100 -0.0206  0.000637 0.0654

This could be done separately for age groups in the merged dataset.

``` r
standardized_table <- age_10_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Sex == "Total") %>%
  calculate_poisson_rate(
    c("Age", "Diagnosis")
  )

standardized_table %>%
  select(Age, name=Diagnosis, value=estimate) %>%
  mutate(
    value = round(value, 3)
  ) %>%
  pivot_wider() %>%
  tail(8)
```

    ## # A tibble: 8 × 3
    ##   Age   `Colorectal (C18)` `Lung (C33-34)`
    ##   <chr>              <dbl>           <dbl>
    ## 1 30-39              0.017          -0.042
    ## 2 40-49             -0.025          -0.109
    ## 3 50-59             -0.036          -0.055
    ## 4 60-69             -0.005           0.004
    ## 5 70-79             -0.01            0.012
    ## 6 80-89             -0.017           0.005
    ## 7 90-x              -0.019          -0.016
    ## 8 Total             -0.001           0.002

Better still, if this is calculated from the 1-year granularity of age
groups:

``` r
standardized_table <- example_dataset %>%
  filter(Period %in% seq(2011, 2019)) %>%
  filter(Age %in% seq(60, 69)) %>%
  calculate_poisson_rate(
    c("Diagnosis")
  )

standardized_table %>%
  mutate(
    Age = "60-69"
  ) %>%
  select(Age, name=Diagnosis, value=estimate) %>%
  mutate(
    value = round(value, 3)
  ) %>%
  pivot_wider() %>%
  tail(8)
```

    ## # A tibble: 1 × 3
    ##   Age   `Colorectal (C18)` `Lung (C33-34)`
    ##   <chr>              <dbl>           <dbl>
    ## 1 60-69             -0.007           0.003

## Expected case numbers

Expected case numbers could originate from virtual numbers
back-projected from rates.

``` r
standardized_table <- purrr::map_dfr(
  list(c(50, 59), c(60, 69), c(70, 79)),
  function(x) {
    example_dataset %>%
    calculate_standardized_rate(
      extra_grouping_cols="Diagnosis",
      standard_population = .create_weight_table_from_population(
        filter(example_dataset, Age %in% seq(x[1], x[2])), "2011"
      )
    )%>%
    mutate(
      Age = paste(x[1], x[2], sep="-")
    )
  }
) %>%
  inner_join(age_10_dataset) %>%
  mutate(
    Projected_cases = round(Population / 100000 * Std_rate, 1)
  )

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  head(8) %>%
  select(Sex, Age, Diagnosis, N_cases, Projected_cases)
```

    ## # A tibble: 8 × 5
    ##   Sex    Age   Diagnosis        N_cases Projected_cases
    ##   <chr>  <chr> <chr>              <int>           <dbl>
    ## 1 Female 50-59 Colorectal (C18)     113            117.
    ## 2 Male   50-59 Colorectal (C18)     149            158 
    ## 3 Female 50-59 Lung (C33-34)        359            383.
    ## 4 Male   50-59 Lung (C33-34)        579            627.
    ## 5 Female 60-69 Colorectal (C18)     332            325.
    ## 6 Male   60-69 Colorectal (C18)     542            526.
    ## 7 Female 60-69 Lung (C33-34)       1266           1241.
    ## 8 Male   60-69 Lung (C33-34)       1821           1785.

Another possible approach is to use the Poisson-model created previously
and predict case numbers based on the fit model.

``` r
source("../scripts/expected_case_numbers.R")

standardized_table <- example_dataset %>%
  filter(Age %in% seq(40, 79), Sex != "Total") %>%
  mutate(
    Age_bin = case_when(
      Age %in% seq(40, 49) ~ "40-49",
      Age %in% seq(50, 59) ~ "50-59",
      Age %in% seq(60, 69) ~ "60-69",
      Age %in% seq(70, 79) ~ "70-79",
      TRUE ~ NA_character_
    )
  ) %>%
  group_split(Diagnosis, Sex, Age_bin) %>%
  map_dfr(
    calculate_poisson_expectation,
    grouping_vars = c("Diagnosis", "Sex", "Age_bin")
  )

standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age_bin, Sex, Diagnosis, value=Predicted_numbers) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8)
```

    ## # A tibble: 4 × 5
    ##   Age_bin Female_Colorectal (C18…¹ Male_Colorectal (C18…² `Female_Lung (C33-34)`
    ##   <chr>                      <dbl>                  <dbl>                  <dbl>
    ## 1 40-49                       37.3                   39.2                   60.9
    ## 2 50-59                       81.8                  161.                   464. 
    ## 3 60-69                      292.                   510.                  1372. 
    ## 4 70-79                      472.                   615.                  1179. 
    ## # ℹ abbreviated names: ¹​`Female_Colorectal (C18)`, ²​`Male_Colorectal (C18)`
    ## # ℹ 1 more variable: `Male_Lung (C33-34)` <dbl>

As a reference, observed case numbers look as below.

``` r
standardized_table %>%
  filter(Period == "2021", Sex != "Total") %>%
  select(Age_bin, Sex, Diagnosis, value=N_cases) %>%
  mutate(
    value = round(value, 2)
  ) %>%
  pivot_wider(names_from=c("Sex", "Diagnosis")) %>%
  tail(8)
```

    ## # A tibble: 4 × 5
    ##   Age_bin Female_Colorectal (C18…¹ Male_Colorectal (C18…² `Female_Lung (C33-34)`
    ##   <chr>                      <dbl>                  <dbl>                  <dbl>
    ## 1 40-49                         51                     52                     49
    ## 2 50-59                        113                    149                    359
    ## 3 60-69                        332                    542                   1266
    ## 4 70-79                        462                    670                   1089
    ## # ℹ abbreviated names: ¹​`Female_Colorectal (C18)`, ²​`Male_Colorectal (C18)`
    ## # ℹ 1 more variable: `Male_Lung (C33-34)` <dbl>

# End
