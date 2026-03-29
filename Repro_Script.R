############################################################
## PROJECT: Mozambique Development Progress Dashboard
## PURPOSE: Clean, harmonize, and prepare World Bank data
##          for comparison between Mozambique and
##          Sub-Saharan Africa
## AUTHOR: Raimundo Cumba
## NOTE:
## - This script starts from raw downloaded World Bank files
## - It integrates the corrected poverty indicator
## - No imputation is applied
############################################################


############################
## 1. LOAD REQUIRED PACKAGES
############################
rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


############################
## 2. DEFINE FILE PATHS
############################

## Main dataset downloaded from World Bank
main_file <- "DataSocioeco_data.csv"

## Corrected poverty dataset downloaded separately
poverty_file <- "poverty_corrected_data.csv"


############################
## 3. READ RAW DATA
############################

## Read the main dataset
df_main <- read_csv(main_file, show_col_types = FALSE)

## Read the corrected poverty dataset
df_poverty <- read_csv(poverty_file, show_col_types = FALSE)


############################
## 4. IDENTIFY YEAR COLUMNS
############################

## World Bank year columns usually look like:
## 2000 [YR2000], 2001 [YR2001], etc.

year_cols_main <- grep("^\\d{4} \\[YR\\d{4}\\]$", names(df_main), value = TRUE)
year_cols_poverty <- grep("^\\d{4} \\[YR\\d{4}\\]$", names(df_poverty), value = TRUE)


############################
## 5. CLEAN MAIN DATASET
############################

## Convert year columns to numeric before reshaping
df_main[year_cols_main] <- lapply(df_main[year_cols_main], as.numeric)

## Reshape main dataset from wide to long format
df_main_long <- df_main %>%
  pivot_longer(
    cols = all_of(year_cols_main),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_remove(Year, " \\[YR\\d{4}\\]$"),
    Year = as.integer(Year)
  ) %>%
  rename(
    Country = `Country Name`,
    CountryCode = `Country Code`,
    Indicator = `Series Name`,
    IndicatorCode = `Series Code`
  ) %>%
  mutate(
    Value = as.numeric(Value)
  )


############################
## 6. CLEAN POVERTY DATASET
############################

## Convert year columns to numeric before reshaping
df_poverty[year_cols_poverty] <- lapply(df_poverty[year_cols_poverty], as.numeric)

## Reshape corrected poverty dataset from wide to long format
df_poverty_long <- df_poverty %>%
  pivot_longer(
    cols = all_of(year_cols_poverty),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_remove(Year, " \\[YR\\d{4}\\]$"),
    Year = as.integer(Year)
  ) %>%
  rename(
    Country = `Country Name`,
    CountryCode = `Country Code`,
    Indicator = `Series Name`,
    IndicatorCode = `Series Code`
  ) %>%
  mutate(
    Value = as.numeric(Value)
  )


############################
## 7. REMOVE WRONG POVERTY FROM MAIN DATA
############################

## We remove any poverty indicator previously included in the main file
## so that we can replace it with the corrected poverty headcount variable

df_main_long <- df_main_long %>%
  filter(!str_detect(Indicator, regex("poverty", ignore_case = TRUE)))


############################
## 8. KEEP ONLY THE CORRECT POVERTY INDICATOR
############################

## Keep only the intended poverty headcount indicator
df_poverty_long <- df_poverty_long %>%
  filter(
    Indicator == "Poverty headcount ratio at $3.00 a day (2021 PPP) (% of population)"
  )


############################
## 9. COMBINE MAIN + CORRECTED POVERTY
############################

df_long <- bind_rows(df_main_long, df_poverty_long)


############################
## 10. FILTER COUNTRIES OF INTEREST
############################

## Keep only Mozambique and Sub-Saharan Africa
df_long <- df_long %>%
  filter(Country %in% c("Mozambique", "Sub-Saharan Africa"))


############################
## 11. CREATE SHORT INDICATOR NAMES
############################

df_long <- df_long %>%
  mutate(
    Indicator_short = case_when(
      Indicator == "GDP per capita (current US$)" ~ "GDP",
      Indicator == "Poverty headcount ratio at $3.00 a day (2021 PPP) (% of population)" ~ "Poverty",
      Indicator == "School enrollment, primary (% gross)" ~ "Enrollment",
      Indicator == "Primary completion rate, total (% of relevant age group)" ~ "Completion",
      Indicator == "Mortality rate, under-5 (per 1,000 live births)" ~ "Under5_Mortality",
      Indicator == "Prevalence of stunting, height for age (modeled estimate, % of children under 5)" ~ "Stunting",
      Indicator == "Access to electricity (% of population)" ~ "Electricity",
      Indicator == "People using at least basic drinking water services (% of population)" ~ "Water",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Indicator_short))


############################
## 12. KEEP ONLY RELEVANT COLUMNS
############################

df_long <- df_long %>%
  select(Country, CountryCode, Indicator, Indicator_short, Year, Value)


############################
## 13. CHECK DATA COVERAGE
############################

coverage_summary <- df_long %>%
  group_by(Country, Indicator_short) %>%
  summarise(
    n_years = sum(!is.na(Value)),
    min_year = min(Year[!is.na(Value)], na.rm = TRUE),
    max_year = max(Year[!is.na(Value)], na.rm = TRUE),
    .groups = "drop"
  )

print(coverage_summary)


############################
## 14. SAVE CLEAN LONG DATASET
############################

write_csv(df_long, "worldbank_long_clean.csv")


############################
## 15. CREATE COMPARISON TABLE
############################

## Convert from long to wide so Mozambique and SSA are separate columns
df_compare <- df_long %>%
  select(Country, Indicator_short, Year, Value) %>%
  pivot_wider(
    names_from = Country,
    values_from = Value
  ) %>%
  arrange(Indicator_short, Year)


############################
## 16. CREATE SAFE DIFFERENCE VARIABLE
############################

## Difference is calculated only when both values are available
df_compare <- df_compare %>%
  mutate(
    Difference = ifelse(
      !is.na(Mozambique) & !is.na(`Sub-Saharan Africa`),
      Mozambique - `Sub-Saharan Africa`,
      NA_real_
    )
  )


############################
## 17. CREATE SAFE PERCENT CHANGE
############################

## Percent change is computed within each indicator
## only when current and previous values are both available
df_compare <- df_compare %>%
  group_by(Indicator_short) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    pct_change_moz = ifelse(
      !is.na(Mozambique) & !is.na(lag(Mozambique)) & lag(Mozambique) != 0,
      (Mozambique - lag(Mozambique)) / lag(Mozambique) * 100,
      NA_real_
    ),
    pct_change_ssa = ifelse(
      !is.na(`Sub-Saharan Africa`) & !is.na(lag(`Sub-Saharan Africa`)) & lag(`Sub-Saharan Africa`) != 0,
      (`Sub-Saharan Africa` - lag(`Sub-Saharan Africa`)) / lag(`Sub-Saharan Africa`) * 100,
      NA_real_
    )
  ) %>%
  ungroup()


############################
## 18. CREATE COMPLETENESS FLAG
############################

df_compare <- df_compare %>%
  mutate(
    Complete_case = !is.na(Mozambique) & !is.na(`Sub-Saharan Africa`)
  )


############################
## 19. CREATE TREND VARIABLE FOR MOZAMBIQUE
############################

## Trend is defined based on first and last available values
trend_table <- df_compare %>%
  group_by(Indicator_short) %>%
  summarise(
    first_value = first(na.omit(Mozambique)),
    last_value  = last(na.omit(Mozambique)),
    Trend = case_when(
      last_value > first_value ~ "Increasing",
      last_value < first_value ~ "Decreasing",
      TRUE ~ "Stable"
    ),
    .groups = "drop"
  )

df_compare <- df_compare %>%
  left_join(trend_table %>% select(Indicator_short, Trend), by = "Indicator_short")


############################
## 20. CHECK MISSING DATA IN COMPARISON TABLE
############################

missing_summary <- df_compare %>%
  group_by(Indicator_short) %>%
  summarise(
    missing_moz = sum(is.na(Mozambique)),
    missing_ssa = sum(is.na(`Sub-Saharan Africa`)),
    valid_pairs = sum(Complete_case),
    .groups = "drop"
  )

print(missing_summary)


############################
## 21. SAVE FINAL COMPARISON DATASET
############################

write_csv(df_compare, "mozambique_vs_ssa_clean.csv")


############################
## 22. OPTIONAL: QUICK VALIDATION PLOT
############################

## Example: GDP comparison plot
gdp_plot <- df_compare %>%
  filter(Indicator_short == "GDP") %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Mozambique, color = "Mozambique"), linewidth = 1) +
  geom_line(aes(y = `Sub-Saharan Africa`, color = "Sub-Saharan Africa"), linewidth = 1) +
  labs(
    title = "GDP per capita: Mozambique vs Sub-Saharan Africa",
    x = "Year",
    y = "GDP per capita (current US$)",
    color = ""
  ) +
  theme_minimal()

print(gdp_plot)


############################################################
## END OF SCRIPT
##
## THANK YOU AND I WISH YOU ALL THE BEST
############################################################