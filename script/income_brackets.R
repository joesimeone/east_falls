library(tidycensus)
library(tigris)
library(tidyverse)



acs_vars <- load_variables(2022, "acs5")


## -----------------------------------------------------------------------------
# Variable Call Prep ----
## -----------------------------------------------------------------------------


## Helps with ACS Income bracket calls 
acs_code_trails <- 1:11
acs_code_trails <- str_pad(acs_code_trails, 3, pad = "0")


income_bracket_codes <- glue::glue("B06010_{acs_code_trails}")


## East Falls
income_brackets <- 
  tidycensus::get_acs(
  geography = "zcta",
  zcta = "19129",
  year = 2022,
  variables = income_bracket_codes,
  geometry = FALSE
)

## PHL County
co_income_brackets <- tidycensus::get_acs(
  geography = "county",
  state = "PA",
  county = "Philadelphia",
  year = 2022,
  variables = income_bracket_codes,
  geometry = FALSE
)

## ----------------------------------------------------------------------------
# Initial data cleaning  -----
## ----------------------------------------------------------------------------

## East Falls
income_cl <- 
  income_brackets %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", 
                    "descriptor", "income_bracket"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
   filter(!is.na(income_bracket) & 
           income_bracket != "") %>% 
  mutate(income_bracket = as.factor(income_bracket),
         income_bracket = fct_relevel(income_bracket, 
                                      "$1 to $9,999 or loss", "$10,000 to $14,999",
                                      "$15,000 to $24,999", "$25,000 to $34,999", 
                                      "$35,000 to $49,999", "$50,000 to $64,999",
                                      "$65,000 to $74,999", "$75,000 or more"))

## County
co_income_cl <- 
  co_income_brackets %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", 
                    "descriptor", "income_bracket"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  filter(!is.na(income_bracket) & 
           income_bracket != "") %>% 
  mutate(income_bracket = as.factor(income_bracket),
         income_bracket = fct_relevel(income_bracket, 
                                      "$1 to $9,999 or loss", "$10,000 to $14,999",
                                      "$15,000 to $24,999", "$25,000 to $34,999", 
                                      "$35,000 to $49,999", "$50,000 to $64,999",
                                      "$65,000 to $74,999", "$75,000 or more"))

## -----------------------------------------------------------------------------
# Export Tables -----
## -----------------------------------------------------------------------------
write_csv(income_cl, "data/east_falls_income_level.csv")
write_csv(co_income_cl, "data/phl_county_income_level.csv")



# ggplot(income_cl, aes(estimate, income_bracket)) +
#   geom_col(fill =    "#6996e3") 

