library(tidycensus)
library(tigris)
library(tidyverse)



acs_vars <- load_variables(2022, "acs5")



## -----------------------------------------------------------------------------
# Variable Call Prep ----
## -----------------------------------------------------------------------------

## Helps with ACS Sex By Age calls 
acs_code_trails <- 1:5
acs_code_trails <- str_pad(acs_code_trails, 3, pad = "0")


race_codes <- c(glue::glue("B02001_{acs_code_trails}"), "B03002_012")



race_breakdowns <- tidycensus::get_acs(
  geography = "zcta",
  zcta = "19129",
  year = 2022,
  variables = race_codes,
  geometry = FALSE
)

co_race_breakdowns <- tidycensus::get_acs(
  geography = "county",
  state = "PA",
  county = "Philadelphia",
  year = 2022,
  variables = race_codes,
  geometry = FALSE
)


## ----------------------------------------------------------------------------
# Initial data cleaning  -----
## ----------------------------------------------------------------------------

race_breakdowns_cl <- 
  race_breakdowns %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", 
                    "racial_category"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  filter(estimate != 0)


co_race_breakdowns_cl <- 
  co_race_breakdowns %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", 
                    "racial_category"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  filter(estimate != 0)


## -----------------------------------------------------------------------------
# Export Tables -----
## -----------------------------------------------------------------------------


write_csv(race_breakdowns_cl, "data/east_falls_race_cats.csv")
write_csv(co_race_breakdowns_cl, "data/phl_county_race_cats.csv")

