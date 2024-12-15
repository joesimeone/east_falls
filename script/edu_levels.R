library(tidycensus)
library(tigris)
library(tidyverse)



acs_vars <- load_variables(2023, "acs5")


## -----------------------------------------------------------------------------
# Variable Call Prep ----
## -----------------------------------------------------------------------------

## Helps with ACS Sex By Age calls 
acs_code_trails <- 2:6
acs_code_trails <- str_pad(acs_code_trails, 3, pad = "0")


edu_codes <- glue::glue("B06009_{acs_code_trails}")


## East Falls 
edu_status <- tidycensus::get_acs(
  geography = "zcta",
  zcta = "19129",
  year = 2023,
  variables = edu_codes,
  geometry = FALSE
  )

## County
co_edu_status <- tidycensus::get_acs(
  geography = "county",
  state = "PA",
  county = "Philadelphia",
  year = 2023,
  variables = edu_codes,
  geometry = FALSE
)




## ----------------------------------------------------------------------------
# Initial data cleaning  -----
## ----------------------------------------------------------------------------

## East Falls
edu_cl <- 
  edu_status %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", 
                    "edu_level"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  mutate(edu_level = 
         snakecase::to_title_case(edu_level),
         edu_level = as.factor(edu_level),
         edu_level = 
           fct_relevel(edu_level, 
                       c("Less than High School Graduate", "High School Graduate Includes Equivalency", 
                         "Some College or Associate s Degree", 
                         "Bachelor s Degree",  "Graduate or Professional Degree")))

## County
co_edu_cl <- 
  co_edu_status %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", 
                    "edu_level"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  mutate(edu_level = 
           snakecase::to_title_case(edu_level),
         edu_level = as.factor(edu_level),
         edu_level = 
           fct_relevel(edu_level, 
                       c("Less than High School Graduate", "High School Graduate Includes Equivalency", 
                         "Some College or Associate s Degree", 
                         "Bachelor s Degree",  "Graduate or Professional Degree")))

## -----------------------------------------------------------------------------
# Export Tables -----
## -----------------------------------------------------------------------------


saveRDS(edu_cl, "data/east_falls_edu_level.R")
saveRDS(co_edu_cl, "data/phl_county_edu_level.R")

# ggplot(edu_cl, aes(estimate, edu_level, fill = edu_level)) +
#  geom_col(fill = "#d8527c")
  
