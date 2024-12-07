library(tidycensus)
library(tigris)
library(tidyverse)



acs_vars <- load_variables(2022, "acs5")

## -----------------------------------------------------------------------------
# Variable Call Prep ----
## -----------------------------------------------------------------------------

## Helps with ACS Sex By Age calls 
acs_code_trails <- 1:49
acs_code_trails <- str_pad(acs_code_trails, 3, pad = "0")


sex_by_age_codes <- glue::glue("B01001_{acs_code_trails}")


## East Falls
sex_by_age <- tidycensus::get_acs(
  geography = "zcta",
  zcta = "19129",
  year = 2022,
  variables = sex_by_age_codes,
  geometry = FALSE
)


## PHL County
co_sex_by_age <- tidycensus::get_acs(
  geography = "county",
  state = "PA",
  county = "Philadelphia",
  year = 2022,
  variables = sex_by_age_codes,
  geometry = FALSE
)




## ----------------------------------------------------------------------------
# Initial data cleaning  -----
## ----------------------------------------------------------------------------

## East Falls
sex_by_age_cl <- 
  sex_by_age %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", "gender", "age"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  mutate(broader_age_groups = 
           case_when(
             age %in% c("Under 5 years", "5 to 9 years", 
                        "10 to 14 years", "15 to 17 years") ~ "17 & Under",
             age %in% c("18 and 19 years",
                        "20 years", "21 years", "22 to 24 years",
                         "25 to 29 years", "30 to 34 years") ~ "18 - 34",
             age %in% c("35 to 39 years", "40 to 44 years", "45 to 49 years",  
                        "50 to 54 years") ~ "35 - 54",
             age %in% c( "55 to 59 years", "60 and 61 years", "62 to 64 years", "65 and 66 years",  
                         "67 to 69 years", "70 to 74 years") ~ "55 - 74", 
             age %in% c( "75 to 79 years", "80 to 84 years", "85 years and over") ~ "75 & Over",
             TRUE ~ age),
         broader_age_groups = as.factor(broader_age_groups),
         gender = as.factor(gender)
  )

## PHL County
co_sex_by_age_cl <- 
  co_sex_by_age %>%
  left_join(acs_vars, 
            by= c("variable" = "name")) %>% 
  mutate(label_to_sep = label) %>% 
  separate(label_to_sep, 
           into = c("estimate_str", "total", "gender", "age"), 
           sep = "[:!!]+", 
           extra = "merge", 
           fill = "right") %>% 
  mutate(broader_age_groups = 
           case_when(
             age %in% c("Under 5 years", "5 to 9 years", 
                        "10 to 14 years", "15 to 17 years") ~ "17 & Under",
             age %in% c("18 and 19 years",
                        "20 years", "21 years", "22 to 24 years",
                        "25 to 29 years", "30 to 34 years") ~ "18 - 34",
             age %in% c("35 to 39 years", "40 to 44 years", "45 to 49 years",  
                        "50 to 54 years") ~ "35 - 54",
             age %in% c( "55 to 59 years", "60 and 61 years", "62 to 64 years", "65 and 66 years",  
                         "67 to 69 years", "70 to 74 years") ~ "55 - 74", 
             age %in% c( "75 to 79 years", "80 to 84 years", "85 years and over") ~ "75 & Over",
             TRUE ~ age),
         broader_age_groups = as.factor(broader_age_groups),
         gender = as.factor(gender)
  )                       

## -----------------------------------------------------------------------------
# Export Tables -----
## -----------------------------------------------------------------------------
saveRDS(co_sex_by_age_cl, "data/phl_county_sex_age_level.R")
saveRDS(sex_by_age_cl, "data/east_falls_sex_age_level.R")






## ----------------------------------------------------------------------------
# Prep data for graph ----
## ----------------------------------------------------------------------------

# sex_by_age_cl <-  
#   sex_by_age_cl %>% 
#   filter(!is.na(broader_age_groups) & 
#            broader_age_groups != "") %>% 
#   mutate(broader_age_groups = 
#            fct_relevel(broader_age_groups, 
#                        c("17 & Under", "18 - 34", 
#                          "35 - 54", "55 - 74",  "75 & Over")),
#           graph_est = 
#            if_else(gender == "Female",
#                    -estimate, estimate))
# 
# ## Graph elements 
# abs_axis <- c(-2000, -1000, 0, 1000, 2000)
# fill_colors <- c("#4060c8", "#b93961")
# 
# 
#   ggplot(sex_by_age_cl, aes(graph_est, broader_age_groups, fill = gender)) +
#   geom_col() +
#   scale_fill_manual(values = fill_colors) +
#   scale_x_continuous(breaks = abs_axis,
#                      labels = abs(abs_axis)) 
# 
