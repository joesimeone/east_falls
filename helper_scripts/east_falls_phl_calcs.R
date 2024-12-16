

# Add variable to help group map call -------------------------------------
east_falls$sex_age <- 
  east_falls$sex_age  %>% 
  mutate(helper = glue::glue("{broader_age_groups}_{gender}"))

philly$sex_age <- philly$sex_age %>% 
  mutate(helper = glue::glue("{broader_age_groups}_{gender}"))


# Calc East Falls Stats--------------------------------------

## Vector to help map2
group_vars <-
  c("edu_level", "income_bracket", 
    "racial_category", "helper")


east_falls_tbls <-
  map2(east_falls, group_vars,
      ~.x %>% 
        group_by({{.y}}, 
                 .drop = TRUE) %>% 
        mutate(est_upper = estimate + moe,
               est_lower = estimate - moe) %>% 
        ungroup() %>% 
        mutate(denom = sum(estimate),
               denom_lower = sum(est_lower),
               denom_upper = sum(est_upper)) %>% 
        group_by(label) %>% 
        mutate(est_pct = estimate / denom,
               est_upper_pct = est_upper / denom_upper,
               est_lower_pct = est_lower / denom_lower) %>% 
        rename(east_falls_est = estimate) %>% 
        select(-estimate_str))
        

phl_tbls <- 
  map2(philly, group_vars,
      ~.x %>% 
        group_by({{.y}}, 
                 .drop = TRUE) %>% 
        mutate(phl_est_upper = estimate + moe,
               phl_est_lower = estimate - moe) %>% 
        ungroup() %>% 
        mutate(phl_denom = sum(estimate),
               phl_denom_lower = sum(phl_est_lower),
               phl_denom_upper = sum(phl_est_upper)) %>% 
        group_by(label) %>% 
        mutate(phl_est_pct = estimate / phl_denom,
               phl_est_upper_pct = phl_est_upper / phl_denom_upper,
               phl_est_lower_pct = phl_est_lower / phl_denom_lower) %>% 
        rename(phl_est = estimate) %>% 
        select(-estimate_str))
           


# Join data for dumbell plots ---------------------------------------------

## Here, I'm having problem joining the age sex problem | Just going to do that
## separtely

east_falls_tbls_to_join <- east_falls_tbls[-c(4)]
phl_tbls_to_join <- phl_tbls[-c(4)]

join_vars <- group_vars[-c(4)]


join_some_tbls <-
  list(phl_tbls_to_join, 
       east_falls_tbls_to_join, 
       join_vars)

phl_east_falls <-
  pmap(join_some_tbls, 
       function(phl_tbls, east_falls_tbl, join_vars) {
    left_join(phl_tbls, east_falls_tbl, by = join_vars) %>% 
           select(all_of(join_vars), contains(c("est")))
  })

phl_east_falls_age_sex <-
  east_falls_tbls$sex_age %>% 
  group_by(broader_age_groups, gender) %>% 
  summarise(across(is.numeric,
                   ~(sum(.)))) %>% 
  left_join(phl_tbls$sex_age, 
            by = c("broader_age_groups", "gender")) %>% 
  distinct(broader_age_groups, gender, .keep_all = TRUE)

phl_east_falls$age_sex <- phl_east_falls_age_sex
  

  

# 
# 
# d <-
#   c %>% 
#   pivot_longer(cols = c("est_pct", "phl_est_pct"),
#                values_to = "percentages",
#                names_to = "calcs")

#   
# come_on <- ggplot(d, aes(percentages, edu_level)) +
#   geom_line() +
#   geom_point(aes(color = calcs), size = 3) +
#   scale_x_continuous(labels = scales::percent)  +
#   scale_color_manual(values = c("darkorange", "lightgray")) +
#   theme(legend.position = "top")
#   
# come_on



# f <- 
#   c %>% 
#   mutate(pct_chng_of_phl_pct = 
#            ((est_pct - phl_est_pct) / phl_est_pct) * 100)
# 
# 
# e <- ggplot(f, aes(pct_chng_of_phl_pct, edu_level)) +
#   geom_point(size = 3, color = "pink")
