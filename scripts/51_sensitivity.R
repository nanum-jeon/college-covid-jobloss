# scripts/51_sensitivity.R

# Load data 
covid_jobloss_results <- readRDS(file.path(DIR_RESULTS, "covid_jobloss_results.rds"))

 

# format data 
covid_jobloss_sens <- covid_jobloss_results$main_results %>%
  rbind(covid_jobloss_results$mechanism_results %>% 
          filter(Effect == "MED")) %>% 
  transmute(estimand = case_when(Effect == "Direct (NDE)" ~ "nde", 
                                 Effect == "Indirect (NIE)" ~ "nie",
                                 Effect == "Total (ATE)" ~ "ate",
                                 Effect == "MED" ~ "med"
                                 ), 
            group = Group, 
            est = Estimate,
            se = SE) %>% 
  mutate(group = factor(group, levels = c("White", "Black", "Light_Black", "Dark_Black"))) %>% 
  arrange(group) 
 

# sensitivity parameter
gamma <- c(-0.3, -0.2, -0.1, 0.1, 0.2, 0.3)
beta <- c(0.1, 0.2, 0.3)


# sensitivity analysis 
covid_jobloss_sens_results <- expand.grid(estimand = c("ate", "nde", "nie", "med"),
            gamma = gamma, beta = beta) %>%
  left_join(covid_jobloss_sens, by = c("estimand")) %>%
  group_by(group) %>% 
  mutate(adj = case_when(estimand=="ate" ~ est + gamma * beta,
                         estimand=="nde" ~ est + gamma * beta,
                         estimand=="nie" ~ est - gamma * beta * est[estimand == "med"],
                         TRUE ~ est - gamma * beta)) %>%
  ungroup() %>% 
  select(-est, -se) %>%
  pivot_wider(names_from = c("estimand", "group"), values_from = "adj") %>%
  mutate(bias = gamma * beta) %>%
  select(gamma, beta, bias, everything()) %>% 
  select(-contains("med_")) 

covid_jobloss_sens_results

# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_jobloss_sens_results.rds")
saveRDS(covid_jobloss_sens_results, rds_out)
message("Saved combined results to: ", rds_out)
