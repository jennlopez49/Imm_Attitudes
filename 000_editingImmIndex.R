### ADDING 287G BY STATE BY YEAR ----------------------------------------------
c287g <- read.csv("/Users/jenniferlopez/Desktop/COIi work/State_Laws/states_287g_sums.csv")


### adding the 2012-2016 to the index 

indicators_state_2016 <- full_indicators %>% left_join(c287g, 
                                                       by = c("State" = "State")) %>%
  select(-c(X.x, X.y))
indicators_state_2016 <- indicators_state_2016 %>% mutate(
  total_exp_lat_2016_nasrm = ifelse(is.na(total_exp_lat_2016), 0, total_exp_lat_2016),
  total_exp_for_2016_nasrm = ifelse(is.na(total_exp_for_2016), 0, total_exp_for_2016),
  imm_clim_2016 = Imm_Class_Concrete_2016 + total_exp_lat_2016_nasrm, 
  imm_clim_2016_alt = Imm_Class_Concrete_2016 + total_exp_for_2016_nasrm, 
)
