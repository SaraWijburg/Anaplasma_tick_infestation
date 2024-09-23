##############################################################################################################
#------------------------------------------------------------------------------------------------------------# 
# Title: Clean data Serology
#------------------------------------------------------------------------------------------------------------# 
##############################################################################################################

data_serology <- serology_data_org1 %>% 
  clean_names() %>% 
  select(-total_score) %>% 
  # from wide to long 
  pivot_longer(!c(group_code, group_id, lamb_id), names_to = "day") %>% 
  mutate(day = str_remove(day, "day_") %>% as.numeric,
         serum = if_else(day == 28, "SERUM-2", "SERUM-1")) %>% 
  # join to other serology dataset 
  left_join(serology_data_org2 %>% 
              clean_names() %>% 
              select(value = sample_date, result, score_margit)) %>% 
  # add column about score and numeric value result 
  mutate(result_num = result %>% as.numeric, 
         score = case_when(
           result == "negative" ~ 0, 
           result == "128" ~ 1, 
           result == "256" ~ 2,  
           result == "512" ~ 3, 
           result == "1024" ~ 4, 
           result == "2048" ~ 5, 
           TRUE ~ 5)) %>% suppressWarnings()

#------------------ Remove
rm(serology_data_org1, serology_data_org2); gc()
