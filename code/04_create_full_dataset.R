deep <- read_csv("data/interim/tidy_deepddg.csv")
mcsm <- read_csv("data/interim/tidy_mcsm.csv")
simba <- read_csv("data/interim/tidy_simba.csv")
bloom <- read_csv("data/processed/bloom_clean.csv")

join_df <- deep %>% 
  bind_rows(mcsm) %>% 
  bind_rows(simba) %>% 
  inner_join(bloom, by = c("Wild" = "wildtype",
                           "Mutated" = "mutant", 
                           "Number" = "site_SARS2"))

join_df %>% 
  mutate(RSA = as.double(RSA)) %>% 
  write_csv("data/interim/all_data.csv")

join_df %>% 
  group_by(Number, mutation, Method) %>% 
  summarize(expr_avg_all = mean(expr_avg),
            bind_avg_all = mean(bind_avg),
            ddG = mean(ddG),
            RSA = mean(RSA)) %>% 
  write_csv("data/interim/all_data_avg.csv")
