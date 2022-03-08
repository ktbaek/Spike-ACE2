bloom <- read_csv("data/raw/1-s2.0-S0092867420310035-mmc2.csv")

bloom_2 <- bloom %>% 
  filter(
    !is.na(bind_lib1),
    !is.na(bind_lib2),
    !is.na(expr_lib1),
    !is.na(expr_lib2)
  ) %>% 
  mutate(
    bind_res = lm(bind_lib2 ~ bind_lib1)$residuals,
    expr_res = lm(expr_lib2 ~ expr_lib1)$residuals
  ) %>% 
  filter(abs(expr_res) <= 1) %>% 
  filter(abs(bind_res) <= 1) %T>% 
  write_csv("data/interim/bloom_wo_outliers.csv")

x <- bloom_2 %>% 
  mutate(type = case_when(
    mutant == "*" ~ "Stop",
    wildtype == mutant ~ "Synonymous",
    TRUE ~ "Nonsynonymous"
  )) %>% 
  filter(
    type != "Stop",
    bind_lib1 >= -4.5,
    bind_lib2 >= -4.5
    ) %>% 
  write_csv("data/processed/bloom_clean.csv") 

x %>% 
  select(-bind_res, -expr_res, -type) %>% 
  write_csv("tables/Table_S1.csv") 
  
