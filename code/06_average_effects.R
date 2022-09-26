all_data_avg <- read_csv("data/interim/all_data_avg.csv")

# Grouping data by site for encoding as B-factor in PDBs and for site-tolerability plot

all_data_avg %>% 
  filter(str_sub(mutation, 1, 1) != str_sub(mutation, 5, 5)) %>% 
  group_by(Number = as.character(Number), Method) %>% 
  summarize(
    ddG = paste0("0", round0(mean(abs(ddG), na.rm = TRUE), 2))
    ) %>% 
  write_csv("data/interim/all_data_avg_ddG.csv")
  
all_data_avg %>% 
  filter(str_sub(mutation, 1, 1) != str_sub(mutation, 5, 5)) %>% 
  group_by(Number = as.character(Number)) %>% 
  summarize(
    expr = paste0("0", round0(mean(abs(expr_avg_all), na.rm = TRUE), 2)),
    bind = paste0("0", round0(mean(abs(bind_avg_all), na.rm = TRUE), 2))
  ) %>% 
  write_csv("data/interim/all_data_avg_expr_bind.csv")

  
  
  
  
  
