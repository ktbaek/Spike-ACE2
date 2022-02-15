read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double())) %>% 
  select(-RSA) %>% 
  pivot_longer(c(expr_avg, bind_avg), names_to = "Parameter") %>%
  mutate(
    group_1 = cut_width(value, 0.5, center = 0.25),
    group_1 = str_remove_all(group_1, "[()\\]\\[]"),
    group_2 = cut_width(value, 0.25, center = 0.125),
    group_2 = str_remove_all(group_2, "[()\\]\\[]")
  ) %>% 
  rowwise() %>% 
  mutate(
    group_1 = as.double(str_split(group_1, ",")[[1]][1]) + 0.25,
    group_2 = as.double(str_split(group_2, ",")[[1]][1]) + 0.125,
    ) %>% 
  ungroup() %>% 
  pivot_longer(c(group_1:group_2), names_to = "group", values_to = "mid") %>% 
  group_by(Parameter, PDB, Method, group, mid) %>% 
  summarize(
    mean_ddg = mean(ddG, na.rm = TRUE),
    sd_ddg = sd(ddG, na.rm = TRUE)
    ) %T>%
  write_csv("data/interim/all_data_binned.csv")

read_csv("data/interim/all_data_avg.csv") %>% 
  select(-RSA) %>% 
  pivot_longer(c(expr_avg_all, bind_avg_all), names_to = "Parameter") %>%
  mutate(
    group_1 = cut_width(value, 0.5, center = 0.25),
    group_1 = str_remove_all(group_1, "[()\\]\\[]"),
    group_2 = cut_width(value, 0.25, center = 0.125),
    group_2 = str_remove_all(group_2, "[()\\]\\[]")
  ) %>% 
  rowwise() %>% 
  mutate(
    group_1 = as.double(str_split(group_1, ",")[[1]][1]) + 0.25,
    group_2 = as.double(str_split(group_2, ",")[[1]][1]) + 0.125,
  ) %>% 
  ungroup() %>% 
  pivot_longer(c(group_1:group_2), names_to = "group", values_to = "mid") %>% 
  group_by(Parameter, Method, group, mid) %>% 
  summarize(
    mean_ddg = mean(ddG, na.rm = TRUE),
    sd_ddg = sd(ddG, na.rm = TRUE)
    ) %T>%
  write_csv("data/interim/all_data_avg_binned.csv")
