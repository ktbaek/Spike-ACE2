p_value <- function(x,y) {
    m <- lm(y ~ x)
    return(summary(m)$coefficients[2,4])
  }

read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double())) %>%
  group_by(PDB, Method) %>% 
  summarize(
    expr_R = sqrt(rsquared(expr_avg, ddG)),
    bind_R = sqrt(rsquared(bind_avg, ddG)),
    expr_p = p_value(expr_avg, ddG),
    bind_p = p_value(bind_avg, ddG)
    ) %>% 
  filter(Method != "Simba_SYM") %>% 
  pivot_longer(c(expr_R:bind_p), names_to = c("Parameter", "Metric"), names_sep = "_") %>% 
  pivot_wider(names_from = Metric, values_from = value) %>% 
  select(Parameter, PDB, everything()) %>% 
  arrange(Parameter, PDB) %>% 
  mutate(
    Parameter = ifelse(Parameter == "bind", "Binding", "Expression"), 
    R = round(R, 2),
    p = ifelse(p < 0.001, "<0.001", p)
    ) %>% 
  write.table(file = "tables/table_1.txt", sep = ",", quote = FALSE, row.names = FALSE)

read_csv("data/interim/all_data_avg.csv", col_types = cols(RSA = col_double())) %>%
  group_by(Method) %>% 
  summarize(
    expr_R = sqrt(rsquared(expr_avg_all, ddG)),
    bind_R = sqrt(rsquared(bind_avg_all, ddG)),
    expr_p = p_value(expr_avg_all, ddG),
    bind_p = p_value(bind_avg_all, ddG)
  ) %>% 
  filter(Method != "Simba_SYM") %>% 
  pivot_longer(c(expr_R:bind_p), names_to = c("Parameter", "Metric"), names_sep = "_") %>% 
  pivot_wider(names_from = Metric, values_from = value) %>% 
  select(Parameter, everything()) %>% 
  arrange(Parameter) %>% 
  mutate(
    Parameter = ifelse(Parameter == "bind", "Binding", "Expression"), 
    R = round(R, 2),
    p = ifelse(p < 0.001, "<0.001", p)
  )  %>% 
  write.table(file = "tables/table_2.txt", sep = ",", quote = FALSE, row.names = FALSE)

read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double())) %>%
  filter(Method %in% c("Simba_IB", "mCSM")) %>%
  group_by(PDB, Method) %>% 
  summarize(
    expr_R = sqrt(rsquared(expr_avg, RSA)),
    bind_R = sqrt(rsquared(bind_avg, RSA)),
    expr_p = p_value(expr_avg, RSA),
    bind_p = p_value(bind_avg, RSA)
  ) %>%
  pivot_longer(c(expr_R:bind_p), names_to = c("Parameter", "Metric"), names_sep = "_") %>% 
  pivot_wider(names_from = Metric, values_from = value) %>% 
  select(Parameter, PDB, everything()) %>% 
  arrange(Parameter, PDB) %>% 
  mutate(
    Parameter = ifelse(Parameter == "bind", "Binding", "Expression"), 
    R = round(R, 2),
    p = ifelse(p < 0.001, "<0.001", p)
  )%>% 
  write.table(file = "tables/table_3.txt", sep = ",", quote = FALSE, row.names = FALSE)

read_csv("data/interim/all_data_avg.csv", col_types = cols(RSA = col_double())) %>%
  filter(Method %in% c("Simba_IB", "mCSM")) %>%
  group_by(Method) %>% 
  summarize(
    expr_R = sqrt(rsquared(expr_avg_all, RSA)),
    bind_R = sqrt(rsquared(bind_avg_all, RSA)),
    expr_p = p_value(expr_avg_all, RSA),
    bind_p = p_value(bind_avg_all, RSA)
  ) %>% 
  filter(Method != "Simba_SYM") %>% 
  pivot_longer(c(expr_R:bind_p), names_to = c("Parameter", "Metric"), names_sep = "_") %>% 
  pivot_wider(names_from = Metric, values_from = value) %>% 
  select(Parameter, everything()) %>% 
  arrange(Parameter) %>% 
  mutate(
    Parameter = ifelse(Parameter == "bind", "Binding", "Expression"), 
    R = round(R, 2),
    p = ifelse(p < 0.001, "<0.001", p)
  ) %>% 
  write.table(file = "tables/table_4.txt", sep = ",", quote = FALSE, row.names = FALSE)

read_csv("data/interim/all_data_binned.csv") %>%
  filter(!(mid < -3.3 & Parameter == "expr_avg")) %>% 
  group_by(Parameter, PDB, Method, group) %>% 
  summarize(
    R = sqrt(krisr::rsquared(mid, mean_ddg)),
    p = p_value(mid, mean_ddg)
  ) %>% 
  filter(Method != "Simba_SYM") %>% 
  mutate(
    Parameter = ifelse(Parameter == "bind_avg", "Binding", "Expression"), 
    Binning = ifelse(group == "group_1", "0.5 wide bins", "0.25 wide bins"),
    R = round0(R, 2),
    p = ifelse(p < 0.001, "<0.001", round0(p, 3))
  ) %>% 
  select(Parameter, PDB, Method, Binning, R, p) %>% 
  arrange(Parameter, PDB, Method, Binning) %>% 
  write.table(file = "tables/table_5.txt", sep = ",", quote = FALSE, row.names = FALSE)

read_csv("data/interim/all_data_avg_binned.csv") %>%
  filter(!(mid < -3.3 & Parameter == "expr_avg_all")) %>% 
  group_by(Parameter, Method, group) %>% 
  summarize(
    R = sqrt(krisr::rsquared(mid, mean_ddg)),
    p = p_value(mid, mean_ddg)
  ) %>% 
  filter(Method != "Simba_SYM") %>% 
  arrange(Parameter, Method) %>% 
  mutate(
    Parameter = ifelse(Parameter == "bind_avg_all", "Binding", "Expression"), 
    Binning = ifelse(group == "group_1", "0.5 wide bins", "0.25 wide bins"),
    R = round0(R, 2),
    p = ifelse(p < 0.001, "<0.001", round0(p, 3))
  ) %>% 
  select(Parameter, Method, Binning, R, p) %>% 
  arrange(Parameter, Method, Binning) %>% 
  write.table(file = "tables/table_6.txt", sep = ",", quote = FALSE, row.names = FALSE)