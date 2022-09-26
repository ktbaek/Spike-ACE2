read_simba <- function(filepath){
  
  method <- ifelse(str_detect(filepath, "SYM"), "Simba_SYM", "Simba_IB")
  
  read_csv(filepath) %>% 
    rename(ddG = 10) %>% 
    mutate(Method = method) 
  
}

simba_df_all <- lapply(list.files("data/external/simba2", full.names = TRUE), read_simba) %>%
  bind_rows()

simba_df_all %>% 
  group_by(PDB, Wild, Number, Mutated, Method) %>% 
  summarize(
    ddG = mean(ddG, na.rm = TRUE),
    RSA = mean(RSA, na.rm = TRUE)
    ) %>% 
  write_csv("data/interim/tidy_simba.csv")

# Confirm that the structures are homooligomers
simba_df_all %>% 
  group_by(PDB, Number) %>%
  mutate(identical = n_distinct(Wild) == 1) %>% 
  group_by(PDB) %>% 
  summarize(homo = all(identical))