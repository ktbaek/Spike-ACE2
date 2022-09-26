read_mcsm <- function(filepath){
  
  file_content <- read_lines(filepath,skip = 1)
  
  pdb <- str_to_upper(str_sub(filepath, 25, 28))
  
  file_content %<>% 
    str_squish() %>% 
    strsplit(split = " ")
  
  df <- tibble(data.frame(matrix(unlist(file_content), nrow = length(file_content), byrow=T)))
  
  df %>% 
    rename(
      PDB = X1,
      Chain = X2,
      Wild = X3,
      Number = X4,
      Mutated = X5,
      RSA = X6,
      ddG = X7
    ) %>% 
    mutate(
      PDB = pdb,
      Number = as.integer(Number),
      RSA = as.double(RSA),
      ddG = as.double(ddG),
      Method = "mCSM"
    ) %>% 
    select(PDB, everything())
  
}

mcsm_df_all <- lapply(list.files("data/external/mCSM", full.names = TRUE), read_mcsm) %>%
  bind_rows()

mcsm_df_all %>% 
  group_by(PDB, Wild, Number, Mutated, Method) %>% 
  summarize(
    ddG = mean(ddG, na.rm = TRUE),
    RSA = mean(RSA / 100, na.rm = TRUE)) %>% 
  write_csv("data/interim/tidy_mcsm.csv")