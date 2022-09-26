read_deep <- function(filepath){
  
  file_content <- read_lines(filepath,skip = 1)
  
  pdb <- str_to_upper(str_sub(filepath, 23, 26))
  
  file_content %<>% 
    str_squish() %>% 
    strsplit(split = " ")
  
  df <- tibble(data.frame(matrix(unlist(file_content), nrow = length(file_content), byrow=T)))
  
  df %>% 
    rename(
      Chain = X1,
      Wild = X2,
      Number = X3,
      Mutated = X4,
      ddG = X5
    ) %>% 
    mutate(
      PDB = pdb,
      Number = as.integer(Number),
      ddG = as.double(ddG),
      Method = "DeepDDG"
    ) %>% 
    select(PDB, everything())
  
}

deep_df_all <- lapply(list.files("data/external/deepddg", full.names = TRUE), read_deep) %>%
  bind_rows()

deep_df_all %>% 
  group_by(PDB, Wild, Number, Mutated, Method) %>% 
  summarize(ddG = mean(ddG, na.rm = TRUE)) %>% 
  write_csv("data/interim/tidy_deepddg.csv")