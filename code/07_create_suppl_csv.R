pretty_csv <- function(data) {
  data %>%
    filter(Method != "Simba_SYM") %>%
    mutate(ddG = krisr::round0(ddG, 2)) %>%
    pivot_wider(names_from = Method, values_from = ddG) %>%
    rename(
      Expression = starts_with("expr_avg"),
      Binding = starts_with("bind_avg"),
      Mutation = mutation
    )
}

read_csv("data/interim/all_data_avg.csv", col_types = cols(RSA = col_double())) %>%
  select(-RSA, -Number) %>%
  pretty_csv() %>%
  write_csv("data/processed/supplemental_datafile.csv")

read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double())) %>%
  select(PDB, Number, mutation, Method, ddG, expr_avg, bind_avg) %>%
  pretty_csv() %>%
  arrange(Number, Mutation) %>%
  write_csv("data/processed/supplemental_datafile_pdbs.csv")
