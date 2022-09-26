bloom <- read_csv("data/raw/1-s2.0-S0092867420310035-mmc2.csv")
bloom_2 <- read_csv("data/processed/bloom_clean.csv")

param_names = c(
  "bind_avg" = "Binding",
  "expr_avg" = "Expression"
)

amager <- c("#b79128", "#006d86", "#79a039", "#e0462e", "#004648", "#1c6ac9", '#fc981e')

p1 <- bloom %>% 
  mutate(type = case_when(
    mutant == "*" ~ "Stop codon",
    wildtype == mutant ~ "Synonymous",
    TRUE ~ "Nonsynonymous"
  )) %>% 
  pivot_longer(c(bind_avg, expr_avg), names_to = "parameter", "value") %>% 
  ggplot() +
  geom_histogram(aes(value, fill = type), alpha = 0.8, position = "stack") +
  scale_y_continuous(expand = expansion(mult = c(0.003, 0.1))) +
  scale_fill_manual(name = "Mutation type", values = amager[c(6, 5, 7)]) +
  labs(
    x = "Value",
    y = "Count") +
  facet_wrap(~ parameter, scales = "free", labeller = labeller(parameter = param_names)) 

p2 <- bloom_2 %>% 
  mutate(type = case_when(
    mutant == "*" ~ "Stop codon",
    wildtype == mutant ~ "Synonymous",
    TRUE ~ "Nonsynonymous"
  )) %>% 
  pivot_longer(c(bind_avg, expr_avg), names_to = "parameter", "value") %>% 
  ggplot() +
  geom_histogram(aes(value, fill = type), alpha = 0.8, position = "stack") +
  scale_y_continuous(expand = expansion(mult = c(0.003, 0.1))) +
  scale_fill_manual(guide = FALSE, name = "Mutation type", values = amager[6:7]) +
  labs(
    x = "Value",
    y = "Count") +
  facet_wrap(~ parameter, scales = "free", labeller = labeller(parameter = param_names)) 
  

p1 / p2 + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 14)) &
  theme(legend.position = "bottom")

ggsave("figures/Figure_S2.png", width = 15, height = 14, units = "cm", dpi = 600)
