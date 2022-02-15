bloom <- read_csv("data/raw/1-s2.0-S0092867420310035-mmc2.csv")
bloom_2 <- read_csv("data/processed/bloom_clean.csv")

param_names = c(
  "bind" = "Binding",
  "expr" = "Expression"
)

p1 <- bloom %>% 
  pivot_longer(
    ends_with(c("lib1", "lib2")), 
    names_to = c("parameter", "library"), 
    "value",
    names_sep = "_") %>% 
  pivot_wider(names_from = library, values_from = value) %>% 
  ggplot() +
  geom_point(aes(lib1, lib2), size = 2, alpha = 0.1) +
  geom_smooth(aes(lib1, lib2), method = "lm", se = FALSE, color = "orange") +
  labs(
    x = "Library 1",
    y = "Library 2"
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = labeller(parameter = param_names)) 

p2 <- bloom_2 %>% 
  pivot_longer(
    ends_with(c("lib1", "lib2")), 
    names_to = c("parameter", "library"), 
    "value",
    names_sep = "_") %>% 
  pivot_wider(names_from = library, values_from = value) %>% 
  ggplot() +
  geom_point(aes(lib1, lib2), size = 2, alpha = 0.1) +
  geom_smooth(aes(lib1, lib2), method = "lm", se = FALSE, color = "orange") +
  labs(
    x = "Library 1",
    y = "Library 2"
  ) + 
  facet_wrap(~ parameter, scales = "free", labeller = labeller(parameter = param_names)) 

p1 / p2 + plot_annotation(tag_levels= "A")


ggsave("figures/Figure_S1.png", width = 15, height = 15, units = "cm", dpi = 600)
