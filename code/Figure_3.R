all_data_avg <- read_csv("data/interim/all_data_avg.csv")

param_names <- c(
  "bind_avg_all" = "Binding",
  "expr_avg_all" = "Expression"
)

all_data_avg %>% 
  filter(Method != "Simba_SYM") %>% 
  select(-RSA) %>% 
  pivot_longer(c(expr_avg_all, bind_avg_all), names_to = "Parameter") %>% 
  ggplot() +
  geom_point(aes(value, ddG), size = 0.6, stroke = 0, alpha = 0.2) +
  geom_smooth(aes(value, ddG), method = "lm", se = FALSE, color = "orange") +
  facet_grid(Method~Parameter, scales = "free", labeller = labeller(Parameter = param_names)) +
  labs(
    x = "Value",
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

ggsave("figures/Figure_3.png", width = 15, height = 10, units = "cm", dpi = 600)
