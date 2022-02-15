all_data_avg <- read_csv("data/interim/all_data_avg.csv")

param_names <- c(
  "bind_avg_all" = "Binding",
  "expr_avg_all" = "Expression"
)

all_data_avg %>% 
  filter(Method %in% c("Simba_IB")) %>%
  select(-ddG) %>% 
  pivot_longer(c(expr_avg_all, bind_avg_all), names_to = "Parameter") %>% 
  ggplot() +
  geom_point(aes(value, RSA), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(value, RSA), method = "lm", se = FALSE, color = "orange") +
  facet_grid(~Parameter, scales = "free", labeller = labeller(Parameter = param_names)) +
  labs(
    x = "Value",
    y = "RSA"
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

ggsave("figures/Figure_S7.png", width = 13, height = 7, units = "cm", dpi = 600)
