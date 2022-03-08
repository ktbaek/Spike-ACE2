all_data_avg <- read_csv("data/interim/all_data_avg.csv")

p_value <- function(x,y) {
  m <- lm(y ~ x)
  return(summary(m)$coefficients[2,4])
}

param_names <- c(
  "bind_avg_all" = "Binding",
  "expr_avg_all" = "Expression"
)

all_data_avg %>% 
  filter(Method != "Simba_SYM") %>% 
  select(-RSA) %>% 
  pivot_longer(c(expr_avg_all, bind_avg_all), names_to = "Parameter") %>% 
  group_by(Method, Parameter) %>% 
  mutate(
    R = round0(sqrt(rsquared(value, ddG)), 2),
    p = case_when(
      round0(p_value(value, ddG), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(value, ddG), 2))
    )
  ) %>% 
  mutate(
    hpos = ifelse(Method == "DeepDDG", 0, 1),
    xpos = ifelse(Method == "DeepDDG", -4.6, 0.7),
    ypos = ifelse(Method == "DeepDDG", 1.6, -5)
  ) %>% 
  ggplot() +
  geom_point(aes(value, ddG), size = 0.6, stroke = 0, alpha = 0.2) +
  geom_smooth(aes(value, ddG), method = "lm", se = FALSE, color = "orange") +
  geom_text(
    aes(
      x = xpos, 
      y = ypos, 
      label = paste0("R=", R, "\np", p),
      hjust = hpos
    ),
    size = rel(2.5),
    check_overlap = TRUE) +
  facet_grid(Method~Parameter, labeller = labeller(Parameter = param_names)) +
  labs(
    x = "Value",
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

ggsave("figures/Figure_4.png", width = 15, height = 10, units = "cm", dpi = 600)
