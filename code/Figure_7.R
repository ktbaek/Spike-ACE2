ddg <- read_csv("data/interim/all_data_avg_ddG.csv")
eb <- read_csv("data/interim/all_data_avg_expr_bind.csv")

p_value <- function(x,y) {
  m <- lm(y ~ x)
  return(summary(m)$coefficients[2,4])
}

param_names <- c(
  "bind" = "Binding",
  "expr" = "Expression"
)

ddg %>% 
  full_join(eb, by = "Number") %>% 
  filter(Method != "Simba_SYM") %>%
  pivot_longer(c(expr, bind), names_to = "Parameter") %>% 
  mutate(
    ddG = as.double(ddG),
    value = as.double(value)
  ) %>% 
  group_by(Method, Parameter) %>% 
  mutate(
    R = round0(sqrt(rsquared(value, ddG)), 2),
    p = case_when(
      round0(p_value(value, ddG), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(value, ddG), 2))
    )
  ) %>% 
  mutate(ypos = case_when(
    Method == "DeepDDG" ~ 5.2,
    Method == "mCSM" ~ 3.5,
    Method == "Simba_IB" ~ 3.5
  )) %>% 
  ggplot() +
  geom_point(aes(value, ddG), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(value, ddG), method = "lm", se = FALSE, color = "orange") +
  geom_text(
    aes(
      x = 0, 
      y = ypos, 
      label = paste0("R=", R, " p", p)
    ),
    size = rel(3),
    check_overlap = TRUE,
    hjust = 0,
    vjust = 1
  ) +
  facet_grid(Method~Parameter, scales = "free_y", labeller = labeller(Parameter = param_names)) +
  labs(
    x = "Mean absolute value per site",
    y = expression(Mean~absolute~Delta*Delta*G[Pred]~"(kcal/mol)"~per~site)
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

ggsave("figures/Figure_7.png", width = 13, height = 10, units = "cm", dpi = 600)

