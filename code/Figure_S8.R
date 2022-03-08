all_data_avg <- read_csv("data/interim/all_data_avg.csv")
binned_data <- read_csv("data/interim/all_data_avg_binned_RSA.csv")

p_value <- function(x,y) {
  m <- lm(y ~ x)
  return(summary(m)$coefficients[2,4])
}

param_names <- c(
  "bind_avg_all" = "Binding",
  "expr_avg_all" = "Expression"
)

p1 <- all_data_avg %>% 
  filter(Method == "Simba_IB") %>%
  select(-ddG) %>% 
  pivot_longer(c(expr_avg_all, bind_avg_all), names_to = "Parameter") %>% 
  group_by(Parameter) %>% 
  mutate(
    R = round0(sqrt(rsquared(RSA, value)), 2),
    p = case_when(
      round0(p_value(RSA, value), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(RSA, value), 2))
    )
  ) %>% 
  ggplot() +
  geom_point(aes(RSA, value), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(RSA, value), method = "lm", se = FALSE, color = "orange") +
  geom_text(
    aes(
      x = 1, 
      y = -4.3, 
      label = paste0("R=", R, " p", p)
    ),
    size = rel(3),
    check_overlap = TRUE,
    hjust = 1
  ) +
  facet_grid(~Parameter, scales = "free", labeller = labeller(Parameter = param_names)) +
  labs(
    y = expression(Effect~of~mutation),
    x = "RSA"
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

p2 <- binned_data %>%  
  group_by(Method, Parameter) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_value)), 2),
    p = case_when(
      round0(p_value(mid, mean_value), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_value), 2))
    )
  ) %>% 
  ggplot() +
  geom_pointrange(aes(x = mid, y = mean_value, ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  stroke = 0, size = 0.5) +
geom_smooth(aes(mid, mean_value), method = "lm", se = FALSE, color = "orange") +
geom_text(
  aes(
    x = 1, 
    y = -2.3, 
    label = paste0("R=", R, " p", p)
  ),
  size = rel(3),
  check_overlap = TRUE,
  hjust = 1
  ) +
  facet_grid(~Parameter, scales = "free", labeller = labeller(Parameter = param_names)) +
labs(
  x = expression(RSA[Binned]),
  y = "Effect of mutation"
) +
theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = rel(2))
)


p1 / p2 + plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 14))


ggsave("figures/Figure_S8.png", width = 14, height = 14, units = "cm", dpi = 600)
