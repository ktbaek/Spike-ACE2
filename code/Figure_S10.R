all_data_avg <- read_csv("data/interim/all_data_avg.csv")

p_value <- function(x,y) {
  m <- lm(y ~ x)
  return(summary(m)$coefficients[2,4])
}


RSA_df <- all_data_avg %>% 
  filter(Method == "Simba_IB") %>% 
  select(Number, RSA) %>% 
  distinct()

all_data_avg %>% 
  select(-RSA, -expr_avg_all, -bind_avg_all) %>% 
  left_join(RSA_df, by = "Number") %>% 
  filter(Method != "Simba_SYM") %>% 
  group_by(Method) %>% 
  mutate(
    R = round0(sqrt(rsquared(RSA, ddG)), 2),
    p = case_when(
      round0(p_value(RSA, ddG), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(RSA, ddG), 2))
    )
  ) %>% 
  ggplot() +
  geom_point(aes(RSA, ddG), size = 0.7, stroke = 0, alpha = 0.2) +
  geom_smooth(aes(RSA, ddG), method = "lm", se = FALSE, color = "orange") +
  geom_text(
    aes(
      x = 1, 
      y = -5.4, 
      label = paste0("R=", R, "\np", p)
    ),
    size = rel(3),
    check_overlap = TRUE,
    hjust = 1
  ) +
  facet_grid(~Method, scales = "free") +
  labs(
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)"),
    x = "RSA"
  ) +
  theme(
    strip.text.y = element_text(angle = 0),
    panel.spacing.x = unit(0.4, "cm")
  )

ggsave("figures/Figure_S10.png", width = 15, height = 6, units = "cm", dpi = 600)
