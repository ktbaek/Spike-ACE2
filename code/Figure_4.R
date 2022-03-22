all_data <- read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double()))

p_value <- function(x,y) {
  m <- lm(y ~ x)
  return(summary(m)$coefficients[2,4])
}

p1 <- all_data %>% 
  filter(Method != "Simba_SYM") %>% 
  group_by(PDB, Method) %>% 
  mutate(
    R = sqrt(rsquared(expr_avg, ddG)),
    p = p_value(expr_avg, ddG)
  ) %>% 
  ggplot() +
  geom_point(aes(expr_avg, ddG), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(expr_avg, ddG), method = "lm", se = FALSE, color = "orange") +
  geom_text(
    aes(
      x = -2.4, 
      y = 4.3, 
      label = paste0("R = ", round0(R, 2))
    ),
    size = rel(2.8),
    check_overlap = TRUE) +
  scale_y_continuous(limits = c(NA, 4.7)) +
  facet_grid(Method~PDB, scales = "free_x") +
  labs(
    x = "Expression",
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )


p2 <- all_data %>% 
  filter(Method != "Simba_SYM") %>% 
  group_by(PDB, Method) %>% 
  mutate(
    R = sqrt(rsquared(bind_avg, ddG)),
    p = p_value(bind_avg, ddG)
  ) %>% 
  ggplot() +
  geom_point(aes(bind_avg, ddG), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(bind_avg, ddG), method = "lm", se = FALSE, color = "orange") +
  geom_text(
    aes(
      x = -2.7, 
      y = 4.3, 
      label = paste0("R = ", round0(R, 2))
    ),
    size = rel(2.8),
    check_overlap = TRUE) +
  scale_y_continuous(limits = c(NA, 4.7)) +
  facet_grid(Method~PDB, scales = "free_x") +
  labs(
    x = "Binding",
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

p1 / p2 + plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 14))

ggsave("figures/Figure_4.png", width = 20, height = 22, units = "cm", dpi = 600)
