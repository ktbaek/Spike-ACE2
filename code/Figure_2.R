all_data <- read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double()))

p1 <- all_data %>% 
  filter(Method != "Simba_SYM") %>% 
  ggplot() +
  geom_point(aes(expr_avg, ddG), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(expr_avg, ddG), method = "lm", se = FALSE, color = "orange") +
  facet_grid(Method~PDB, scales = "free") +
  labs(
    x = expression(Expression[Avg]),
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )


p2 <- all_data %>% 
  filter(Method != "Simba_SYM") %>% 
  ggplot() +
  geom_point(aes(bind_avg, ddG), stroke = 0, alpha = 0.2) +
  geom_smooth(aes(bind_avg, ddG), method = "lm", se = FALSE, color = "orange") +
  facet_grid(Method~PDB, scales = "free") +
  labs(
    x = expression(Binding[Avg]),
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

p1 / p2 + plot_annotation(tag_levels = "A")

ggsave("figures/Figure_2.png", width = 20, height = 22, units = "cm", dpi = 600)
