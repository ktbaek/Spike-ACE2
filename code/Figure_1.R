read_csv("data/processed/bloom_clean.csv") %>% 
  mutate(R = sqrt(rsquared(expr_avg, bind_avg))) %>% 
  ggplot() +
  geom_point(aes(expr_avg, bind_avg), size = 2, alpha = 0.1) +
  geom_smooth(aes(expr_avg, bind_avg), method = "lm", se = FALSE, color = "orange") +
  geom_text(aes(x = -3.6, y = 0.3, label = paste0("R = ", round(R, 2))), check_overlap = TRUE) +
  labs(
    x = expression(Expression[Avg]),
    y = expression(Binding[Avg])
  )

ggsave("figures/Figure_1.png", width = 10, height = 8, units = "cm", dpi = 600)