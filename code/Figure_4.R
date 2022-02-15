binned_data <- read_csv("data/interim/all_data_avg_binned.csv")

param_names <- c(
  "bind_avg_all" = "Binding",
  "expr_avg_all" = "Expression"
)

  
plot_layer <- list(
  geom_pointrange(aes(x = mid, y = mean_ddg, ymin = mean_ddg - sd_ddg, ymax = mean_ddg + sd_ddg),
                  stroke = 0, size = 0.3),
  geom_smooth(aes(mid, mean_ddg), method = "lm", se = FALSE, color = "orange"),
  facet_grid(Method~Parameter, labeller = labeller(Parameter = param_names)),
  labs(
    x = expression(Effect~of~mutation[Binned]),
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ),
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = rel(2)),
    strip.text.y = element_text(angle = 0)
  )
)

p1 <- binned_data %>%  
  filter(
    Method != "Simba_SYM",
    group == "group_1",
    !(mid < -3.3 & Parameter == "expr_avg_all")) %>% 
  ggplot() +
  plot_layer

p2 <- binned_data %>%  
  filter(
    Method != "Simba_SYM",
    group == "group_2",
    !(mid < -3.3 & Parameter == "expr_avg_all")) %>% 
  ggplot() +
  plot_layer

p1 + p2 + plot_annotation(tag_levels = "A")

ggsave("figures/Figure_4.png", width = 18, height = 10, units = "cm", dpi = 600)
