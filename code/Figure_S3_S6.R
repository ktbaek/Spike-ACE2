binned_data <- read_csv("data/interim/all_data_binned.csv")
  
plot_layer <- list(
  geom_pointrange(aes(x = mid, y = mean_ddg, ymin = mean_ddg - sd_ddg, ymax = mean_ddg + sd_ddg),
                  stroke = 0, size = 0.3),
  geom_smooth(aes(mid, mean_ddg), method = "lm", se = FALSE, color = "orange"),
  facet_grid(Method~PDB),
  labs(y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")),
  theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = rel(2)),
      strip.text.y = element_text(angle = 0)
    )
)

binned_data %>%  
  filter(
    Parameter == "bind_avg",
    Method != "Simba_SYM",
    group == "group_1"
    ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Binding[Binned]))
 
ggsave("figures/Figure_S5.png", width = 16, height = 11, units = "cm", dpi = 600)

binned_data %>%  
  filter(
    Parameter == "bind_avg",
    Method != "Simba_SYM",
    group == "group_2"
  ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Binding[Binned]))

ggsave("figures/Figure_S6.png", width = 16, height = 11, units = "cm", dpi = 600)

binned_data %>%  
  filter(
    Parameter == "expr_avg",
    group == "group_1",
    mid > -3.3,
    Method != "Simba_SYM",
    ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Expression[Binned]))

ggsave("figures/Figure_S3.png", width = 16, height = 11, units = "cm", dpi = 600)

binned_data %>%  
  filter(
    Parameter == "expr_avg",
    group == "group_2",
    mid > -3.3,
    Method != "Simba_SYM",
  ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Expression[Binned]))

ggsave("figures/Figure_S4.png", width = 16, height = 11, units = "cm", dpi = 600)

  