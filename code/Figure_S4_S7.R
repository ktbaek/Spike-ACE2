binned_data <- read_csv("data/interim/all_data_binned.csv")
  
plot_layer <- list(
  geom_pointrange(aes(x = mid, y = mean_ddg, ymin = mean_ddg - sd_ddg, ymax = mean_ddg + sd_ddg),
                  stroke = 0, size = 0.3),
  geom_smooth(aes(mid, mean_ddg), method = "lm", se = FALSE, color = "orange"),
  geom_text(
    aes(
      x = 0.7, 
      y = -5, 
      label = paste0("R=", R, "\np", p)
    ),
    size = rel(2.3),
    check_overlap = TRUE,
    hjust = 1),
  facet_grid(Method~PDB),
  scale_y_continuous(limits = c(-5.7, NA)),
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
  group_by(Method, PDB) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_ddg)), 2),
    p = case_when(
      round0(p_value(mid, mean_ddg), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_ddg), 2))
    )
  ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Binding[Binned]))
 
ggsave("figures/Figure_S6.png", width = 16, height = 11, units = "cm", dpi = 600)

binned_data %>%  
  filter(
    Parameter == "bind_avg",
    Method != "Simba_SYM",
    group == "group_2"
  ) %>% 
  group_by(Method, PDB) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_ddg)), 2),
    p = case_when(
      round0(p_value(mid, mean_ddg), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_ddg), 2))
    )
  ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Binding[Binned]))

ggsave("figures/Figure_S7.png", width = 16, height = 11, units = "cm", dpi = 600)

binned_data %>%  
  filter(
    Parameter == "expr_avg",
    group == "group_1",
    mid > -3.3,
    Method != "Simba_SYM",
    ) %>% 
  group_by(Method, PDB) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_ddg)), 2),
    p = case_when(
      round0(p_value(mid, mean_ddg), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_ddg), 2))
    )
  ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Expression[Binned]))

ggsave("figures/Figure_S4.png", width = 16, height = 11, units = "cm", dpi = 600)

binned_data %>%  
  filter(
    Parameter == "expr_avg",
    group == "group_2",
    mid > -3.3,
    Method != "Simba_SYM",
  ) %>% 
  group_by(Method, PDB) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_ddg)), 2),
    p = case_when(
      round0(p_value(mid, mean_ddg), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_ddg), 2))
    )
  ) %>% 
  ggplot() +
  plot_layer + 
  labs(x = expression(Expression[Binned]))

ggsave("figures/Figure_S5.png", width = 16, height = 11, units = "cm", dpi = 600)

  