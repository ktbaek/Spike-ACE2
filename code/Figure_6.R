binned_data <- read_csv("data/interim/all_data_avg_binned.csv")

p_value <- function(x,y) {
  m <- lm(y ~ x)
  return(summary(m)$coefficients[2,4])
}

param_names <- c(
  "bind_avg_all" = "Binding",
  "expr_avg_all" = "Expression"
)

  
plot_layer <- list(
  geom_pointrange(aes(x = mid, y = mean_ddg, ymin = mean_ddg - sd_ddg, ymax = mean_ddg + sd_ddg),
                  stroke = 0, size = 0.3),
  geom_smooth(aes(mid, mean_ddg), method = "lm", se = FALSE, color = "orange"),
  geom_text(
    aes(
      x = -1, 
      y = -5.3, 
      label = paste0("R=", R, " p", p)
    ),
    size = rel(2.3),
    check_overlap = TRUE),
  facet_grid(Method~Parameter, labeller = labeller(Parameter = param_names)),
  scale_x_continuous(limits = c(NA, 0.7)),
  scale_y_continuous(limits = c(-5.7, 1.7)),
  labs(
    x = expression(Effect~of~mutation[Binned]),
    y = expression(Delta*Delta*G[Pred]~"(kcal/mol)")
  ),
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = rel(2)),
    strip.text.y = element_text(angle = 0),
    panel.spacing = unit(0.3, "cm")
  )
)

p1 <- binned_data %>%  
  filter(
    Method != "Simba_SYM",
    group == "group_1",
    !(mid < -3.3 & Parameter == "expr_avg_all")) %>% 
  group_by(Method, Parameter) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_ddg)), 2),
    p = case_when(
      round0(p_value(mid, mean_ddg), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_ddg), 2))
    )
  ) %>% 
  ggplot() +
  plot_layer

p2 <- binned_data %>%  
  filter(
    Method != "Simba_SYM",
    group == "group_2",
    !(mid < -3.3 & Parameter == "expr_avg_all")) %>% 
  group_by(Method, Parameter) %>% 
  mutate(
    R = round0(sqrt(rsquared(mid, mean_ddg)), 2),
    p = case_when(
      round0(p_value(mid, mean_ddg), 2) == "0.00" ~ "<0.005",
      TRUE ~ paste0("=", round0(p_value(mid, mean_ddg), 2))
    )
  ) %>% 
  ggplot() +
  plot_layer

p1 + p2 + plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 12))

ggsave("figures/Figure_6.png", width = 19, height = 10, units = "cm", dpi = 600)
