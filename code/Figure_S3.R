p1 <- read_csv("data/processed/bloom_clean.csv") %>% 
  mutate(R = sqrt(rsquared(expr_avg, bind_avg))) %>% 
  ggplot() +
  geom_point(aes(expr_avg, bind_avg), size = 2, alpha = 0.1) +
  geom_smooth(aes(expr_avg, bind_avg), method = "lm", se = FALSE, color = "orange") +
  geom_text(aes(x = -3.6, y = 0.1, label = paste0("R=", round0(R, 2))), check_overlap = TRUE) +
  labs(
    x = "Expression",
    y = "Binding" 
  )

p2 <- read_csv("data/processed/bloom_clean.csv") %>% 
  mutate( 
    group_1 = cut_width(expr_avg, 0.5, center = 0.25),
    group_1 = str_remove_all(group_1, "[()\\]\\[]"),
    group_2 = cut_width(expr_avg, 0.25, center = 0.125),
    group_2 = str_remove_all(group_2, "[()\\]\\[]")
  ) %>%
  rowwise() %>% 
  mutate(
    group_1 = as.double(str_split(group_1, ",")[[1]][1]) + 0.25,
    group_2 = as.double(str_split(group_2, ",")[[1]][1]) + 0.125,
  ) %>% 
  ungroup() %>% 
  pivot_longer(c(group_1:group_2), names_to = "group", values_to = "mid") %>% 
  group_by(group, mid) %>% 
  summarize(
    mean_bind_avg = mean(bind_avg, na.rm = TRUE),
    sd_bind_avg = sd(bind_avg, na.rm = TRUE)
  ) %>% 
  filter(
    group == "group_2",
    mid > -3.5) %>% 
  mutate(R = sqrt(rsquared(mid, mean_bind_avg))) %>% 
  ggplot() +
  geom_pointrange(aes(x = mid, y = mean_bind_avg, 
                      ymin = mean_bind_avg - sd_bind_avg, 
                      ymax = mean_bind_avg + sd_bind_avg),
                  stroke = 0, size = 0.6) + 
  geom_smooth(aes(mid, mean_bind_avg), method = "lm", se = FALSE, color = "orange") +
  geom_text(aes(x = -3, y = 0.3, label = paste0("R=", round0(R, 2))), check_overlap = TRUE) +
  labs(
    x = expression(Expression[Binned]),
    y = "Binding"
  )

p1 + p2 + plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 14))

ggsave("figures/Figure_S3.png", width = 18, height = 8, units = "cm", dpi = 600)