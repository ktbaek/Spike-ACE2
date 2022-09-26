all_data <- read_csv("data/interim/all_data.csv", col_types = cols(RSA = col_double()))

analyze_pairs <- function(data, var1, var2, name1, name2) {
  data %>%
    filter(Method %in% c(name1, name2)) %>%
    select(PDB, Wild, Number, Mutated, Method, ddG) %>%
    pivot_wider(names_from = Method, values_from = ddG) %>%
    group_by(PDB) %>%
    mutate(R = sqrt(rsquared({{ var1 }}, {{ var2 }})))
}

plot_pairs <- function(data, var1, var2, name1, name2, text_x = 4.7, text_y = -6.5, h = 1, v = 0) {
  data %>%
    ggplot() +
    geom_point(aes({{ var1 }}, {{ var2 }}), stroke = 0, alpha = 0.2) +
    geom_smooth(aes({{ var1 }}, {{ var2 }}), method = "lm", se = FALSE, color = "orange") +
    geom_text(
      aes(
        x = text_x,
        y = text_y,
        hjust = h,
        vjust = v,
        label = paste0("R = ", round0(R, 2))
      ),
      size = rel(3.2),
      check_overlap = TRUE
    ) +
    scale_x_continuous(limits = c(-6.5, 4.7), breaks = c(-5, 0, 5)) +
    scale_y_continuous(limits = c(-6.5, 4.7)) +
    facet_wrap(~PDB, ncol = 8, scales = "fixed") +
    labs(
      x = bquote(.(name1) ~ Delta * Delta * G[Pred] ~ "(kcal/mol)"),
      y = bquote(atop(.(name2), Delta * Delta * G[Pred] ~ "(kcal/mol)"))
    ) +
    coord_fixed() +
    theme(
      strip.text.y = element_text(angle = 0)
    )
}

p1 <- all_data %>%
  analyze_pairs(DeepDDG, mCSM, "DeepDDG", "mCSM") %>%
  plot_pairs(DeepDDG, mCSM, "DeepDDG", "mCSM")

p2 <- all_data %>%
  analyze_pairs(mCSM, Simba_IB, "mCSM", "Simba_IB") %>%
  plot_pairs(mCSM, Simba_IB, "mCSM", "Simba_IB")

p3 <- all_data %>%
  analyze_pairs(Simba_IB, DeepDDG, "Simba_IB", "DeepDDG") %>%
  plot_pairs(Simba_IB, DeepDDG, "Simba_IB", "DeepDDG", h = 0, v = 1, text_x = -6.5, text_y = 4.7)

p1 / p2 / p3

ggsave("figures/Figure_S8.png", width = 22, height = 13, units = "cm", dpi = 600)
