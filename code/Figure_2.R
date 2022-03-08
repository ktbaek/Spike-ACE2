all_data_avg <- read_csv("data/interim/all_data_avg.csv")

plot_data <- all_data_avg %>% 
  filter(Method != "Simba_SYM") %>% 
  select(-RSA) %>% 
  mutate(
    Wild = str_sub(mutation, 1, 1),
    Mutated = str_sub(mutation, 5, 5)
  )

dots <- plot_data %>% 
  select(Number, Wild, Mutated) %>% 
  distinct()

plot_layer <- list( 
  geom_point(data = dots, aes(Number, Mutated, size=ifelse(Wild == Mutated, "dot", "no_dot"))),
  scale_size_manual(values=c(dot=0.2, no_dot=NA), guide="none"),
  scale_x_continuous(expand = expansion(mult = 0.01)),
  labs(
    x = "Site",
    y = "Mutant"
  ),
  theme(
    panel.grid = element_blank()
  )
)

p1 <- plot_data %>% 
  ggplot() +
  geom_tile(aes(Number, Mutated, fill = expr_avg_all)) +
  scale_fill_gradientn(name = expression(Delta*log(MFI)), 
                       colors = c("#dd8a0b", "gray95", "blue"),
                       values = c(0, 0.86, 1)) +
  
  plot_layer +
  labs(title = "RBD expression") 

p5 <- plot_data %>% 
  ggplot() +
  geom_tile(aes(Number, Mutated, fill = bind_avg_all)) +
  scale_fill_gradientn(name = expression(Delta*log(italic(K["D, app"]))), 
                       colors = c("#dd8a0b", "gray95", "blue"),
                       values = c(0, 0.94, 1)) +
  
  plot_layer +
  labs(title = "ACE2 binding") 

p2 <- plot_data %>% 
  filter(Method == "DeepDDG") %>% 
  ggplot() +
  geom_tile(aes(Number, Mutated, fill = ddG)) +
  scale_fill_gradientn(name = expression(Delta*Delta*G[Pred]~"(kcal/mol)"),
                       colors = c("#dd8a0b", "gray95", "blue"),
                       values = c(0, 0.69, 1)) +
  plot_layer +
  labs(title = "Protein stability, DeepDDG") 

p3 <- plot_data %>% 
  filter(Method == "mCSM") %>% 
  ggplot() +
  geom_tile(aes(Number, Mutated, fill = ddG)) +
  scale_fill_gradientn(name = expression(Delta*Delta*G[Pred]~"(kcal/mol)"),
                       colors = c("#dd8a0b", "gray95", "blue"),
                       values = c(0, 0.61, 1)) +
  plot_layer +
  labs(title = "Protein stability, mCSM") 

p4 <- plot_data %>% 
  filter(Method == "Simba_IB") %>% 
  ggplot() +
  geom_tile(aes(Number, Mutated, fill = ddG)) +
  scale_fill_gradientn(name = expression(Delta*Delta*G[Pred]~"(kcal/mol)"),
                       colors = c("#dd8a0b", "gray95", "blue"),
                       values = c(0, 0.58, 1)) +
  plot_layer +
  labs(title = "Protein stability, SimBa-IB") 

p1 / p5 / p2 / p3 / p4 + plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 16))

ggsave("figures/Figure_2.png", width = 30, height = 42, units = "cm", dpi = 600)
