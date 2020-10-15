library(piecewiseSEM)

d.till <- d %>%
  filter(!is.na(Tillage)) %>%
  mutate(Tillage = as.numeric(as.factor(Tillage)))

test <- psem(
  lme(Tillage.SOC.LRR ~ Tillage,
      random = ~1|Paper, na.action = "na.omit", data = d.till, method = "ML"),
  lme(Tillage.IR.LRR ~ Tillage.SOC.LRR + Tillage,
      random = ~1|Paper, na.action = "na.omit", data = d.till, method = "ML")
)

summary(test)
print(summary(test))

ggplot(data = d.till, aes(x=Tillage, y=Tillage.SOC.LRR))+
  geom_boxplot()



till.plot <- plot(test, 
                      node_attrs = list(
                        label = c("SOC LRR", 
                                  "Infiltration rate LRR", "Tillage, y/n"),
                        fillcolor = "white",
                        shape = "rectangle", color = "gray",
                        x = c(3, 5, 1), y = c(2, 1, 1),
                        fontsize = 8, fixedsize = FALSE,
                      ),
                      alpha = 0.05,
                      add_edge_label_spaces = FALSE
)

fert.myp_plot