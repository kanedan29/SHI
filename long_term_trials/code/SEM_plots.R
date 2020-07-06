source(here::here("code", "libraries.R"))
source(here::here("code", "analysis_SEM.R"))

## Basic formatting attributes:
node_attrs <- list(
  label = c("Adjusted SOC LRR", "MYP (%)", "Conservation Practice - Fertilizer"), # adjust for each model
  fillcolor = "white",
  shape = "rectangle", color = "gray",
  x = c(2, 3.5, 1), y = c(2, 1.5, 1),
  fontsize = 8, fixedsize = FALSE
)

ggplot(d, mapping = aes(x = delta.Tillage.SOC.LRR, y = MYP_percent)) +
  geom_point()

## MYP_percent models

summary(fert.soclrr.myp_sem1)
rsquared(fert.soclrr.myp_sem1)

fert.myp_plot <- plot(fert.soclrr.myp_sem1, 
                          node_attrs = list(
                            label = c("Adjusted SOC LRR", 
                                      "MYP (%)", "Conservation Practice - \\n Fertilizer"),
                            fillcolor = "white",
                            shape = "rectangle", color = "gray",
                            x = c(3, 5, 1), y = c(2, 1, 1),
                            fontsize = 8, fixedsize = FALSE
                          ),
                          alpha = 0.05,
                          add_edge_label_spaces = TRUE
                      )

fert.myp_plot

summary(org.soclrr.myp_sem)
rsquared(org.soclrr.myp_sem)

org.myp_plot <- plot(org.soclrr.myp_sem, 
                      node_attrs = list(
                        label = c("Adjusted SOC LRR", "MYP (%)", "Conservation Practice - \\n Organic Amendments"),
                        fillcolor = "white",
                        shape = "rectangle", color = "gray",
                        x = c(3, 5, 1), y = c(2, 1, 1),
                        fontsize = 8, fixedsize = FALSE
                      ),
                      alpha = 0.05,
                      add_edge_label_spaces = TRUE
)

org.myp_plot

summary(till.soclrr.myp_sem)
rsquared(till.soclrr.myp_sem)

till.myp_plot <- plot(till.soclrr.myp_sem, 
                     node_attrs = list(
                       label = c("Adjusted SOC LRR", "MYP (%)", "Conservation Practice - \\n Tillage"),
                       fillcolor = "white",
                       shape = "rectangle", color = "gray",
                       x = c(3, 5, 1), y = c(2, 1, 1),
                       fontsize = 8, fixedsize = FALSE
                     ),
                     alpha = 0.05,
                     add_edge_label_spaces = TRUE
)

till.myp_plot

## CV models

summary(fert.soclrr.cv_sem)
rsquared(fert.soclrr.cv_sem)

fert.cv_plot <- plot(fert.soclrr.cv_sem, 
                      node_attrs = list(
                        label = c("Adjusted SOC LRR", "Yield CV", "Conservation Practice - \\n Fertilizer"),
                        fillcolor = "white",
                        shape = "rectangle", color = "gray",
                        x = c(3, 5, 1), y = c(2, 1, 1),
                        fontsize = 8, fixedsize = FALSE
                      ),
                      alpha = 0.05,
                      add_edge_label_spaces = TRUE
)


fert.cv_plot

summary(org.soclrr.cv_sem)
rsquared(org.soclrr.cv_sem)

org.cv_plot <- plot(org.soclrr.cv_sem, 
                     node_attrs = list(
                       label = c("Adjusted SOC LRR", "Yield CV", "Conservation Practice - \\n Organic Amendments"),
                       fillcolor = "white",
                       shape = "rectangle", color = "gray",
                       x = c(3, 5, 1), y = c(2, 1, 1),
                       fontsize = 8, fixedsize = FALSE
                     ),
                     alpha = 0.05,
                     add_edge_label_spaces = TRUE
)

org.cv_plot
