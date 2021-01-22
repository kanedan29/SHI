library(piecewiseSEM)
library(nlme)

d.sem <- d %>%
  filter(!is.na(Org.amend),
         Paper != "Moebius-Clune et al. 2008") %>%
  mutate(conprac = case_when(Control == "y" ~ 0, 
                             TRUE ~ 1))

test2 <- psem(
  lme(SOC.g.kg.weighted ~ conprac,
      random = ~1|Paper, na.action = "na.omit", data = d.sem, method = "ML"),
  lme(IR ~ SOC.g.kg.weighted+conprac,
      random = ~1|Paper, na.action = "na.omit", data = d.sem, method = "ML")
)

summary(test2)



plot(
  test2,
  node_attrs = list(
    fillcolor = "white",
    shape = "rectangle",
    color = "gray",
    x = c(3, 5, 1),
    y = c(2, 1, 1),
    fontsize = 8,
    fixedsize = FALSE
  ),
  alpha = 0.05, 
  add_edge_label_spaces = FALSE
)


