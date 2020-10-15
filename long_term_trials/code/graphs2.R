source("code/libraries.R")

load("data/d.yield.RData")
load("data/d.prepared.RData")
load(here::here("data", "d.yield.RData"))


# Non-aggregated yield data

## Plotting interaction of treatment with EI by year

d.yield.nonagg %>%
  filter(!is.na(Fertilizer)) %>% # Change for each treatment
  ggplot(mapping = aes(x = EI.by.year.weighted, y = Yield.weighted)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, mapping = aes(group = Fertilizer, color = Fertilizer))

d.yield.nonagg %>%
  filter(!is.na(Rotation)) 
  
## Plotting interaction of treatment with SPEI

test <-d.yield.nonagg %>%
  filter(!is.na(Rotation)) %>%
  filter(SPEI.1 != "-Inf") %>%
  filter(SPEI.1 >= (mean(.$SPEI.1) - 2*sd(.$SPEI.1)) & SPEI.1 < (mean(.$SPEI.1) - sd(.$SPEI.1))) %>%
  ggplot(data = ., aes(x = Rotation, y = Yield.weighted, fill = Rotation)) +
  geom_boxplot() + 
  theme(legend.position = "none")+
  ylab("Weighted yield")

# Carbon and yield stability metrics

d.nonirr %>%
  # filter(Paper != "Thierfelder and Wall 2012", # Sensitive papers
  #        Paper != "Gao et al. 2015") %>%
  ggplot(mapping = aes(x = SOC.g.kg.weighted, y = MYP.weighted.percent)) +
  geom_point(aes(size = CV.yield.weighted), alpha = 1/3) +
  geom_smooth(method = "lm", formula = y ~ x)


