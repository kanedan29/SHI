source("code/libraries.R")

load("data/d.yield.RData")
load("data/d.prepared.RData")

# Non-aggregated yield data

## Plotting interaction of treatment with EI by year

d.yield.nonagg %>%
  filter(!is.na(Fertilizer)) %>% # Change for each treatment
  ggplot(mapping = aes(x = EI.by.year.weighted, y = Yield.weighted)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, mapping = aes(group = Fertilizer, color = Fertilizer))

## Plotting interaction of treatment with SPEI

d.yield.nonagg %>%
  filter(!is.na(Rotation)) %>%
  ggplot(data = ., mapping = aes(x = SPEI.1, y = Yield.weighted)) +
  geom_point() +
  geom_smooth(mapping = aes(group = Rotation, color = Rotation))

# Carbon and yield stability metrics

d.nonirr %>%
  # filter(Paper != "Thierfelder and Wall 2012", # Sensitive papers
  #        Paper != "Gao et al. 2015") %>%
  ggplot(mapping = aes(x = delta.SOC.LRR, y = MYP.weighted.percent)) +
  geom_point(aes(size = CV.yield.weighted), alpha = 1/3) +
  geom_smooth(method = "lm", formula = y ~ x)


