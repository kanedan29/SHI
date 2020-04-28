source("code/libraries.R")
load("data/d.prepared.RData")

d <- as.data.frame(d)

ggplot(d, mapping = aes(x = Tillage.SOC.LRR, y = MYP, color = Crop)) +
  geom_point()

d <- d %>%
  filter(Crop != "Biomass",
         Crop != "Maize biomass",
         Crop != "Cotton lint")

MYP_SOM <- ggplot(d, mapping = aes(x = SOC.g.kg.weighted, y = MYP, color = Crop)) +
  geom_point(mapping = aes(size = CV.yield)) + 
  labs(title = "SOM effects on MYP")
MYP_PC1 <- ggplot(d, mapping = aes(x = clim_PC1, y = MYP, color = Crop)) +
  geom_point(mapping = aes(size = CV.yield)) +
  labs(title = "Temperature effects on MYP")
MYP_PC2 <- ggplot(d, mapping = aes(x = clim_PC2, y = MYP, color = Crop)) +
  geom_point(mapping = aes(size = CV.yield)) +
  labs(title = "Precipitation & actual evapotranspiration effects on MYP")
MYP_PC3 <- ggplot(d, mapping = aes(x = clim_PC3, y = MYP, color = Crop)) +
  geom_point(mapping = aes(size = CV.yield)) +
  labs(title = "Drought severity effects on MYP")

ggpubr::ggarrange(MYP_SOM, MYP_PC1, MYP_PC2, MYP_PC3, ncol = 2, nrow = 2, common.legend = TRUE)
