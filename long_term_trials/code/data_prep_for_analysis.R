source("code/libraries.R")

load("data/d.carbon.summary.RData")
load("data/d.yield.stability.RData")

d.yield.stability <- d.yield.stability[d.yield.stability$MYP > -17000,]
d.yield.stability$Tillage[d.yield.stability$Tillage %in% "t"] <- "y"
d.yield.stability <- d.yield.stability[!is.na(d.yield.stability$Paper),]

d.carbon.trts <- read.csv("data/d.carbon.trts.csv")
str(d.carbon.trts)

d.carbon.summary %>%
  inner_join(d.carbon.trts[,c(1,10,11)]) -> d.carbon.summary

d.yield.stability %>%
  inner_join(d.carbon.summary[,c(1,3,4)]) %>%
  distinct(.) -> d.yield.stability.carbon

## Calculate LRRs

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Tillage == "n") %>%
  summarise(Tillage.SOC.control = mean(SOC.g.kg.weighted)) -> d.till.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Cover == "n") %>%
  summarise(Cover.SOC.control = mean(SOC.g.kg.weighted)) -> d.cover.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Org.amend == "n") %>%
  summarise(Org.amend.SOC.control = mean(SOC.g.kg.weighted)) -> d.org.amend.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Rotation == "n") %>%
  summarise(Rotation.SOC.control = mean(SOC.g.kg.weighted)) -> d.rotation.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Fertilizer == "n") %>%
  summarise(Fertilizer.SOC.control = mean(SOC.g.kg.weighted)) -> d.fertilizer.soc.control

d.yield.stability.carbon %>%
  left_join(d.till.soc.control) %>%
  left_join(d.cover.soc.control) %>%
  left_join(d.org.amend.soc.control) %>%
  left_join(d.rotation.soc.control) %>%
  left_join(d.fertilizer.soc.control) %>%
  mutate(
    Tillage.SOC.LRR = log(SOC.g.kg.weighted/Tillage.SOC.control),
    Cover.SOC.LRR = log(SOC.g.kg.weighted/Cover.SOC.control),
    Org.amend.SOC.LRR = log(SOC.g.kg.weighted/Org.amend.SOC.control),
    Rotation.SOC.LRR = log(SOC.g.kg.weighted/Rotation.SOC.control),
    Fertilizer.SOC.LRR = log(SOC.g.kg.weighted/Fertilizer.SOC.control)
  ) %>%
  distinct(.) -> d

d <- d[!is.na(d$MYP),]

d %>%
  group_by(Paper, Crop, Units) %>%
  mutate(Paper_crop_mean_yield = mean(Mean.yield),
         MYP_percent= MYP/Paper_crop_mean_yield) -> d

## Climate dimension reduction

# Want to evaluate climate variables for correlation before building LMEs
clim_vars_carbon <- d[,c(9:11, 13:16)] # excluding soil moisture for now
corr1 <- cor(clim_vars_carbon)
ggcorrplot::ggcorrplot(corr1, type = "lower", lab = TRUE)

clim_vars_yield <- d.yield.stability[,c(9:11, 13:16)] # excluding soil moisture again
corr2 <- cor(clim_vars_yield)
ggcorrplot::ggcorrplot(corr2, type = "lower", lab = TRUE)

# Lots of correlation between variables, will conduct PCA to reduce and build new climate predictor variables.
# Excluding soil moisture for now

climate.pca1 <- prcomp(clim_vars_carbon, center = TRUE, scale. = TRUE)
summary(climate.pca1)
factoextra::fviz_eig(climate.pca1)
# Three principal components may be sufficient for C dataset, fourth doesn't add much to variance explanation

climate.pca2 <- prcomp(clim_vars_yield, center = TRUE, scale. = TRUE)
summary(climate.pca2)
factoextra::fviz_eig(climate.pca2)
# Again, three PCs seems good for yield dataset

# Interpretation of principal components:

climate.pca1$rotation
climate.pca2$rotation

# PC1: temperature (max & min) and potential evapotranspiration
# PC2: precipitation and actual evapotranspiration
# PC3: drought severity

new_climate_carbon <- as.data.frame(climate.pca1$x[,1:3])
names(new_climate_carbon) <- c("clim_PC1", "clim_PC2", "clim_PC3")
d <- d %>%
  bind_cols(new_climate_carbon) %>%
  as.data.frame(.)
  

new_climate_yield <- as.data.frame(climate.pca2$x[,1:3])
names(new_climate_yield) <- c("clim_PC1", "clim_PC2", "clim_PC3")
d.yield.stability <- d.yield.stability %>%
  bind_cols(new_climate_yield) %>%
  as.data.frame(.)

save("d", "d.yield.stability", file = "data/d.prepared.RData")

