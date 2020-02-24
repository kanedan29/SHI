source("code/libraries.R")

load("data/d.prepared.RData")

mapping = c("y" = 1, "n" = 0)

d <- d %>%
  mutate_at(names(.)[18:24], function(i) mapping[i])

#########
## MYP ##
#########

m0_sem <- psem(lme(MYP ~ SOC.g.kg.weighted, random = ~1|Paper/Crop, data = d))
summary(m0_sem)

m1_sem <- psem(
  lme(SOC.g.kg.weighted ~ clim_PC1 + clim_PC2 + clim_PC3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  data = d)

summary(m1_sem)


m2_sem <- psem(
  lme(SOC.g.kg.weighted ~ clim_PC1 + clim_PC2 + clim_PC3, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted + clim_PC1 + clim_PC2 + clim_PC3, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d
)

summary(m2_sem, .progressBar = FALSE)

m3_sem <- psem(
  lme(SOC.g.kg.weighted ~ clim_PC1 + clim_PC2 + clim_PC3 + Tillage, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted + clim_PC1 + clim_PC2 + clim_PC3 + Tillage, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d
)

summary(m3_sem, .progressBar = FALSE)
plot(m3_sem)

m4_sem <- psem(
  lme(SOC.g.kg.weighted ~ clim_PC1 + clim_PC2 + clim_PC3 + Org.amend, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted + clim_PC1 + clim_PC2 + clim_PC3 + Org.amend, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d
)

summary(m4_sem, .progressBar = FALSE)

# Using unreduced climate variables, SOC influences climate effects
m5_sem <- psem(
  lme(Mean.def ~ SOC.g.kg.weighted, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ Mean.def, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d
)

summary(m5_sem)

plot(m5_sem)

# Leave out SOM as mediation
m6_sem <- psem(
  lme(MYP ~ SOC.g.kg.weighted + clim_PC1 + clim_PC2 + clim_PC3 + SOC.g.kg.weighted:clim_PC2, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d)

summary(m6_sem)

# Leave out climate
m7_sem <- psem(
  lme(SOC.g.kg.weighted ~ Tillage, random = ~1|Paper/Crop, 
      na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d
)

summary(m7_sem)

########
## CV ##
########

m8_sem <- psem(
  lme(clim_PC2 ~ SOC.g.kg.weighted, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ clim_PC2, random = ~1|Paper/Crop,
      na.action = "na.omit", data = d, method = "ML"),
  data = d
)

summary(m8_sem)
