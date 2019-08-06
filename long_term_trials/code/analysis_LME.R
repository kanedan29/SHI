source("code/libraries.R")
source("code/data_prep_for_analysis.R")

## Models with carbon

# MYP by SOC

d %>%
  filter(Crop != "Biomass",
         Crop != "Maize biomass",
         Crop != "Cotton lint") %>%

lme(MYP ~ SOC.g.kg.weighted*Crop, 
             random = ~1|Paper, data = ., na.action = "na.omit") -> m1

summary(m1)
anova(m1)

# MYP by direct effects with SOC added

m2 <- lme(MYP ~ SOC.g.kg.weighted+Org.amend+Org.amend:SOC.g.kg.weighted, 
            random = ~1|Paper/Crop, data = d, na.action = "na.omit")

m3 <- lme(MYP ~ SOC.g.kg.weighted+Tillage+Tillage:SOC.g.kg.weighted, 
                  random = ~1|Paper/Crop, data = d, na.action = "na.omit")

summary(m2)
summary(m3)

# MYP by LRR models
m4 <- lm(MYP ~ Org.amend.SOC.LRR, 
            data = d, na.action = "na.omit")

m5 <- lme(MYP ~ Org.amend.SOC.LRR, random = ~1|Paper/Crop,
            data = d, na.action = "na.omit")

summary(m4)
summary(m5)

ggplot(data = d, aes(x = Org.amend.SOC.LRR, y = MYP, color = Crop))+
  geom_point()


# CV by SOC


m6 <- lme(CV.yield ~ SOC.g.kg.weighted, 
          random = ~1|Paper/Crop, data = d, na.action = "na.omit")

summary(m6)


# CV by direct effects with SOC added

m7 <- lme(CV.yield ~ SOC.g.kg.weighted+Org.amend+Org.amend:SOC.g.kg.weighted, 
          random = ~1|Paper/Crop, data = d, na.action = "na.omit")

m8 <- lme(CV.yield ~ SOC.g.kg.weighted+Tillage+Tillage:SOC.g.kg.weighted, 
          random = ~1|Paper/Crop, data = d, na.action = "na.omit")

summary(m7)
summary(m8)

# CV by LRR models
m9 <- lme(CV.yield ~ Tillage.SOC.LRR, random = ~1|Paper/Crop,
          data = d, na.action = "na.omit")

m10 <- lme(CV.yield ~ Org.amend.SOC.LRR, random = ~1|Paper/Crop,
          data = d, na.action = "na.omit")

summary(m9)
summary(m10)




# Models without carbon 

d.yield.stability %>%
  group_by(Paper, Crop, Units) %>%
  mutate(Paper_crop_mean_yield = mean(Mean.yield),
         MYP_percent= MYP/Paper_crop_mean_yield) -> d.yield.stability


summary(lme(MYP_percent ~ Tillage, 
            random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))

summary(lme(MYP_percent ~ Org.amend,
            random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))

summary(lme(MYP_percent ~ Rotation, 
            random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))


