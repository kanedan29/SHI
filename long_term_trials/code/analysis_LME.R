source("code/libraries.R")
source("code/data_prep_for_analysis.R")


mapping = c("y" = 1, "n" = 0)

d <- d %>%
  mutate_at(names(.)[25:31], function(i) mapping[i])

d.yield.stability %>%
  group_by(Paper, Crop, Units) %>%
  mutate(Paper_crop_mean_yield = mean(Mean.yield),
         MYP_percent= MYP/Paper_crop_mean_yield) %>%
  mutate_at(names(.)[25:31], function(i) mapping[i]) -> d.yield.stability

################################################################################
### Models without carbon ######################################################
################################################################################

### Using Crop as random effect ################################################

spei <- lme(MYP_percent ~ Fertilizer*Mean.SPEI.1, 
            random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit")

spei.tidy <- broom.mixed::tidy(spei)

## all results pasted into Google Sheet for easier reading ##

clipr::write_clip(spei.tidy)


ggplot(d.yield.stability, mapping = aes(x = Mean.SPEI.1, y = CV.yield)) +
  geom_point()

#   # Fertilizer significant for both MYP and MYP percent
# 
# summary(lme(MYP_percent ~ Org.amend,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # Org amend significant for both MYP and MYP percent
# 
# summary(lme(MYP_percent ~ Rotation, 
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # Rotation significant for both MYP and MYP percent
# 
# summary(lme(CV.yield ~ Fertilizer, 
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # Fertilizer significant for yield CV (reduces CV)
# 
# summary(lme(CV.yield ~ Mean.SPEI.1,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # Rotation significant for yield CV
# 
# summary(lme(MYP_percent ~ Mean.SPEI.12,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # 9- and 12-month SPEI significant predictors of MYP percent, but not MYP
# 
# summary(lme(MYP ~ Mean.clim_PC1,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # Clim PC1 (temperature) significant predictor of MYP but not MYP percent
# 
# summary(lme(CV.yield ~ Mean.clim_PC1*Mean.clim_PC2,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
# summary(lme(CV.yield ~ Mean.clim_PC2,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
# 
#   # Clim PCs 1 & 2 significant predictors of yield CV
# 
# summary(lme(CV.yield ~ Mean.SPEI.1,
#             random = ~1|Paper/Crop, data = d.yield.stability, na.action = "na.omit"))
#   # 6-, 9-, and 12-month SPEI are significant predictors of yield CV
# 
# ### Finding best fit
# myp.fert.1 <- lme(MYP_percent ~ Fertilizer + Mean.clim_PC1, random = ~1|Paper/Crop,
#                   data = d.yield.stability, na.action = "na.omit")
# 

### By-crop analysis ###########################################################

climate_effects_by_crop <- d.yield.stability %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(MYP_percent ~ Tillage*Mean.SPEI.1, random = ~1|Paper,
                                            data = df, na.action = "na.omit")))
  mutate(resids = map2(data, model, modelr::add_residuals))

climate_tidy <- climate_effects_by_crop %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

## all results pasted into Google Sheet for easier reading ##

climate_tidy %>%
  select(-c(data, model, resids)) %>%
  clipr::write_clip()

################################################################################
### Models with carbon #########################################################
################################################################################

# MYP by SOC

d %>%
  filter(Crop != "Biomass",
         Crop != "Maize biomass",
         Crop != "Cotton lint") %>%

lme(MYP_percent ~ SOC.g.kg.weighted, 
             random = ~1|Paper/Crop, data = ., na.action = "na.omit") -> m1

m0 <- lme(CV.yield ~ SOC.g.kg.weighted, random = ~1|Paper/Crop, data = d, na.action = "na.omit")

m0.tidy <- broom.mixed::tidy(m0)

clipr::write_clip(m0.tidy)

summary(m0)

summary(m1)
anova(m1)

  # SOC not significant predictor of MYP by itself

# MYP by direct effects with SOC added

m2 <- lme(MYP ~ SOC.g.kg.weighted+Org.amend+Org.amend:SOC.g.kg.weighted, 
            random = ~1|Paper/Crop, data = d, na.action = "na.omit")

  # Why does SOC become significant here?
  qqplot(m2)

m3 <- lme(MYP ~ SOC.g.kg.weighted+Tillage+Tillage:SOC.g.kg.weighted, 
                  random = ~1|Paper/Crop, data = d, na.action = "na.omit")
  # And here

summary(lme(MYP ~ SOC.g.kg.weighted + Fertilizer + SOC.g.kg.weighted:Fertilizer, 
         random = ~1|Paper/Crop, data = d, na.action = "na.omit"))

summary(m2)
summary(m3)

# MYP by LRR models
m4 <- lm(MYP ~ Org.amend.SOC.LRR, 
            data = d, na.action = "na.omit")

m5 <- lme(MYP ~ delta.Org.amend.SOC.LRR, random = ~1|Paper/Crop,
            data = d, na.action = "na.omit")

summary(m4)
summary(m5)

ggplot(data = d, aes(x = delta.Org.amend.SOC.LRR, y = MYP, color = Crop))+
  geom_point()


# CV by SOC


m6 <- lme(CV.yield ~ SOC.g.kg.weighted, 
          random = ~1|Paper/Crop, data = d, na.action = "na.omit")

summary(m6)

  # SOC not significant standalone predictor of CV


# CV by direct effects with SOC added

m7 <- lme(CV.yield ~ SOC.g.kg.weighted+Org.amend+Org.amend:SOC.g.kg.weighted, 
          random = ~1|Paper/Crop, data = d, na.action = "na.omit")
    # Again, SOC is significant here when paired with org.amend
    # Why is unclear, I guess this makes the case for why we should use SEMs

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



m01 <- lme(SOC.g.kg.weighted ~ Tillage, random = ~1|Paper/Crop, data = d, na.action = "na.omit")
summary(m01)

m02 <- lme(SOC.g.kg.weighted ~ Fertilizer, random = ~1|Paper/Crop, data = d, na.action = "na.omit")
summary(m02)

m03 <- lme(SOC.g.kg.weighted ~ Residue.mgmt, random = ~1|Paper/Crop, data = d, na.action = "na.omit")
summary(m03)

m04 <- lme(delta.Org.amend.SOC.LRR ~ Org.amend, random = ~1|Paper/Crop, data = d, na.action = "na.omit",
           control = lmeControl(returnObject = TRUE))
summary(m04)

m05 <- lme(SOC.g.kg.weighted ~ Mean.SPEI.12, random = ~1|Paper/Crop, data = d, na.action = "na.omit")
summary(m05)
