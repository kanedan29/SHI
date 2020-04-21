source("code/libraries.R")
load(here::here("data", "d.yield.RData"))

################################################################################
### No carbon, non-aggregated yield data
################################################################################

mapping = c("y" = 1, "n" = 0)

d.yield.nonagg <- d.yield.nonagg %>%
  mutate_at(names(.)[41:47], function(i) mapping[i])

d.yield.nonagg <- d.yield.nonagg[d.yield.nonagg$Yield.kg.per.hectare < 125000,]

# d.yield.nonagg$Yield.kg.ha.scaled <- scale(d.yield.nonagg$Yield.kg.per.hectare)

### Using crop as a random effect ##############################################

# Rotation significant
yield.rot.all <- lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper/Crop,
                     data = d.yield.nonagg, na.action = "na.omit")
summary(yield.rot.all) 

# Rotation & tillage significant when both included, otherwise tillage not significant
yield.rot.till.all <- lme(Yield.kg.per.hectare ~ Rotation*Tillage, random = ~1|Paper/Crop,
                          data = d.yield.nonagg, na.action = "na.omit")
summary(yield.rot.till.all)


# Org amend significant
yield.org.amend.all <- lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper/Crop,
                     data = d.yield.nonagg, na.action = "na.omit", method = "ML")
summary(yield.org.amend.all) 

# Fertilizer significant
yield.fert.all <- lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper/Crop,
                     data = d.yield.nonagg, na.action = "na.omit", method = "ML")
summary(yield.fert.all) 

# Interaction of fertilizer and org amend significant too
yield.fert.org.amend.all <- lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper/Crop,
                      data = d.yield.nonagg, na.action = "na.omit", method = "ML")
summary(yield.fert.org.amend.all)

# Weed management significant
yield.weed.mgmt.all <- lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper/Crop,
                           data = d.yield.nonagg, na.action = "na.omit")
summary(yield.weed.mgmt.all)

# Clim PC1 (tmin/tmax) only significant climate predictor variable of Yield
yield.climpc.1.all <- lme(Yield.kg.per.hectare ~ clim_PC1, random = ~1|Paper/Crop,
                        data = d.yield.nonagg, na.action = "na.omit")

summary(yield.climpc.1.all)

# Tillage, cover, residue managmenet not significant independently

yield.rot.climpc.1 <- lme(Yield.kg.per.hectare ~ Rotation + clim_PC1 + Rotation:clim_PC1, random = ~1|Paper/Crop,
                          data = d.yield.nonagg, na.action = "na.omit")

summary(yield.rot.climpc.1)
plot(yield.rot.climpc.1)

yield.rot.till.climpc.1 <- lme(Yield.kg.per.hectare ~ Rotation*Tillage*clim_PC1, random = ~1|Paper/Crop,
                          data = d.yield.nonagg, na.action = "na.omit")
summary(yield.rot.till.climpc.1)

yield.fert.climpc.all <- lme(Yield.kg.per.hectare ~ Fertilizer*clim_PC1, random = ~1|Paper/Crop,
                             data = d.yield.nonagg, na.action = "na.omit")
summary(yield.fert.climpc.all)

yield.cover.climpc.all <- lme(Yield.kg.per.hectare ~ Cover*clim_PC1, random = ~1|Paper/Crop,
                             data = d.yield.nonagg, na.action = "na.omit")
summary(yield.cover.climpc.all)

################################################################################
### Create yield models for each crop independently using nested dataframes ####
################################################################################

row.names(d.yield.nonagg) <- make.names(row.names(d.yield.nonagg))

by_crop <- d.yield.nonagg %>%
  group_by(Crop) %>%
  nest()

climate_effects_by_crop <- d.yield.nonagg %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(spei_3_model = map(data, function(df) lm(Yield.kg.per.hectare ~ SPEI.3, data = df)),
         spei_3_resids = map2(data, spei_3_model, modelr::add_residuals),
         spei_6_model = map(data, function(df) lm(Yield.kg.per.hectare ~ SPEI.6, data = df)),
         spei_6_resids = map2(data, spei_6_model, modelr::add_residuals),
         spei_9_model = map(data, function(df) lm(Yield.kg.per.hectare ~ SPEI.9, data = df)),
         spei_9_resids = map2(data, spei_9_model, modelr::add_residuals),
         spei_12_model = map(data, function(df) lm(Yield.kg.per.hectare ~ SPEI.12, data = df)),
         spei_12_resids = map2(data, spei_12_model, modelr::add_residuals),
         clim_PC1_model = map(data, function(df) lm(Yield.kg.per.hectare ~ clim_PC1, data = df)),
         clim_PC1_resids = map2(data, clim_PC1_model, modelr::add_residuals),
         clim_PC2_model = map(data, function(df) lm(Yield.kg.per.hectare ~ clim_PC2, data = df)),
         clim_PC2_resids = map2(data, clim_PC1_model, modelr::add_residuals))

spei_3_yield_glance <- climate_effects_by_crop %>%
  select(Crop, data, spei_3_model, spei_3_resids) %>%
  mutate(glance = map(spei_3_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

spei_6_yield_glance <- climate_effects_by_crop %>%
  select(Crop, data, spei_6_model, spei_6_resids) %>%
  mutate(glance = map(spei_6_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

spei_9_yield_glance <- climate_effects_by_crop %>%
  select(Crop, data, spei_9_model, spei_9_resids) %>%
  mutate(glance = map(spei_9_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

spei_12_yield_glance <- climate_effects_by_crop %>%
  select(Crop, data, spei_12_model, spei_12_resids) %>%
  mutate(glance = map(spei_12_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

clim_PC1_yield_glance <- climate_effects_by_crop %>%
  select(Crop, data, clim_PC1_model, clim_PC1_resids) %>%
  mutate(glance = map(clim_PC1_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

clim_PC2_yield_glance <- climate_effects_by_crop %>%
  select(Crop, data, clim_PC2_model, clim_PC2_resids) %>%
  mutate(glance = map(clim_PC2_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

tillage_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(tillage_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Tillage, data = df))) %>%
  mutate(resids = map2(data, tillage_model, modelr::add_residuals))

tillage_yield_glance <- tillage_effects_by_crop %>%
  mutate(glance = map(tillage_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

cover_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Cover)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(cover_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Cover, data = df))) %>%
  mutate(resids = map2(data, cover_model, modelr::add_residuals))

cover_yield_glance <- cover_effects_by_crop %>%
  mutate(glance = map(cover_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

fert_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Fertilizer)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(fert_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Fertilizer, data = df))) %>%
  mutate(resids = map2(data, fert_model, modelr::add_residuals))

fert_yield_glance <- fert_effects_by_crop %>%
  mutate(glance = map(fert_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

residue_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Residue.mgmt)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(residue_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Residue.mgmt, data = df))) %>%
  mutate(resids = map2(data, residue_model, modelr::add_residuals))

residue_yield_glance <- residue_effects_by_crop %>%
  mutate(glance = map(residue_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

org_amend_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Org.amend)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(org_amend_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Org.amend, data = df))) %>%
  mutate(resids = map2(data, org_amend_model, modelr::add_residuals))

org_amend_yield_glance <- org_amend_effects_by_crop %>%
  mutate(glance = map(org_amend_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

rotation_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Rotation)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(rotation_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Rotation, data = df))) %>%
  mutate(resids = map2(data, rotation_model, modelr::add_residuals))

rotation_yield_glance <- rotation_effects_by_crop %>%
  mutate(glance = map(rotation_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)

weed_effects_by_crop <- d.yield.nonagg %>%
  filter(!is.na(Weed.mgmt)) %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(weed_mgmt_model = map(data, function(df) lm(Yield.kg.per.hectare ~ Weed.mgmt, data = df))) %>%
  mutate(resids = map2(data, weed_mgmt_model, modelr::add_residuals))

weed_yield_glance <- weed_effects_by_crop %>%
  mutate(glance = map(weed_mgmt_model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  arrange(-r.squared, p.value)


# Copy to clipboard for pasting into Google Sheet
clim_PC2_yield_glance %>%
  select(-c(data, clim_PC2_model, clim_PC2_resids)) %>%
  clipr::write_clip()
