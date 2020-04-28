source("code/libraries.R")
load(here::here("data", "d.yield.RData"))

################################################################################
### No carbon, non-aggregated yield data
################################################################################

mapping = c("y" = 1, "n" = 0)

d.yield.nonagg <- d.yield.nonagg %>%
  mutate_at(names(.)[44:50], function(i) mapping[i])

d.yield.nonagg <- d.yield.nonagg[d.yield.nonagg$Yield.kg.per.hectare < 125000,]


  # Data frame representing observations in years with significant acute drought events
  d.yield.filtered1 <- d.yield.nonagg %>%
    filter(SPEI.1.year.min < (SPEI.1 - 2*SPEI.1.SD))

  # Data frame representing observations in all years with drought conditions on average
  d.yield.filtered2 <- d.yield.nonagg %>%
    filter(SPEI.1 < 0)

## 

ggplot(data = d.yield.filtered2, mapping = aes(x = SPEI.1, y = Yield.kg.per.hectare)) +
  geom_point(mapping = aes(color = Crop)) +
  guides(color = FALSE)
  
### Using crop as a random effect ##############################################

### Rotation ###################################################################

yield.rot.all <- lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper/Crop,
                      data = d.yield.nonagg, na.action = "na.omit")
yield.rot.all.tidy <- broom.mixed::tidy(yield.rot.all)
  
yield.rot.acute <- lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper/Crop,
                     data = d.yield.filtered1, na.action = "na.omit")
yield.rot.acute.tidy <- broom.mixed::tidy(yield.rot.acute)

yield.rot.general <- lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper/Crop,
                       data = d.yield.filtered2, na.action = "na.omit")
yield.rot.general.tidy <- broom.mixed::tidy(yield.rot.general) 


### Tillage ####################################################################

yield.till.all <- lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper/Crop,
                        data = d.yield.nonagg, na.action = "na.omit")
yield.till.all.tidy <- broom.mixed::tidy(yield.till.all)

yield.till.acute <- lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper/Crop,
                          data = d.yield.filtered1, na.action = "na.omit")
yield.till.acute.tidy <- broom.mixed::tidy(yield.till.acute)

yield.till.general <- lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper/Crop,
                        data = d.yield.filtered2, na.action = "na.omit")
yield.till.general.tidy <- broom.mixed::tidy(yield.till.general)

### Organic amendments #########################################################

yield.org.amend.all <- lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper/Crop,
                      data = d.yield.nonagg, na.action = "na.omit")
yield.org.amend.all.tidy <- broom.mixed::tidy(yield.org.amend.all)

yield.org.amend.acute <- lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper/Crop,
                     data = d.yield.filtered1, na.action = "na.omit")
yield.org.amend.acute.tidy <- broom.mixed::tidy(yield.org.amend.acute)

yield.org.amend.general <- lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper/Crop,
                             data = d.yield.filtered2, na.action = "na.omit")
yield.org.amend.general.tidy <- broom.mixed::tidy(yield.org.amend.general)

### Fertilizer #################################################################

yield.fert.all <- lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper/Crop,
                     data = d.yield.nonagg, na.action = "na.omit")
yield.fert.all.tidy <- broom.mixed::tidy(yield.fert.all)

yield.fert.acute <- lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper/Crop,
                      data = d.yield.filtered1, na.action = "na.omit")
yield.fert.acute.tidy <- broom.mixed::tidy(yield.fert.acute)

yield.fert.general <- lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper/Crop,
                      data = d.yield.filtered2, na.action = "na.omit")
yield.fert.general.tidy <- broom.mixed::tidy(yield.fert.general)

### Interaction of fertilizer and org amend significant ########################
yield.fert.org.amend.all <- lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper/Crop,
                      data = d.yield.nonagg, na.action = "na.omit")
yield.fert.org.amend.all.tidy <- broom.mixed::tidy(yield.fert.org.amend.all)

yield.fert.org.amend.acute <- lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper/Crop,
                                data = d.yield.filtered1, na.action = "na.omit")
yield.fert.org.amend.acute.tidy <- broom.mixed::tidy(yield.fert.org.amend.acute)

yield.fert.org.amend.general <- lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper/Crop,
                                data = d.yield.filtered2, na.action = "na.omit")
yield.fert.org.amend.general.tidy <- broom.mixed::tidy(yield.fert.org.amend.general)

### Weed management ############################################################

yield.weed.mgmt.all <- lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper/Crop,
                           data = d.yield.nonagg, na.action = "na.omit")
yield.weed.mgmt.all.tidy <- broom.mixed::tidy(yield.weed.mgmt.all)

yield.weed.mgmt.acute <- lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper/Crop,
                             data = d.yield.filtered1, na.action = "na.omit")
yield.weed.mgmt.acute.tidy <- broom.mixed::tidy(yield.weed.mgmt.acute)

yield.weed.mgmt.general <- lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper/Crop,
                             data = d.yield.filtered2, na.action = "na.omit")
yield.weed.mgmt.general.tidy <- broom.mixed::tidy(yield.weed.mgmt.general)

### Residue management #########################################################

yield.res.mgmt.all <- lme(Yield.kg.per.hectare ~ Residue.mgmt, random = ~1|Paper/Crop,
                           data = d.yield.nonagg, na.action = "na.omit")
yield.res.mgmt.all.tidy <- broom.mixed::tidy(yield.res.mgmt.all)

yield.res.mgmt.acute <- lme(Yield.kg.per.hectare ~ Residue.mgmt, random = ~1|Paper/Crop,
                             data = d.yield.filtered1, na.action = "na.omit")
yield.res.mgmt.acute.tidy <- broom.mixed::tidy(yield.res.mgmt.acute)

yield.res.mgmt.general <- lme(Yield.kg.per.hectare ~ Residue.mgmt, random = ~1|Paper/Crop,
                               data = d.yield.filtered2, na.action = "na.omit")
yield.res.mgmt.general.tidy <- broom.mixed::tidy(yield.res.mgmt.general)

### Cover ######################################################################

yield.cover.all <- lme(Yield.kg.per.hectare ~ Cover, random = ~1|Paper/Crop,
                       data = d.yield.nonagg, na.action = "na.omit")
yield.cover.all.tidy <- broom.mixed::tidy(yield.cover.all)

yield.cover.acute <- lme(Yield.kg.per.hectare ~ Cover, random = ~1|Paper/Crop,
                         data = d.yield.filtered1, na.action = "na.omit")
yield.cover.acute.tidy <- broom.mixed::tidy(yield.cover.acute)

yield.cover.general <- lme(Yield.kg.per.hectare ~ Cover, random = ~1|Paper/Crop,
                         data = d.yield.filtered2, na.action = "na.omit")
yield.cover.general.tidy <- broom.mixed::tidy(yield.cover.general)

### Copy to clipboard for transfer to Google Sheet #############################

clipr::write_clip(yield.cover.all.tidy)

# # Clim PC1 (tmin/tmax) only significant climate predictor variable of Yield
# yield.climpc.1.all <- lme(Yield.kg.per.hectare ~ clim_PC1, random = ~1|Paper/Crop,
#                         data = d.yield.nonagg, na.action = "na.omit")
# 
# summary(yield.climpc.1.all)
# 
# # Tillage, cover, residue managmenet not significant independently
# 
# yield.rot.climpc.1 <- lme(Yield.kg.per.hectare ~ Rotation + clim_PC1 + Rotation:clim_PC1, random = ~1|Paper/Crop,
#                           data = d.yield.nonagg, na.action = "na.omit")
# 
# summary(yield.rot.climpc.1)
# plot(yield.rot.climpc.1)
# 
# yield.rot.till.climpc.1 <- lme(Yield.kg.per.hectare ~ Rotation*Tillage*clim_PC1, random = ~1|Paper/Crop,
#                           data = d.yield.nonagg, na.action = "na.omit")
# summary(yield.rot.till.climpc.1)
# 
# yield.fert.climpc.all <- lme(Yield.kg.per.hectare ~ Fertilizer*clim_PC1, random = ~1|Paper/Crop,
#                              data = d.yield.nonagg, na.action = "na.omit")
# summary(yield.fert.climpc.all)
# 
# yield.cover.climpc.all <- lme(Yield.kg.per.hectare ~ Cover*clim_PC1, random = ~1|Paper/Crop,
#                              data = d.yield.nonagg, na.action = "na.omit")
# summary(yield.cover.climpc.all)

################################################################################
### Create yield models for each crop independently using nested dataframes ####
################################################################################

### Tillage ####################################################################

tillage_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper,
                                                    data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

tillage_yield_tidy_acute <- tillage_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy)

tillage_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper,
                                                    data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

tillage_yield_tidy_general <- tillage_effects_by_crop_general %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy)

tillage_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Tillage)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Tillage, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

tillage_yield_tidy_all <- tillage_effects_by_crop_all %>%
  mutate(tidy = map(model, broom.mixed::tidy)) %>%
  unnest(tidy)

### Cover ######################################################################

cover_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Cover)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Cover, random = ~1|Paper,
                                                    data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

cover_yield_tidy_acute <- cover_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

cover_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Cover)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Cover, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

cover_yield_tidy_general <- cover_effects_by_crop_general %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

cover_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Cover)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Cover, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

cover_yield_tidy_all <- cover_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Fertilizer #################################################################

fert_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Fertilizer)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

fert_yield_tidy_acute <- fert_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

fert_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Fertilizer)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

fert_yield_tidy_general <- fert_effects_by_crop_general %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

fert_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Fertilizer)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Fertilizer, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

fert_yield_tidy_all <- fert_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Residue management #########################################################

residue_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Residue.mgmt)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Residue.mgmt, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

residue_yield_tidy_acute <- residue_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

residue_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Residue.mgmt)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Residue.mgmt, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

residue_yield_tidy_general <- residue_effects_by_crop_general %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

residue_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Residue.mgmt)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Residue.mgmt, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

residue_yield_tidy_all <- residue_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Organic amendments #########################################################

org_amend_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Org.amend)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

org_amend_yield_tidy_acute <- org_amend_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

org_amend_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Org.amend)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

org_amend_yield_tidy_general <- org_amend_effects_by_crop_general %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

org_amend_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Org.amend)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Org.amend, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

org_amend_yield_tidy_all <- org_amend_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Rotation ###################################################################

rotation_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Rotation)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

rotation_yield_tidy_acute <- rotation_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

rotation_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Rotation)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

rotation_yield_tidy_general <- rotation_effects_by_crop_general %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

rotation_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Rotation)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Rotation, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

rotation_yield_tidy_all <- rotation_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Weed management ############################################################

weed_effects_by_crop_acute <- d.yield.filtered1 %>%
  filter(!is.na(Weed.mgmt)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

weed_yield_tidy_acute <- weed_effects_by_crop_acute %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

weed_effects_by_crop_general <- d.yield.filtered2 %>%
  filter(!is.na(Weed.mgmt)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

weed_yield_tidy_general <- weed_effects_by_crop_general %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

weed_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Weed.mgmt)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Weed.mgmt, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

weed_yield_tidy_all <- weed_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Fertilizer * Organic amendments interaction ################################

# fert_org_effects_by_crop_acute <- d.yield.filtered1 %>%
#   filter(!is.na(Fertilizer) & !is.na(Org.amend)) %>%
#   group_by(Crop) %>%
#   filter(length(unique(Paper)) > 1) %>%
#   nest() %>%
#   mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper,
#                                             data = df, na.action = "na.omit"))) %>%
#   mutate(resids = map2(data, model, modelr::add_residuals))
# 
# fert_org_yield_tidy_acute <- fert_effects_by_crop_acute %>%
#   mutate(tidy = map(model, broom::tidy)) %>%
#   unnest(tidy)
# 
# fert_org_effects_by_crop_general <- d.yield.filtered2 %>%
#   filter(!is.na(Fertilizer) & !is.na(Org.amend)) %>%
#   group_by(Crop) %>%
#   filter(length(unique(Paper)) > 1) %>%
#   nest() %>%
#   mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper,
#                                             data = df, na.action = "na.omit"))) %>%
#   mutate(resids = map2(data, model, modelr::add_residuals))
# 
# fert_yield_tidy_general <- fert_effects_by_crop_general %>%
#   mutate(tidy = map(model, broom::tidy)) %>%
#   unnest(tidy)

fert_org_effects_by_crop_all <- d.yield.nonagg %>%
  filter(!is.na(Fertilizer) & !is.na(Org.amend)) %>%
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(Yield.kg.per.hectare ~ Fertilizer*Org.amend, random = ~1|Paper,
                                            data = df, na.action = "na.omit"))) %>%
  mutate(resids = map2(data, model, modelr::add_residuals))

fert_org_yield_tidy_all <- fert_org_effects_by_crop_all %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

### Copy to clipboard for pasting into Google Sheet ############################
fert_org_yield_tidy_all %>%
  select(-c(data, model, resids)) %>%
  clipr::write_clip()
