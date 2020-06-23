source(here::here("code", "libraries.R"))

load(here::here("data", "d.prepared.RData"))

mapping = c("y" = 1, "n" = 0)

d.nonirr <- d.nonirr %>%
 mutate_at(names(.)[25:32], function(i) mapping[i])

### Results saved in "sem_output.txt"

### With fertilizer and climate variables ######################################

fert_sem <- psem(
  lme(delta.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR + Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)

  summary(fert_sem)
  print(summary(fert_sem))
  
fert_sem_climate <- psem(
  lme(delta.SOC.LRR ~ Fertilizer + Mean.pet,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR + Fertilizer + Mean.pet,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)
  
  summary(fert_sem_climate)

  # Inclusion of climate variables does not significantly improve model fit
  anova(fert_sem, fert_sem_climate)

### With organic amendments and climate variables ##############################


org.amend_sem <- psem(
  lme(delta.SOC.LRR ~ Org.amend, # Using both ppt and pet causes false convergence
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)

    print(summary(org.amend_sem))
  
org.amend_sem_climate <- psem(
  lme(delta.SOC.LRR ~ Org.amend, # Using both ppt and pet causes false convergence
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR + Mean.ppt,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)
    
    summary(org.amend_sem_climate)
    
    anova(org.amend_sem, org.amend_sem_climate)

### With fertilizer and org amend interaction ##################################
# Singularity issue

fert.org.soclrr.myp_sem <- psem(
  lme(delta.SOC.LRR ~ Fertilizer*Org.amend,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)

### With rotation and climate variables ########################################

    
rot_sem <- psem(
  lme(delta.SOC.LRR ~ Rotation,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)
    
    print(summary(rot_sem))

### With tillage and climate variables #########################################
    
till_sem <- psem(
  lme(delta.SOC.LRR ~ Tillage,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted + delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)
    
    print(summary(till_sem))

### Residue management #########################################################

res_sem <- psem(
  lme(delta.SOC.LRR ~ Residue.mgmt,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(MYP.weighted ~ delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML"),
  lme(CV.yield.weighted ~ MYP.weighted + delta.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d.nonirr, method = "ML")
)

    
# Not enough data for: Cover, Residue management, Weed management