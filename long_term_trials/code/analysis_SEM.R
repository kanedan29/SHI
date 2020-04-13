source(here::here("code", "libraries.R"))

load(here::here("data", "d.prepared.RData"))

mapping = c("y" = 1, "n" = 0)

d <- d %>%
 mutate_at(names(.)[24:30], function(i) mapping[i])

####################
##### With SOC #####
####################

#########
## MYP ##
#########

# With tillage and climate variables

till.climpc.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Tillage + Mean.clim_PC1 + Mean.clim_PC2 + Tillage:Mean.clim_PC1 + Tillage:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted + Mean.clim_PC2 + SOC.g.kg.weighted:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.climpc.soc.myp_sem)

    ## Tillage has significant effect on SOC, but not on MYP directly
    ## SOC does not have significant influence on MYP
    ## clim_PC2 (precipitation & aet) have significant influence on both SOC and MYP


till.climpc.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.clim_PC1 + Mean.clim_PC2 + Tillage:Mean.clim_PC1 + Tillage:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Tillage.SOC.LRR + Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.climpc.soclrr.myp_sem)
    ## Tillage has significant effect on SOC LRR, including interactions with both clim PCs
    ## SOC LRR has significant effect on MYP 
    ## Tillage does not have significant direct effect on MYP
    ## clim_PC2 has significant effect on MYP directly, but not clim_PC1

till.spei3.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Tillage + Mean.SPEI.3 + Tillage:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted + Mean.SPEI.3 + SOC.g.kg.weighted:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.spei3.soc.myp_sem)
    ## 3 month SPEI significant influence on SOC, nothing else new

till.spei3.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.SPEI.3 + Tillage:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Tillage.SOC.LRR + Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.spei3.soclrr.myp_sem)
    
    ## With SPEI, tillage SOC LRR has significant influence on MYP, no interaction with SPEI
    ## SPEI has significant influence on MYP now?
    
    anova(till.climpc.soc.myp_sem, till.climpc.soclrr.myp_sem)
    anova(till.spei3.soc.myp_sem, till.spei3.soclrr.myp_sem)
    anova(till.climpc.soclrr.myp_sem, till.spei3.soclrr.myp_sem)
    
    ## LRR models fit better overall than baseline SOC models
    ## Clim PCs and SPEI not significantly different, makes sense

till.spei6.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.SPEI.6 + Tillage:Mean.SPEI.6,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Tillage.SOC.LRR + Mean.SPEI.6,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.spei6.soclrr.myp_sem)
    
till.spei9.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.SPEI.9 + Tillage:Mean.SPEI.9,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Tillage.SOC.LRR + Mean.SPEI.9,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(till.spei9.soclrr.myp_sem)

till.spei12.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.SPEI.12 + Tillage:Mean.SPEI.12,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Tillage.SOC.LRR + Mean.SPEI.12,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.spei12.soclrr.myp_sem)
    anova(till.spei3.soclrr.myp_sem, till.spei6.soclrr.myp_sem,
          till.spei9.soclrr.myp_sem, till.spei12.soclrr.myp_sem,
          till.climpc.soclrr.myp_sem)

    ## All SPEI scale models are significantly different from one another,
    ## but not significantly different from clim PC models
    
till.trtcode.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Trt.code + Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Tillage.SOC.LRR + Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.trtcode.soclrr.myp_sem)
    ## Still not sure how to best incorporate treatment codes, interaction non-significant
    ## but independently both tillage and treatment code are significant. Can that be meaningfully interpreted?
    
### No cover crop data for SOC #################################################

### With fertilizer and climate variables ######################################

fert.climpc.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Fertilizer + Mean.clim_PC1 + Mean.clim_PC2 + Fertilizer:Mean.clim_PC1 + Fertilizer:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.climpc.soc.myp_sem)
    ## Nothing significant that is new here
    
fert.climpc.soclrr.myp_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Fertilizer.SOC.LRR + Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.climpc.soclrr.myp_sem)
    ## Climate variables insignificant in both paths with fertilizer
    

fert.spei3.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Fertilizer + Mean.SPEI.3 + Fertilizer:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.spei3.soc.myp_sem)
    ## Again, nothing new & significant

fert.spei3.soclrr.myp_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer + Mean.SPEI.3 + Fertilizer:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Fertilizer.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.spei3.soclrr.myp_sem)
    ## Again, climate not significant in either path of model, but Fertilizer is for both

fert.soclrr.myp_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Fertilizer.SOC.LRR + Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.soclrr.myp_sem)
    ## Final fertilizer model

### With organic amendments and climate variables ##############################

org.climpc.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Org.amend + Mean.clim_PC1 + Mean.clim_PC2 + Org.amend:Mean.clim_PC1 + Org.amend:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(org.climpc.soc.myp_sem)
    ## Org amend significantly predicts absolute SOC levels
    ## nothing else significant

org.climpc.soclrr.myp_sem <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend + Mean.clim_PC1 + Mean.clim_PC2 + Org.amend:Mean.clim_PC1 + Org.amend:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.climpc.soclrr.myp_sem)
    ## Org amend significantly influences SOC LRR and through interaction with clim PC1 (temperature)
    ## SOC LRR significantly influences MYP 
    ## Org amend does not significantly and directly influence MYP

org.spei3.soclrr.myp_sem <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend + Mean.SPEI.3 + Org.amend:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.spei3.soclrr.myp_sem)
    ## SPEI does not interact with Org amend to influence SOC LRR or MYP
    ## SPEI more precipitation driven, org amend may be more important for mediating temperature effects
    ## than precipitation effects

### With rotation and climate variables ########################################

rot.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Rotation,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(rot.soc.myp_sem)
    ## Rotation not significantly influencing SOC or MYP
    
# rot.soclrr.myp_sem <- psem(
#   lme(delta.Rotation.SOC.LRR ~ Rotation,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(MYP ~ delta.Rotation.SOC.LRR,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
# )

    ## Convergence error code: 1, may not be enough data 
    ## n Rotation == 1 = 2 only

    # summary(rot.soclrr.myp_sem)

### With residue management and climate variables ##############################

res.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Residue.mgmt,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(res.soc.myp_sem)
    ## Residue management significantly predicts absolute SOC values, but not MYP

res.soclrr.myp_sem <- psem(
  lme(delta.Residue.mgmt.SOC.LRR ~ Residue.mgmt,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ delta.Residue.mgmt.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(res.soclrr.myp_sem)
    ## Residue management significantly influences SOC LRR, but not MYP

res.spei3.soc.myp_sem <- psem(
  lme(SOC.g.kg.weighted ~ Residue.mgmt + Mean.SPEI.3 + Residue.mgmt:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(res.spei3.soc.myp_sem)

# res.spei3.soclrr.myp_sem <- psem(
#   lme(delta.Residue.mgmt.SOC.LRR ~ Residue.mgmt + Mean.SPEI.3 + Residue.mgmt:Mean.SPEI.3,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(MYP ~ delta.Residue.mgmt.SOC.LRR,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
# )

    ## Singularity error

################################################################################
## CV ##########################################################################
################################################################################

### With tillage and climate variables #########################################

till.climpc.soc.cv_sem <- psem(
  lme(SOC.g.kg.weighted ~ Tillage + Mean.clim_PC1 + Mean.clim_PC2 + Tillage:Mean.clim_PC1 + Tillage:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.climpc.soc.cv_sem)
    ## SOC does not significantly influence CV

till.climpc.soclrr.cv_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.clim_PC1 + Mean.clim_PC2 + Tillage:Mean.clim_PC1 + Tillage:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Tillage.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(till.climpc.soclrr.cv_sem)
    ## Tillage interacts with climate to influence SOC LRR (as above)
    ## Tillage SOC LRR does not significantly influence yield CV

### No cover crop data for SOC #################################################

### With fertilizer and climate variables ######################################


fert.climpc.soclrr.cv_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer + Mean.clim_PC1 + Mean.clim_PC2 + Fertilizer:Mean.clim_PC1 + Fertilizer:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Fertilizer.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(fert.climpc.soclrr.cv_sem)
    ## Fertilizer significantly influences SOC LRR
    ## Fertilizer SOC LRR significantly influences yield CV
    ## Fertilizer does not significantly influence yield CV

fert.spei3.soclrr.cv_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer + Mean.SPEI.3 + Fertilizer:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Fertilizer.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.spei3.soclrr.cv_sem)
    ## No significant climate interactions with fertilizer

### With organic amendments and climate data ###################################

org.climpc.soclrr.cv_sem <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend + Mean.clim_PC1 + Mean.clim_PC2 + Org.amend:Mean.clim_PC1 + Org.amend:Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.climpc.soclrr.cv_sem)
    ## Org SOC LRR significantly influences yield CV
    ## No significant interactions between org amend and climate PCs

org.spei3.soclrr.cv_sem <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend + Mean.SPEI.3 + Org.amend:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.spei3.soclrr.cv_sem)
    ## Confirmed, no siginificant interactions between climate vars and org amend

### With rotation and climate variables ########################################

# rot.climpc.soclrr.cv_sem <- psem(
#   lme(delta.Rotation.SOC.LRR ~ Rotation + Mean.clim_PC1 + Mean.clim_PC2 + Rotation:Mean.clim_PC1 + Rotation:Mean.clim_PC2,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(CV.yield ~ delta.Rotation.SOC.LRR,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
# )

    ## singularity again
    
rot.spei3.soc.cv_sem <- psem(
  lme(SOC.g.kg.weighted ~ Rotation + Mean.SPEI.3 + Rotation:Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ SOC.g.kg.weighted,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(rot.spei3.soc.cv_sem)
    ## nothing significant

# rot.spei3.soclrr.cv_sem <- psem(
#   lme(delta.Rotation.SOC.LRR ~ Rotation + Mean.SPEI.3 + Rotation:Mean.SPEI.3,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(CV.yield ~ delta.Rotation.SOC.LRR,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
# )

    ## Error: overfitted model

### With residue management and climate variables ##############################

res.soclrr.cv_sem <- psem(
  lme(delta.Residue.mgmt.SOC.LRR ~ Residue.mgmt,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Residue.mgmt.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(res.soclrr.cv_sem)
    ## Residue management significantly influences SOC LRR (as above)
    ## Residue management & SOC LRR does not siginificantly influence yield CV

################################################################################


#######################
##### Without SOC #####
#######################

d.yield.stability <- d.yield.stability %>%
  mutate_at(names(.)[18:24], function(i) mapping[i])
