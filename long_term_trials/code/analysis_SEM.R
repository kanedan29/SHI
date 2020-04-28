source(here::here("code", "libraries.R"))

load(here::here("data", "d.prepared.RData"))

mapping = c("y" = 1, "n" = 0)

d <- d %>%
 mutate_at(names(.)[25:31], function(i) mapping[i])

####################
##### With SOC #####
####################

#########
## MYP ##
#########

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
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer*Mean.clim_PC1,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Fertilizer.SOC.LRR + Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.climpc.soclrr.myp_sem)
    ## Climate variables insignificant in both paths with fertilizer
    
fert.soclrr.myp_sem1 <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Fertilizer.SOC.LRR + Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

fert.soclrr.myp_sem2 <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Fertilizer.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

anova(fert.soclrr.myp_sem1, fert.climpc.soclrr.myp_sem)
summary(fert.soclrr.myp_sem2)
  # Two models are not significantly different, so we'll favor the parsimony without
  # climate variables, including partial mediation structure
  
fert.spei.9.soclrr.myp_sem1 <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Fertilizer.SOC.LRR*Mean.SPEI.9 + Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

summary(fert.spei.9.soclrr.myp_sem1)

  ## Final models for fertilizer ##
  summary(fert.soclrr.myp_sem1)
  #####################################

### With organic amendments and climate variables ##############################


org.soclrr.myp_sem <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.soclrr.myp_sem)
  
org.clim.soclrr.myp_sem1 <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Org.amend.SOC.LRR + Mean.SPEI.9,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.clim.soclrr.myp_sem1)

org.clim.soclrr.myp_sem2 <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Org.amend.SOC.LRR*Mean.SPEI.9,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)   

    
    anova(org.clim.soclrr.myp_sem1, org.clim.soclrr.myp_sem2)
    # Models are significantly different
    # AIC and BIC are lower for the non-interaction models
    # suggesting that is the better fitting model
    # Also, both predictor variables become significant for predicting MYP_percent
    
    anova(org.soclrr.myp_sem, org.clim.soclrr.myp_sem2)
    # Now, models don't fit significantly differently, leave climate out in favor
    # of more parismonious model, i.e. don't reject the null
    
    org.soclrr.myp.null_sem <- psem(
      lme(delta.Org.amend.SOC.LRR ~ Org.amend,
          random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
      lme(MYP_percent ~ 1,
          random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
    )
    
    anova(org.soclrr.myp.null_sem, org.soclrr.myp_sem)
    
    # Final organic amendment model:
    summary(org.soclrr.myp_sem)
    ###############################

### With fertilizer and org amend interaction ##################################

# fert.org.soclrr.myp_sem <- psem(
#   lme(delta.Fertilizer.SOC.LRR ~ Fertilizer*Org.amend,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(delta.Org.amend.SOC.LRR ~ Fertilizer*Org.amend,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(MYP_percent ~ delta.Fertilizer.SOC.LRR + delta.Org.amend.SOC.LRR,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
# )
#     
### With rotation and climate variables ########################################
# Not enough data, too noisy/messy
    
# rot.soclrr.myp_sem <- psem(
#   lme(delta.Rotation.SOC.LRR ~ Rotation,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
#   lme(MYP ~ delta.Rotation.SOC.LRR,
#       random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
# )

    ## Convergence error code: 1, may not be enough data 
    ## n Rotation == 1 = 2 only

    # summary(rot.soclrr.myp_sem)
    
### With tillage and climate variables #########################################
    
till.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Tillage.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(till.soclrr.myp_sem)
    
till.clim.soclrr.myp_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(MYP_percent ~ delta.Tillage.SOC.LRR + Mean.SPEI.3,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(till.clim.soclrr.myp_sem)
    
    anova(till.soclrr.myp_sem, till.clim.soclrr.myp_sem)
    # Again, climate variables don't improve model fit
    
    ## Final tillage model: ###
    summary(till.soclrr.myp_sem)
    ###########################
    
    
### No cover crop data for SOC #################################################

################################################################################
## CV ##########################################################################
################################################################################

### No cover crop data for SOC #################################################

### With fertilizer and climate variables ######################################

fert.soclrr.cv_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Fertilizer.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)
    
    summary(fert.soclrr.cv_sem)

fert.clim.soclrr.cv_sem <- psem(
  lme(delta.Fertilizer.SOC.LRR ~ Fertilizer*Mean.SPEI.9,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Fertilizer.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(fert.clim.soclrr.cv_sem)
    
    anova(fert.soclrr.cv_sem, fert.clim.soclrr.cv_sem)
    # Models are not significantly different in terms of fit, select one without
    # climate data for parsimony
    
    ## Final model: ###########
    summary(fert.soclrr.cv_sem)
    ###########################

### With organic amendments and climate data ###################################

org.soclrr.cv_sem <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.soclrr.cv_sem)
    ## Org SOC LRR significantly influences yield CV
    ## No significant interactions between org amend and climate PCs

org.clim.soclrr.cv_sem1 <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend*Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Org.amend.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.clim.soclrr.cv_sem1)

org.clim.soclrr.cv_sem2 <- psem(
  lme(delta.Org.amend.SOC.LRR ~ Org.amend,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Org.amend.SOC.LRR + Mean.SPEI.6,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    summary(org.clim.soclrr.cv_sem2)
    
    anova(org.soclrr.cv_sem, org.clim.soclrr.cv_sem1)
    anova(org.soclrr.cv_sem, org.clim.soclrr.cv_sem2)
    
    ## Final model: ##########
    summary(org.soclrr.cv_sem)
    ##########################

### With rotation and climate variables ########################################

rot.soclrr.cv_sem <- psem(
  lme(delta.Rotation.SOC.LRR ~ Rotation,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Rotation.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

    ## singularity again

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
    
### With tillage and climate variables #########################################

till.soclrr.cv_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Tillage.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

summary(till.soclrr.cv_sem)

till.clim.soclrr.cv_sem <- psem(
  lme(delta.Tillage.SOC.LRR ~ Tillage + Mean.clim_PC1 + Mean.clim_PC2,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML"),
  lme(CV.yield ~ delta.Tillage.SOC.LRR,
      random = ~1|Paper/Crop, na.action = "na.omit", data = d, method = "ML")
)

summary(till.clim.soclrr.cv_sem)

  ## Tillage SOC not significant for yield CV

################################################################################
