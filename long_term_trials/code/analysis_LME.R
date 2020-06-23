source("code/libraries.R")

load("data/d.prepared.RData")

mapping = c("y" = 1, "n" = 0)

d.nonirr <- d.nonirr %>%
  mutate_at(names(.)[25:31], function(i) mapping[i])

d.irr <- d.irr %>%
  mutate_at(names(.)[25:31], function(i) mapping[i])

d.yield.stability %>%
  mutate_at(names(.)[25:31], function(i) mapping[i]) -> d.yield.stability

### By-crop analysis ###########################################################

  # Update with revised data and metrics if we want to re-run this analysis #

climate_effects_by_crop <- d.yield.stability %>%
  filter(!is.na(Tillage)) %>% # Change for different treatments
  group_by(Crop) %>%
  filter(length(unique(Paper)) > 1) %>%
  nest() %>%
  mutate(model = map(data, function(df) lme(MYP.weighted.percent ~ Tillage*Mean.SPEI.1, random = ~1|Paper,
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

m0 <- lme(MYP.weighted.percent ~ delta.SOC.LRR, random = ~1|Paper/Crop, data = d.nonirr, na.action = "na.omit")

summary(m0)
plot(m0)

# Sensitivity analysis (leave-one-out by Paper)

m0$coefficients$fixed[[2]] # effect size
sqrt(m0$varFix[2,2]) # standard error; effect size +/- 1.96 * SE for 95% CI
anova(m0)[2,4] # p-value

sensitivity_analysis <- function(df, y, x) {
  f <- paste(y, "~", x)
  results <- data.frame(matrix(ncol = 5, nrow = 0))
  names <- c("Papers", "Effect.size", "95CI.lower", "95CI.upper", "P")
  colnames(results) <- names
  
  # Model with all papers
  model <- lme(as.formula(f), random = ~1|Paper/Crop, data = df, na.action = "na.omit")
  effect <- round(model$coefficients$fixed[[2]], digits = 4)
  CI.lower <- round(effect - 1.96 * sqrt(model$varFix[2,2]), digits = 4)
  CI.upper <- round(effect + 1.96 * sqrt(model$varFix[2,2]), digits = 4)
  p.value <- round(anova(model)[2,4], digits = 4)
  results[1,] <- c("All", effect, CI.lower, CI.upper, p.value)
  
  # Leave one out
  for (i in 1:length(unique(df$Paper))) {
    df_minus <- df %>%
      filter(Paper != unique(df$Paper)[i])
    
    model <- lme(as.formula(f), random = ~1|Paper/Crop, data = df_minus, na.action = "na.omit")
    effect <- round(model$coefficients$fixed[[2]], digits = 4)
    CI.lower <- round(effect - 1.96 * sqrt(model$varFix[2,2]), digits = 4)
    CI.upper <- round(effect + 1.96 * sqrt(model$varFix[2,2]), digits = 4)
    p.value <- round(anova(model)[2,4], digits = 4)
    
    results[i+1,] <- c(paste("-", unique(df$Paper)[i]), effect, CI.lower, CI.upper, p.value)
  }
  
  return(results)
}

sensitivity_results_myp <- sensitivity_analysis(d.nonirr, "MYP.weighted.percent", "delta.SOC.LRR")

## Thierfelder and Wall 2012, Gao et al. 2015 seem to have significant effects on MYP effect size

myp_test_data <- d.nonirr %>%
  filter(Paper != "Thierfelder and Wall 2012",
         Paper != "Gao et al. 2015")

m01 <- lme(MYP.weighted.percent ~ delta.SOC.LRR, random = ~1|Paper/Crop, data = myp_test_data, na.action = "na.omit")
summary(m01)

# CV by SOC

m1 <- lme(CV.yield.weighted ~ delta.SOC.LRR, random = ~1|Paper/Crop, data = d.nonirr, na.action = "na.omit")

summary(m1)
plot(m1)

sensitivity_results_cv <- sensitivity_analysis(d.nonirr, "CV.yield.weighted", "delta.SOC.LRR")

## Thierfelder and Wall 2012 seems to have significant effect on CV effect size (likely connected to effects on MYP)

cv_test_data <- d.nonirr %>%
  filter(Paper != "Thierfelder and Wall 2012")

m02 <- lme(CV.yield.weighted ~ delta.SOC.LRR, random = ~1|Paper/Crop, data = cv_test_data, na.action = "na.omit")
summary(m02)
