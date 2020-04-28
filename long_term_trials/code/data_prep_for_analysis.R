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
  inner_join(d.carbon.summary[,c(1,3:6)]) %>%
  distinct(.) -> d.yield.stability.carbon

## Calculate LRRs

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Tillage == "n") %>%
  summarise(Tillage.SOC.control = mean(SOC.g.kg.weighted),
            SD.tillage.control = sd(SOC.g.kg.weighted),
            N.tillage.control = n()) -> d.till.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Cover == "n") %>%
  summarise(Cover.SOC.control = mean(SOC.g.kg.weighted),
            SD.cover.control = sd(SOC.g.kg.weighted),
            N.cover.control = n()) -> d.cover.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Org.amend == "n") %>%
  summarise(Org.amend.SOC.control = mean(SOC.g.kg.weighted),
            SD.org.amend.control = sd(SOC.g.kg.weighted),
            N.org.amend.control = n()) -> d.org.amend.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Rotation == "n") %>%
  summarise(Rotation.SOC.control = mean(SOC.g.kg.weighted),
            SD.rotation.control = sd(SOC.g.kg.weighted),
            N.rotation.control = n()) -> d.rotation.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Residue.mgmt == "n") %>%
  summarise(Residue.mgmt.SOC.control = mean(SOC.g.kg.weighted),
            SD.res.control = sd(SOC.g.kg.weighted),
            N.res.control = n()) -> d.res.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Fertilizer == "n") %>%
  summarise(Fertilizer.SOC.control = mean(SOC.g.kg.weighted),
            SD.fertilizer.control = sd(SOC.g.kg.weighted),
            N.fertilizer.control = n()) -> d.fertilizer.soc.control

d.yield.stability.carbon %>%
  left_join(d.till.soc.control) %>%
  left_join(d.cover.soc.control) %>%
  left_join(d.org.amend.soc.control) %>%
  left_join(d.rotation.soc.control) %>%
  left_join(d.res.soc.control) %>%
  left_join(d.fertilizer.soc.control) %>%
  distinct(.) %>%
  group_by(Paper, Trt.combo) %>%
  mutate(SD.treatment = sd(SOC.g.kg.weighted),
         N.treatment = n()) -> d

d %>%
  mutate(
    Tillage.SOC.LRR = log(SOC.g.kg.weighted/Tillage.SOC.control),
    var.Tillage.SOC.LRR = (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) + (SD.tillage.control^2)/(N.tillage.control * (Tillage.SOC.control^2)),
    Cover.SOC.LRR = log(SOC.g.kg.weighted/Cover.SOC.control),
    var.Cover.SOC.LRR = (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) + (SD.cover.control^2)/(N.cover.control * (Cover.SOC.control^2)),
    Org.amend.SOC.LRR = log(SOC.g.kg.weighted/Org.amend.SOC.control),
    var.Org.amend.SOC.LRR = (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) + (SD.org.amend.control^2)/(N.org.amend.control * (Org.amend.SOC.control^2)),
    Rotation.SOC.LRR = log(SOC.g.kg.weighted/Rotation.SOC.control),
    var.Rotation.SOC.LRR = (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) + (SD.rotation.control^2)/(N.rotation.control * (Rotation.SOC.control^2)),
    Residue.mgmt.SOC.LRR = log(SOC.g.kg.weighted/Residue.mgmt.SOC.control),
    var.Residue.mgmt.SOC.LRR = (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) + (SD.res.control^2)/(N.res.control * (Residue.mgmt.SOC.control^2)),
    Fertilizer.SOC.LRR = log(SOC.g.kg.weighted/Fertilizer.SOC.control),
    var.Fertilizer.SOC.LRR = (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) + (SD.fertilizer.control^2)/(N.fertilizer.control * (Fertilizer.SOC.control^2))
  ) -> d

## Correct LRRs for bias

d %>%
  mutate(
    delta.Tillage.SOC.LRR = Tillage.SOC.LRR + 0.5 * ( (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) - (SD.tillage.control^2)/(N.tillage.control * (Tillage.SOC.control^2)) ),
    delta.var.Tillage.SOC.LRR = var.Tillage.SOC.LRR + 0.5 * ( (SD.treatment^4)/(N.treatment^2 * (SOC.g.kg.weighted^4)) + (SD.tillage.control^4)/(N.tillage.control^2 * (Tillage.SOC.control^4)) ),
    delta.Cover.SOC.LRR = Cover.SOC.LRR + 0.5 * ( (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) - (SD.cover.control^2)/(N.cover.control * (Cover.SOC.control^2)) ),
    delta.var.Cover.SOC.LRR = var.Cover.SOC.LRR + 0.5 * ( (SD.treatment^4)/(N.treatment^2 * (SOC.g.kg.weighted^4)) + (SD.cover.control^4)/(N.cover.control^2 * (Cover.SOC.control^4)) ),
    delta.Org.amend.SOC.LRR = Org.amend.SOC.LRR + 0.5 * ( (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) - (SD.org.amend.control^2)/(N.org.amend.control * (Org.amend.SOC.control^2)) ),
    delta.var.Org.amend.SOC.LRR = var.Org.amend.SOC.LRR + 0.5 * ( (SD.treatment^4)/(N.treatment^2 * (SOC.g.kg.weighted^4)) + (SD.org.amend.control^4)/(N.org.amend.control^2 * (Org.amend.SOC.control^4)) ),
    delta.Rotation.SOC.LRR = Rotation.SOC.LRR + 0.5 * ( (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) - (SD.rotation.control^2)/(N.rotation.control * (Rotation.SOC.control^2)) ),
    delta.var.Rotation.SOC.LRR = var.Rotation.SOC.LRR + 0.5 * ( (SD.treatment^4)/(N.treatment^2 * (SOC.g.kg.weighted^4)) + (SD.rotation.control^4)/(N.rotation.control^2 * (Rotation.SOC.control^4)) ),
    delta.Residue.mgmt.SOC.LRR = Residue.mgmt.SOC.LRR + 0.5 * ( (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) - (SD.res.control^2)/(N.res.control * (Residue.mgmt.SOC.control^2)) ),
    delta.var.Residue.mgmt.SOC.LRR = var.Residue.mgmt.SOC.LRR + 0.5 * ( (SD.treatment^4)/(N.treatment^2 * (SOC.g.kg.weighted^4)) + (SD.res.control^4)/(N.res.control^2 * (Residue.mgmt.SOC.control^4)) ),
    delta.Fertilizer.SOC.LRR = Fertilizer.SOC.LRR + 0.5 * ( (SD.treatment^2)/(N.treatment * (SOC.g.kg.weighted^2)) - (SD.fertilizer.control^2)/(N.fertilizer.control * (Fertilizer.SOC.control^2)) ),
    delta.var.Fertilizer.SOC.LRR = var.Fertilizer.SOC.LRR + 0.5 * ( (SD.treatment^4)/(N.treatment^2 * (SOC.g.kg.weighted^4)) + (SD.fertilizer.control^4)/(N.fertilizer.control^2 * (Fertilizer.SOC.control^4)) )
  ) -> d

d %>% 
  group_by(Paper, Trt.combo) %>%
  mutate(modified.gearys = case_when( (SOC.g.kg.weighted / SD.treatment) * (4 * N.treatment^(3.0 / 2.0) / (1 + 4*N.treatment)) >= 3 ~ TRUE,
                                      (SOC.g.kg.weighted / SD.treatment) * (4 * N.treatment^(3.0 / 2.0) / (1 + 4*N.treatment)) < 3 ~ FALSE)) -> d
  

d <- d[!is.na(d$MYP),]

d %>%
  group_by(Paper, Crop, Units) %>%
  mutate(Paper_crop_mean_yield = mean(Mean.yield),
         MYP_percent= MYP/Paper_crop_mean_yield) -> d

save("d", "d.yield.stability", file = "data/d.prepared.RData")

