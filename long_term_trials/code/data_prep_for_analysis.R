source("code/libraries.R")

load("data/d.carbon.summary.RData")
load("data/d.yield.stability.RData")

d.yield.stability <- d.yield.stability[d.yield.stability$MYP > -17000,]
d.yield.stability$Tillage[d.yield.stability$Tillage %in% "t"] <- "y"

d.carbon.trts <- read.csv("d.carbon.trts.csv")
str(d.carbon.trts)

d.carbon.summary %>%
  inner_join(d.carbon.trts[,c(1,10,11)]) -> d.carbon.summary

d.yield.stability %>%
  inner_join(d.carbon.summary[,c(1,3,4)]) %>%
  distinct(.) -> d.yield.stability.carbon

## Calculate LRRs

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Tillage == "n") %>%
  summarise(Tillage.SOC.control = mean(SOC.g.kg.weighted)) -> d.till.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Cover == "n") %>%
  summarise(Cover.SOC.control = mean(SOC.g.kg.weighted)) -> d.cover.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Org.amend == "n") %>%
  summarise(Org.amend.SOC.control = mean(SOC.g.kg.weighted)) -> d.org.amend.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Rotation == "n") %>%
  summarise(Rotation.SOC.control = mean(SOC.g.kg.weighted)) -> d.rotation.soc.control

d.yield.stability.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Fertilizer == "n") %>%
  summarise(Fertilizer.SOC.control = mean(SOC.g.kg.weighted)) -> d.fertilizer.soc.control

d.yield.stability.carbon %>%
  left_join(d.till.soc.control) %>%
  left_join(d.cover.soc.control) %>%
  left_join(d.org.amend.soc.control) %>%
  left_join(d.rotation.soc.control) %>%
  left_join(d.fertilizer.soc.control) %>%
  mutate(
    Tillage.SOC.LRR = log(SOC.g.kg.weighted/Tillage.SOC.control),
    Cover.SOC.LRR = log(SOC.g.kg.weighted/Cover.SOC.control),
    Org.amend.SOC.LRR = log(SOC.g.kg.weighted/Org.amend.SOC.control),
    Rotation.SOC.LRR = log(SOC.g.kg.weighted/Rotation.SOC.control),
    Fertilizer.SOC.LRR = log(SOC.g.kg.weighted/Fertilizer.SOC.control)
  ) %>%
  distinct(.) -> d

d <- d[!is.na(d$MYP),]

d %>%
  group_by(Paper, Crop, Units) %>%
  mutate(Paper_crop_mean_yield = mean(Mean.yield),
         MYP_percent= MYP/Paper_crop_mean_yield) -> d