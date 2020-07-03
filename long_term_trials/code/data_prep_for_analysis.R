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

names(d.carbon.summary)[1] <- "Corresponding soil paper"

d.yield.stability %>%
  inner_join(d.carbon.summary[,c(1:5)]) %>%
  distinct(.) -> d.yield.stability.carbon.irr

d.yield.stability %>%
  anti_join(d.carbon.summary[,c(1:5)]) %>%
  distinct(.) -> dropped

d.yield.stability.carbon.irr %>%
  filter(Irrigation != "y" | is.na(Irrigation)) -> d.yield.stability.carbon

## Calculate LRRs

d.yield.stability.carbon %>%
  dplyr::group_by(`Corresponding soil paper`) %>%
  filter(Control == 1) %>%
  summarise(SOC.control = mean(SOC.g.kg.weighted),
            SD.SOC.control = case_when(n() == 1 ~ sum(SOC.SD),
                                       n() != 1 ~ sum(SOC.SD^2/SOC.n)),
            N.SOC.control = sum(SOC.n)) -> d.soc.control


d.yield.stability.carbon.irr %>%
  dplyr::group_by(`Corresponding soil paper`) %>%
  filter(Control == 1) %>%
  summarise(SOC.control = mean(SOC.g.kg.weighted),
            SD.SOC.control = case_when(n() == 1 ~ sum(SOC.SD),
                                       n() != 1 ~ sum(SOC.SD^2/SOC.n)),
            N.SOC.control = sum(SOC.n)) -> d.soc.control.irr



d.yield.stability.carbon %>%
  left_join(d.soc.control) %>%
  distinct(.) -> d.nonirr

d.yield.stability.carbon.irr %>%
  left_join(d.soc.control.irr) %>%
  distinct(.) -> d.irr

d.nonirr %>%
  mutate(SOC.LRR = log(SOC.g.kg.weighted/SOC.control),
         var.SOC.LRR = (SOC.SD^2)/(SOC.n * (SOC.g.kg.weighted^2)) + (SD.SOC.control^2)/(N.SOC.control * (SOC.control^2))
  ) -> d.nonirr

d.irr %>%
  mutate(SOC.LRR = log(SOC.g.kg.weighted/SOC.control),
         var.SOC.LRR = (SOC.SD^2)/(SOC.n * (SOC.g.kg.weighted^2)) + (SD.SOC.control^2)/(N.SOC.control * (SOC.control^2))
  ) -> d.irr

## Correct LRRs for bias

d.nonirr %>%
  mutate(
    delta.SOC.LRR = SOC.LRR + 0.5 * ( (SOC.SD^2)/(SOC.n * (SOC.g.kg.weighted^2)) - (SD.SOC.control^2)/(N.SOC.control * (SOC.control^2)) ),
    delta.var.SOC.LRR = var.SOC.LRR + 0.5 * ( (SOC.SD^4)/(SOC.n^2 * (SOC.g.kg.weighted^4)) + (SD.SOC.control^4)/(N.SOC.control^2 * (SOC.control^4)) )
  ) -> d.nonirr

d.irr %>%
  mutate(
    delta.SOC.LRR = SOC.LRR + 0.5 * ( (SOC.SD^2)/(SOC.n * (SOC.g.kg.weighted^2)) - (SD.SOC.control^2)/(N.SOC.control * (SOC.control^2)) ),
    delta.var.SOC.LRR = var.SOC.LRR + 0.5 * ( (SOC.SD^4)/(SOC.n^2 * (SOC.g.kg.weighted^4)) + (SD.SOC.control^4)/(N.SOC.control^2 * (SOC.control^4)) )
  ) -> d.irr

d.nonirr %>% 
  group_by(Paper, Trt.combo) %>%
  mutate(modified.gearys = case_when( (SOC.g.kg.weighted / SOC.SD) * (4 * SOC.n^(3.0 / 2.0) / (1 + 4*SOC.n)) >= 3 ~ TRUE,
                                      (SOC.g.kg.weighted / SOC.SD) * (4 * SOC.n^(3.0 / 2.0) / (1 + 4*SOC.n)) < 3 ~ FALSE)) -> d.nonirr

d.irr %>% 
  group_by(Paper, Trt.combo) %>%
  mutate(modified.gearys = case_when( (SOC.g.kg.weighted / SOC.SD) * (4 * SOC.n^(3.0 / 2.0) / (1 + 4*SOC.n)) >= 3 ~ TRUE,
                                      (SOC.g.kg.weighted / SOC.SD) * (4 * SOC.n^(3.0 / 2.0) / (1 + 4*SOC.n)) < 3 ~ FALSE)) -> d.irr  


d.nonirr <- d.nonirr[!is.na(d.nonirr$MYP),]
d.irr <- d.irr[!is.na(d.irr$MYP),]


save("d.nonirr", "d.irr", "d.yield.stability", file = "data/d.prepared.RData")

