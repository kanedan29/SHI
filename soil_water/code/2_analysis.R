source("code/0_libraries.R")
detach("package:plyr")

d <- read_excel("data/data_export.xlsx")
d <- d[,-c(4:10)]

## Summarize carbon data
d <- d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(`Year of observation`== max(as.numeric(`Year of observation`))) %>%
  filter(!is.na(IR)) %>%
  separate(Depth, into = c("Top.depth", "Bottom.depth"), sep = "-", remove = F) %>%
  mutate(Depth.increment=as.numeric(Bottom.depth) - as.numeric(Top.depth)) %>%
  do(mutate(., max.bottom = as.numeric(max(as.numeric((Bottom.depth)))))) %>%
  mutate(Depth.proportion =as.numeric(Depth.increment)/max.bottom) %>%
  ungroup(.)

d$Depth.proportion
d[is.na(d$Depth.proportion), "Depth.proportion"] <- 1

d.soc <- d %>%
  group_by(Paper, Trt.combo, `Method/Site`, Depth) %>%
  mutate(SOC.g.kg.weighted = Depth.proportion*SOC) %>%
  group_by(Paper, Trt.combo, `Method/Site`) %>%
  dplyr::summarise(SOC.g.kg.weighted =  sum(SOC.g.kg.weighted))

d <- distinct(d[,c(1,2,3,4,5,11,12)]) %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  ungroup(.) %>%
  left_join(d.soc)

d.trts <- read_excel("data/d.trts.xlsx")

d <- d %>%
  left_join(d.trts) %>%
  distinct(.)

# fix variables in d

d <- d %>%
  mutate(IR.units = case_when(IR.units == "None" ~ "cm/h",
                              IR.units == "cm/hr" ~ "cm/h",
                              IR.units == "mm/hr" ~ "mm/h",
                              TRUE ~ .$IR.units),
         IR = case_when(IR.units == "mm/h" ~ IR/10,
                        IR.units == "ln(cm/h)" ~ exp(IR),
                        TRUE ~ as.numeric(.$IR))) %>%
  mutate(IR.units = case_when(IR.units == "mm/h" ~ "cm/h",
                       IR.units == "ln(cm/h)" ~ "cm/h",
                       TRUE ~ .$IR.units)) %>%
  distinct(.)


## Calculate LRR for IR within each treatment

d.controls <- d %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(Control == "y") %>%
  dplyr::select(Paper,`Method/Site`, IR, SOC.g.kg.weighted) %>%
  rename(IR.control = IR, SOC.control = SOC.g.kg.weighted)

d.test <- d %>%
  ungroup(.) %>%
  left_join(d.controls) %>%
  mutate(
    IR.LRR = log(IR/IR.control),
    SOC.LRR = log(SOC.g.kg.weighted/SOC.control)
  )

# d %>%
#   dplyr::group_by(Paper, `Method/Site`) %>%
#   filter(Tillage == "n", Control == "y") %>%
#   summarise(Tillage.IR.control = mean(IR)) -> d.till.control
#   
# d %>%
#   dplyr::group_by(Paper, `Method/Site`) %>%
#   filter(Cover == "n", Control == "y") %>%
#   summarise(Cover.IR.control = mean(IR)) -> d.cover.control
# 
# d %>%
#   dplyr::group_by(Paper, `Method/Site`) %>%
#   filter(Org.amend == "n", Control == "y") %>%
#   summarise(Org.amend.IR.control = mean(IR)) -> d.org.amend.control
# 
# 
# d %>%
#   dplyr::group_by(Paper, `Method/Site`) %>%
#   filter(Tillage == "n", Control == "y") %>%
#   summarise(Tillage.SOC.control = mean(SOC.g.kg.weighted)) -> d.till.soc.control
# 
# d %>%
#   dplyr::group_by(Paper, `Method/Site`) %>%
#   filter(Cover == "n", Control == "y") %>%
#   summarise(Cover.SOC.control = mean(SOC.g.kg.weighted)) -> d.cover.soc.control
# 
# d %>%
#   dplyr::group_by(Paper, `Method/Site`) %>%
#   filter(Org.amend == "n", Control == "y") %>%
#   summarise(Org.amend.SOC.control = mean(SOC.g.kg.weighted)) -> d.org.amend.soc.control
# 
# 
# d <- d %>%
#   ungroup(.) %>%
#   left_join(d.till.control) %>%
#   left_join(d.cover.control) %>%
#   left_join(d.org.amend.control) %>%
#   left_join(d.till.soc.control) %>%
#   left_join(d.cover.soc.control) %>%
#   left_join(d.org.amend.soc.control) %>%
#   mutate(
#     Tillage.IR.LRR = log(IR/Tillage.IR.control),
#     Cover.IR.LRR = log(IR/Cover.IR.control),
#     Org.amened.IR.LRR = log(IR/Org.amend.IR.control),
#     Tillage.SOC.LRR = log(SOC.g.kg.weighted/Tillage.SOC.control),
#     Cover.SOC.LRR = log(SOC.g.kg.weighted/Cover.SOC.control),
#     Org.amened.SOC.LRR = log(SOC.g.kg.weighted/Org.amend.SOC.control))

# Models
d <- d.test

summary(lmerTest::lmer(IR.LRR ~ SOC.g.kg.weighted + (1|Paper), data = d))

summary(lme(IR ~ SOC.g.kg.weighted+Tillage+Cover+Org.amend, 
            random = ~1|Paper, data = d, na.action = "na.omit"))

summary(lme(IR ~ SOC.g.kg.weighted*Cover, 
            random = ~1|Paper, data = d, na.action = "na.omit"))

summary(lme(IR ~ SOC.g.kg.weighted*Org.amend, 
            random = ~1|Paper, data = d, na.action = "na.omit"))

View(d %>%
       filter(!is.na(Org.amend)))


ggplot(data = d, aes(x=SOC.LRR, y=IR.LRR))+
  geom_point()

##

summary(gls(Tillage.IR.LRR ~ Tillage.SOC.LRR, data = d[!is.na(d$Tillage),]))

ggplot(data = d, aes(x=Tillage.SOC.LRR, y=Tillage.IR.LRR))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Relative impact of conservation tillage on SOC")+
  ylab("Relative impact of\nconservation tillage on IR")+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")

test <- d[!is.na(d$Tillage.SOC.LRR),]

summary(gls(Cover.IR.LRR ~ Cover.SOC.LRR, data = d[!is.na(d$Cover),]))

ggplot(data = d, aes(x=Cover.SOC.LRR, y=Cover.IR.LRR))+
  geom_point()+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")

summary(gls(Org.amened.IR.LRR ~ Org.amened.SOC.LRR, data = d[!is.na(d$Org.amend),]))

ggplot(data = d, aes(x=Org.amened.SOC.LRR, y=Org.amened.IR.LRR))+
  geom_point()+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")



unique(d$Paper)
 




