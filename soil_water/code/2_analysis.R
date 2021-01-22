
# Import packages and data ------------------------------------------------



source("code/0_libraries.R")
detach("package:plyr")
library(nlme)

d.mod <- read_rds("data/d.mod.rds") %>%
  filter(Paper != "Nyalemegbe et al. 2011") %>% #checked back on this paper, and it's not clear what the units are, so removing
  filter(IR.units != "Time for 500 mL (s)")
  



# Clean data,  normalize IR to cm/h ---------------------------------------

d <- d.mod %>%
  ungroup(.) %>%
  rename(Site=`Method/Site`) %>%
  mutate(
    IR.cmh = case_when(
      IR.units == "inch/hr" ~ IR*2.54,
      IR.units == "x10^-3 cm/min" ~ (IR/1000)*60,
      IR.units == "x10^-2 cm/min" ~ (IR/100)*60,
      IR.units == "mm/h" ~ IR / 10,
      IR.units == "mm/hr" ~ IR / 10,
      IR.units == "ln(cm/h)" ~ exp(IR),
      IR.units == "cm/d" ~ IR / 24,
      IR.units == "mm per 1.5h" ~ (IR / 1.5) / 10,
      IR.units == "cm/3h" ~ IR / 3,
      IR.units == "mm/s" ~ (IR / 10) * 360,
      IR.units == "cm/min (steady state)" ~ IR * 60,
      TRUE ~ as.numeric(.$IR)
    )
  ) %>%
  group_by(Paper) %>%
  mutate(IR.compress = scale(IR)) %>%
  distinct(.)

# Calculate LRR for in IR in each treatment -------------------------------

d.controls <- d %>%
  dplyr::group_by(Paper, Site) %>%
  filter(Control == "y") %>%
  dplyr::select(Paper, Site, IR, SOC.g.kg.weighted) %>%
  rename(IR.control = IR, SOC.control = SOC.g.kg.weighted)

d <- d %>%
  ungroup(.) %>%
  left_join(d.controls) %>%
  distinct(.) %>%
  mutate(
    IR.LRR = log(IR/IR.control),
    SOC.LRR = log(SOC.g.kg.weighted/SOC.control)
  ) %>%
  mutate(SOC.LRR.scaled = scale(SOC.LRR))


# IR_LRR ~ SOC_LRR models -------------------------------------------------

m.org.amend <- d %>%
  filter(!is.na(Org.amend), 
         is.na(Control)) %>%
  lme(IR.LRR ~ SOC.LRR, random = ~1|Paper/Site, data = ., na.action = "na.omit", weights = "Length_of_experiment")

m.tillage <- d %>%
  filter(!is.na(Tillage),
         is.na(Control)) %>%
  lme(IR.LRR ~ SOC.LRR, random = ~1|Paper/Site, data = ., na.action = "na.omit", weights = "Length_of_experiment")

m.cover <- d %>%
  filter(!is.na(Cover),
         is.na(Control)) %>%
  lme(IR.LRR ~ SOC.LRR, random = ~1|Paper/Site, data = ., na.action = "na.omit", weights = "Length_of_experiment")


# Generate df of IR_LRR ~ SOC_LRR model terms and create dotplot ----------

model.terms <-
  as.data.frame(intervals(m.org.amend, level = 0.95, which = 'fixed')[[1]]) %>%
  mutate(Conprac = "Organic amendment", term = row.names(.)) %>%
  bind_rows(
    as.data.frame(intervals(m.tillage, level = 0.95, which = 'fixed')[[1]]) %>%
      mutate(Conprac = "Tillage", term = row.names(.))) %>%
      bind_rows(
        as.data.frame(intervals(m.cover, level = 0.95, which = 'fixed')[[1]]) %>%
          mutate(Conprac = "Cover", term = row.names(.))) %>%
  mutate(term = case_when(term == "SOC.LRR" ~ "Soil Carbon LRR",
                          term == "(Intercept)" ~ "Intercept"))
  
ggplot(data = model.terms) + 
  geom_point(aes(x = term, y= est., color = Conprac))+
  geom_errorbar(aes(x = term, ymin = lower, ymax = upper, color = Conprac), width =0.5)+
  geom_abline(slope = 0, intercept = 0, color = "grey")+
  theme(axis.title = element_blank(), 
        legend.position = "none")+
  coord_flip() +
  facet_wrap(facets = "Conprac", nrow = 3, strip.position = "right") 
  

# IR cm/h ~ SOC + conservation practice models ----------------------------

m.cover <- d %>%
  filter(!is.na(Cover)) %>%
  lme(IR.cmh ~ SOC.g.kg.weighted + Cover, random = ~1|Paper/Site, data = ., na.action = "na.omit")
summary(m.cover)

m.tillage <- d %>%
  filter(!is.na(Tillage)) %>%
  lme(IR.cmh ~ SOC.g.kg.weighted + Tillage, random = ~1|Paper/Site, data = ., na.action = "na.omit")
summary(m.tillage)

m.org <- d %>%
  filter(!is.na(Org.amend)) %>%
  lme(IR.cmh ~ SOC.g.kg.weighted + Org.amend, random = ~1|Paper/Site, data = ., na.action = "na.omit")
summary(m.org)


# SOC ~ practice models ---------------------------------------------------

m.cover <- d %>%
  filter(!is.na(Cover)) %>%
  lme(SOC.g.kg.weighted ~ Cover, random = ~1|Paper/Site, data = ., na.action = "na.omit")
summary(m.cover)

m.tillage <- d %>%
  filter(!is.na(Tillage)) %>%
  lme(SOC.g.kg.weighted ~ Tillage, random = ~1|Paper/Site, data = ., na.action = "na.omit")
summary(m.tillage)

m.org <- d %>%
  filter(!is.na(Org.amend)) %>%
  lme(SOC.g.kg.weighted ~ Org.amend, random = ~1|Paper/Site, data = ., na.action = "na.omit")
summary(m.org)

