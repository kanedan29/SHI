# Import packages and data ------------------------------------------------
source("code/0_libraries.R")
detach("package::plyr")
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

# Convert into a long dataframe -------------------------------

d_long <- d %>% 
  mutate(Paper_site = case_when(is.na(Site) ~ Paper, 
                                !is.na(Site) ~ paste(Paper, Site, sep = "_"))) %>%
  pivot_longer(Tillage:Weed.mgmt, names_to = "Con_practice", values_to = "yes_no")


# Pub bias ----------------------------------------------------------------
standard_error <- function(x) sd(x) / sqrt(length(x))

cover <- left_join(d_long %>%
                     filter(Con_practice %in% c("Cover","Rotation"), yes_no == "y"), 
                   d_long %>%
                     filter(Con_practice %in% c("Cover","Rotation"), yes_no == "n") %>%
                     group_by(Paper, Site) %>%
                     summarize(IR.control = mean(IR),
                               SOC.control = mean(SOC.g.kg.weighted))) %>%
  mutate(IR.LRR = log(IR/IR.control),
         SOC.LRR = log(SOC.g.kg.weighted/SOC.control)) %>%
  group_by(Paper_site) %>%
  mutate(IR.se = standard_error(IR.LRR), 
         SOC.se = standard_error(SOC.LRR),
         Paper_site_f = factor(Paper_site)) %>%
  ungroup(.)


cover %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = IR.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("Infiltration rate (log response ratio)")+
  xlab("")+
  coord_flip()


cover %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,SOC.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = SOC.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("Infiltration rate (log response ratio)")+
  xlab("")+
  coord_flip()


ggplot(data = cover, aes(x=IR.LRR)) + 
  geom_histogram(bins = 7)
  
hist(cover$SOC.LRR)

tillage <- left_join(d_long %>%
                     filter(Con_practice %in% c("Tillage"), yes_no == "y"), 
                   d_long %>%
                     filter(Con_practice %in% c("Tillage"), yes_no == "n") %>%
                     group_by(Paper, Site) %>%
                     summarize(IR.control = mean(IR),
                               SOC.control = mean(SOC.g.kg.weighted))) %>%
  mutate(IR.LRR = log(IR/IR.control),
         SOC.LRR = log(SOC.g.kg.weighted/SOC.control)) %>%
  group_by(Paper_site) %>%
  mutate(IR.se = standard_error(IR.LRR),
         SOC.se = standard_error(SOC.LRR),
         Paper_site_f = factor(Paper_site)) %>%
  ungroup(.)


tillage %>%
  ggplot(., aes(x = Paper_site, y = SOC.LRR)) +
  geom_point() +
  coord_flip()




ggplot(data = tillage, aes(x=IR.LRR)) + 
  geom_histogram(bins =5)




org.res <- left_join(d_long %>%
                       filter(Con_practice %in% c("Org.amend","Residue.mgmt"), yes_no == "y"), 
                     d_long %>%
                       filter(Con_practice %in% c("Org.amend","Residue.mgmt"), yes_no == "n") %>%
                       group_by(Paper, Site) %>%
                       summarize(IR.control = mean(IR),
                                 SOC.control = mean(SOC.g.kg.weighted))) %>%
  mutate(IR.LRR = log(IR/IR.control),
         SOC.LRR = log(SOC.g.kg.weighted/SOC.control)) %>%
  group_by(Paper_site) %>%
  mutate(IR.se = standard_error(IR.LRR),
         SOC.se = standard_error(SOC.LRR), 
         Paper_site_f = factor(Paper_site)) %>%
  ungroup(.)


org.res %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = IR.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("Infiltration rate (log response ratio)")+
  xlab("")+
  coord_flip()


org.res %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = SOC.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("SOC (log response ratio)")+
  xlab("")+
  coord_flip()


tillage %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = IR.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("Infiltration rate (log response ratio)")+
  xlab("")+
  coord_flip()



tillage %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = SOC.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("SOC (log response ratio)")+
  xlab("")+
  coord_flip()


cover %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = IR.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("Infiltration rate (log response ratio)")+
  xlab("")+
  coord_flip()


cover %>%
  mutate(Paper_site_f = fct_reorder(Paper_site_f,IR.se)) %>%
  ggplot(., aes(x = Paper_site_f, y = SOC.LRR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey")+
  geom_boxplot(width = 0.5, alpha = 0.8, color = "grey")+
  geom_point(size = 1.5) + 
  ylab("SOC (log response ratio)")+
  xlab("")+
  coord_flip()

