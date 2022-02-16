
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
  pivot_longer(Tillage:Weed.mgmt, names_to = "Con_practice", values_to = "yes_no") %>%
  mutate(Paper_site = case_when(is.na(Site) ~ Paper, 
                                !is.na(Site) ~ paste(Paper, Site, sep = ", ")))


# IR_LRR ~ SOC_LRR models -------------------------------------------------


#* Cover -------------------------------------------------------------------

cover <- left_join(d_long %>%
                    filter(Con_practice %in% c("Cover","Rotation"), yes_no == "y"), 
                  d_long %>%
                    filter(Con_practice %in% c("Cover","Rotation"), yes_no == "n") %>%
                    group_by(Paper, Site) %>%
                    summarize(IR.control = mean(IR),
                              SOC.control = mean(SOC.g.kg.weighted))) %>%
  mutate(IR.LRR = log(IR/IR.control),
         SOC.LRR = log(SOC.g.kg.weighted/SOC.control)) %>%
  ungroup(.)


m.cover <- cover %>%
  lme(IR.LRR ~ SOC.LRR, random = ~1|Paper_site, data = ., na.action = "na.omit", weights = "Length_of_experiment")
summary(m.cover)

cover_nested <- cover %>%
  nest(data = -Paper)

cover_list <- list()

for(i in 1:nrow(cover_nested)){
  
  cover_list[[i]] <- unnest(cover_nested[-i,], cols=c(data))
  
}
names(cover_list) <- cover_nested$Paper

cover_models <- plyr::ldply(cover_list, function(x){
  
  temp <- lme(IR.LRR ~ SOC.LRR, random = ~1|Paper_site, data = x, na.action = "na.omit", weights = "Length_of_experiment")
    
  return(as.data.frame(intervals(temp, level = 0.95, which = 'fixed')[[1]]) %>%
           mutate(Conprac = "Cover", term = row.names(.) ))
  
})
  
m.cover.intervals <- as.data.frame(intervals(m.cover)[[1]]) %>%
                mutate(Conprac = "Cover", term = row.names(.) ) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Other effects",
                          term == "SOC.LRR" ~ "Soil carbon LRR"))

cover_jack <- cover_models %>%
  mutate(Paper = .id,
         term = case_when(term == "(Intercept)" ~ "Other effects",
                          term == "SOC.LRR" ~ "Soil carbon LRR")) %>%
  ggplot(data = .) +
  geom_hline(data = m.cover.intervals, aes(yintercept = est.), linetype = "dashed", color = "dark grey") +
  geom_hline(data = m.cover.intervals, aes(yintercept = lower), color = "dark grey") +
  geom_hline(data = m.cover.intervals, aes(yintercept = upper), color = "dark grey")+
  geom_point(aes(x = Paper, y= est.))+
  geom_errorbar(aes(x = Paper, ymin = lower, ymax = upper), width =0.5)+
  theme(axis.title = element_blank(), 
        legend.position = "none")+
  coord_flip() +
  facet_wrap(facets = "term", ncol = 2)


#* Tillage -------------------------------------------------------------------

tillage <- left_join(d_long %>%
                     filter(Con_practice == "Tillage", yes_no == "y"), 
                   d_long %>%
                     filter(Con_practice == "Tillage", yes_no == "n") %>%
                     group_by(Paper, Site) %>%
                     summarize(IR.control = mean(IR),
                               SOC.control = mean(SOC.g.kg.weighted))) %>%
  mutate(IR.LRR = log(IR/IR.control),
         SOC.LRR = log(SOC.g.kg.weighted/SOC.control))



m.tillage <- tillage %>%
  lme(IR.LRR ~ SOC.LRR, random = ~1|Paper_site, data = ., na.action = "na.omit", weights = "Length_of_experiment")
summary(m.tillage)


tillage_nested <- tillage %>%
  nest(data = -Paper)

tillage_list <- list()

for(i in 1:nrow(tillage_nested)){
  
  tillage_list[[i]] <- unnest(tillage_nested[-i,], cols=c(data))
  
}
names(tillage_list) <- tillage_nested$Paper

tillage_models <- plyr::ldply(tillage_list, function(x){
  
  temp <- lme(IR.LRR ~ SOC.LRR, random = ~1|Paper_site, data = x, na.action = "na.omit", weights = "Length_of_experiment")
  
  return(as.data.frame(intervals(temp, level = 0.95, which = 'fixed')[[1]]) %>%
           mutate(Conprac = "Tillage", term = row.names(.) ))
  
})

m.tillage.intervals <- as.data.frame(intervals(m.tillage)[[1]]) %>%
  mutate(Conprac = "Tillage", term = row.names(.) ) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Other effects",
                          term == "SOC.LRR" ~ "Soil carbon LRR"))

till_jack <- tillage_models %>%
  mutate(Paper = .id,
         term = case_when(term == "(Intercept)" ~ "Other effects",
                          term == "SOC.LRR" ~ "Soil carbon LRR")) %>%
  ggplot(data = .) +
  geom_hline(data = m.tillage.intervals, aes(yintercept = est.), linetype = "dashed", color = "dark grey") +
  geom_hline(data = m.tillage.intervals, aes(yintercept = lower), color = "dark grey") +
  geom_hline(data = m.tillage.intervals, aes(yintercept = upper), color = "dark grey")+
  geom_point(aes(x = Paper, y= est.))+
  geom_errorbar(aes(x = Paper, ymin = lower, ymax = upper), width =0.5)+
  theme(axis.title = element_blank(), 
        legend.position = "none")+
  coord_flip() +
  facet_wrap(facets = "term", ncol = 2, scales = "free_x")



#* Organic amendments and residue -------------------------------------------------------------------

org.res <- left_join(d_long %>%
                       filter(Con_practice %in% c("Org.amend","Residue.mgmt"), yes_no == "y"), 
                     d_long %>%
                       filter(Con_practice %in% c("Org.amend","Residue.mgmt"), yes_no == "n") %>%
                       group_by(Paper, Site) %>%
                       summarize(IR.control = mean(IR),
                                 SOC.control = mean(SOC.g.kg.weighted))) %>%
  mutate(IR.LRR = log(IR/IR.control),
         SOC.LRR = log(SOC.g.kg.weighted/SOC.control))


m.org.res <- org.res %>%
  lme(IR.LRR ~ SOC.LRR, random = ~1|Paper_site, data = ., na.action = "na.omit", weights = "Length_of_experiment")
summary(m.org.res)




org_nested <- org.res %>%
  nest(data = -Paper)

org_list <- list()

for(i in 1:nrow(org_nested)){
  
  org_list[[i]] <- unnest(org_nested[-i,], cols=c(data))
  
}
names(org_list) <- org_nested$Paper

org_models <- plyr::ldply(org_list, function(x){
  
  temp <- lme(IR.LRR ~ SOC.LRR, random = ~1|Paper_site, data = x, na.action = "na.omit", weights = "Length_of_experiment")
  
  return(as.data.frame(intervals(temp, level = 0.95, which = 'fixed')[[1]]) %>%
           mutate(Conprac = "Org/res", term = row.names(.) ))
  
})

m.org.intervals <- as.data.frame(intervals(m.org.res)[[1]]) %>%
  mutate(Conprac = "Tillage", term = row.names(.) ) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Other effects",
                          term == "SOC.LRR" ~ "Soil carbon LRR"))

org_jack <- org_models %>%
  mutate(Paper = .id,
         term = case_when(term == "(Intercept)" ~ "Other effects",
                          term == "SOC.LRR" ~ "Soil carbon LRR")) %>%
  ggplot(data = .) +
  geom_hline(data = m.org.intervals, aes(yintercept = est.), linetype = "dashed", color = "dark grey") +
  geom_hline(data = m.org.intervals, aes(yintercept = lower), color = "dark grey") +
  geom_hline(data = m.org.intervals, aes(yintercept = upper), color = "dark grey")+
  geom_point(aes(x = Paper, y= est.))+
  geom_errorbar(aes(x = Paper, ymin = lower, ymax = upper), width =0.5)+
  theme(axis.title = element_blank(), 
        legend.position = "none")+
  coord_flip() +
  facet_wrap(facets = "term", ncol = 2, scales = "free_x")



# combine jacknife plots --------------------------------------------------


ggpubr::ggarrange(cover_jack, org_jack, till_jack, ncol = 1, nrow = 3, 
                  labels = c("a)","b)","c)"))



# Generate df of IR_LRR ~ SOC_LRR model terms and create dotplot ----------

model.terms <-
  as.data.frame(intervals(m.org.res, level = 0.95, which = 'fixed')[[1]]) %>%
  mutate(Conprac = "Organic amendment", term = row.names(.)) %>%
  bind_rows(
    as.data.frame(intervals(m.tillage, level = 0.95, which = 'fixed')[[1]]) %>%
      mutate(Conprac = "Reduced tillage", term = row.names(.))) %>%
  bind_rows(
    as.data.frame(intervals(m.cover, level = 0.95, which = 'fixed')[[1]]) %>%
      mutate(Conprac = "Living cover", term = row.names(.))) %>%
  mutate(term = case_when(term == "SOC.LRR" ~ "Soil Carbon LRR",
                          term == "(Intercept)" ~ "Other effects"))

ggplot(data = model.terms) + 
  geom_point(aes(x = term, y= est., color = Conprac))+
  geom_text(aes(x = term, y= est., label = round(est., digits = 2)), nudge_x = 0.4, size = 3)+
  geom_errorbar(aes(x = term, ymin = lower, ymax = upper, color = Conprac), width =0.5)+
  scale_color_brewer(palette="Dark2") + 
  geom_abline(slope = 0, intercept = 0, color = "grey")+
  theme(axis.title = element_blank(), 
        legend.position = "none")+
  coord_flip() +
  facet_wrap(facets = "Conprac", nrow = 3, strip.position = "right") 



# Practice impacts --------------------------------------------------------

test <- list(cover, org.res, tillage)
names(test) <- c("Living cover", "Organic amendment", "Reduced tillage")


test <- plyr::ldply(test) %>%
  rename("conprac" = ".id")

ir.dist <- ggplot(test, aes(x = IR.LRR)) +
  geom_vline(xintercept = 0, color = "grey")+
  geom_density(adjust = 1.5)+
  geom_dotplot()+
  facet_wrap(facets = "conprac", nrow = 3, strip.position = "right") +
  ylab("")+
  xlab("Infiltration rate LRR")


soc.dist <- ggplot(test, aes(x = SOC.LRR)) +
  geom_vline(xintercept = 0, color = "grey")+
  geom_density(adjust = 1.5)+
  #geom_dotplot()+
  facet_wrap(facets = "conprac", nrow = 3, strip.position = "right") +
  ylab("")+
  xlab("Soil carbon LRR")


ggpubr::ggarrange(ir.dist, soc.dist)


#

test %>%
  filter(!is.na(IR.LRR)) %>%
  dplyr::mutate(conprac = as.factor(conprac)) %>%
  dplyr::group_by(conprac) %>%
  dplyr::summarise(mean_lrr = mean(IR.LRR, na.rm = T),
            low = mean(IR.LRR,na.rm = T) - sd(IR.LRR, na.rm = T),
            high = mean(IR.LRR,na.rm = T) + sd(IR.LRR, na.rm = T))

# Soc by practice models --------------------------------------------------


m.cover <- d_long %>%
  filter(Con_practice %in% c("Cover","Rotation"),
         !is.na(yes_no)) %>%
  lme(SOC.g.kg.weighted ~ yes_no, random = ~1|Paper_site, data = ., na.action = "na.omit", weights = "Length_of_experiment")
summary(m.cover)

m.tillage <- d_long %>%
  filter(Con_practice == "Tillage",
         !is.na(yes_no)) %>%
  lme(SOC.g.kg.weighted ~ yes_no, random = ~1|Paper_site, data = ., na.action = "na.omit", weights = "Length_of_experiment")
summary(m.tillage)

m.org <- d_long %>%
  filter(Con_practice %in% c("Org.amend","Residue.mgmt"),
         !is.na(yes_no)) %>%
  lme(SOC.g.kg.weighted ~ yes_no, random = ~1|Paper_site, data = ., na.action = "na.omit", weights = "Length_of_experiment")
summary(m.org)


