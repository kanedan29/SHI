source("code/analysis_LME.R")

## Graphs

d %>%
  filter(Crop != "Biomass",
         Crop != "Maize biomass",
         Crop != "Cotton lint",
         Crop != "Rice",
         Crop != "Oat",
         Crop != "Spring wheat") %>%
  
  ggplot(data = ., aes(x=SOC.g.kg.weighted, y = MYP_percent, color = Crop))+
  geom_point()

ggplot(data = d.yield.stability[!is.na(d.yield.stability$Tillage) & d.yield.stability$Crop %in% "Maize",], 
       aes(x = Tillage, y=MYP_percent, fill = Tillage))+
  geom_boxplot(outlier.shape = NA)+
  ylim(0,1.25)+
  xlab("Conservation tillage")+
  ylab("MYP as proportion of mean yield for site")+
  scale_fill_discrete(name="Conservation practice",
                      breaks=c("n", "y"),
                      labels=c("No", "Yes"))


ggplot(data = d.yield.stability[!is.na(d.yield.stability$Org.amend) & d.yield.stability$Crop %in% "Maize",], 
       aes(x = Org.amend, y=MYP_percent, fill = Org.amend))+
  geom_boxplot(outlier.shape = NA)+
  xlab("Organic amendments")+
  ylab(NULL)+
  ylim(0,1.25)+
  scale_fill_discrete(name="Conservation practice",
                      breaks=c("n", "y"),
                      labels=c("No", "Yes"))

ggplot(data = d.yield.stability[!is.na(d.yield.stability$Rotation) & d.yield.stability$Crop %in% "Maize",], 
       aes(x = Rotation, y=MYP_percent, fill = Rotation))+
  geom_boxplot(outlier.shape = NA)+
  xlab("Crop rotation")+
  ylab(NULL)+
  ylim(0,1.25)+
  scale_fill_discrete(name="Conservation practice",
                      breaks=c("n", "y"),
                      labels=c("No", "Yes"))

ggpubr::ggarrange(CT.MYP, OR.MYP, CR.MYP, ncol = 3, nrow = 1, common.legend = T)






ggplot(data = d.yield.stability[!is.na(d.yield.stability$Cover),], aes(x = Cover, y=MYP))+
  geom_boxplot()

ggplot(data = d.yield.stability[!is.na(d.yield.stability$Org.amend),], aes(x = Org.amend, y=MYP))+
  geom_boxplot()

ggplot(data = d.yield.stability[!is.na(d.yield.stability$Rotation),], aes(x = Rotation, y=MYP))+
  geom_boxplot()

d %>%
  group_by(Paper, Crop, Units) %>%
  mutate(Paper_crop_mean_yield = mean(Mean.yield),
         `MYP as percent of site mean`= MYP/Paper_crop_mean_yield) -> test

ggplot(data = test[test$Crop %in% "Maize",], aes(x = Tillage.SOC.LRR, y=`MYP as percent of site mean`))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data = test[test$Crop %in% "Maize",], aes(x = Org.amend.SOC.LRR, y=`MYP as percent of site mean`))+
  geom_point()

ggplot(data = test[test$Crop %in% "Maize",], aes(x = Rotation.SOC.LRR, y=`MYP as percent of site mean`))+
  geom_point()



View(d.yield.stability[!is.na(d.yield.stability$Cover),])