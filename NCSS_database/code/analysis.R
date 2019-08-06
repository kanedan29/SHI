source("code/libraries.R")
source("code/data_NCSS_database_clean.R")


## Random forest analysis

rf.model <- randomForest(PWP~Soil_order+Total.Carbon+Total.Clay+Total.Silt+Total.Sand, 
                  data=d3,na.action = "na.omit")

#test <- partial(rf.model, partial.xvar = "Total.Carbon")

test <- data.frame(t(ldply(partialPlot(x = rf.model, 
                                       pred.data = d3[!is.na(d3$Total.Carbon),], 
                                       x.var = Total.Carbon))))

test.save <- test
#test <- test.save
test <- test[-1:2,]
names(test) <- c("Total Carbon (%)","Water retention (PWP)")  
test$`Total Carbon (%)` <- as.numeric(as.character(test$`Total Carbon (%)`))
test$`Water retention (PWP)` <- as.numeric(as.character(test$`Water retention (PWP)`))
test <- test[-c(1:2),]

c.pwp <-ggplot(data = test, aes(x = `Total Carbon (%)` , y = `Water retention (PWP)` ))+
  geom_point(data = d3, aes(x = Total.Carbon, y=PWP), shape = '.', alpha=0.3)+
  geom_line(color = "blue", size = 2, alpha = 0.7)+
  #geom_smooth(data = d2, aes(x = Total.Carbon, y=Water.Retention..15.bar...2mm), shape=".", alpha=0.3)+
  xlim(0,10)
c.pwp

ggplot(data = d3, aes(x = Total.Carbon, y = PWP))+
  geom_point(shape = '.', alpha=0.8)+
  facet_wrap(~Soil_order, nrow = 4,ncol = 3)

## LME analysis

# Model with no random shifts to slope
gls.model <- gls(PWP~Total.Carbon+Total.Clay+Total.Silt+Total.Sand,
                 na.action = na.omit, data = d3)
summary(gls.model)
#Model with random shifts to slope
lme.model <- lme(PWP~Total.Carbon+Total.Clay+Total.Silt+Total.Sand, random = ~1|Soil_order,
                 na.action = na.omit, data = d3)
summary(lme.model)
