
varImpPlot(rf.model)

test <- data.frame(t(ldply(partialPlot(x = rf.model, 
                                       pred.data = d3[!is.na(d3$Total.Carbon),], 
                                       x.var = Total.Carbon))))

test.save <- test
#test <- test.save
test <- test[-1,]
names(test) <- c("Total Carbon (%)","Water retention (PWP)")  
test$`Total Carbon (%)` <- as.numeric(as.character(test$`Total Carbon (%)`))
test$`Water retention (PWP)` <- as.numeric(as.character(test$`Water retention (PWP)`))


c.pwp <-ggplot(data = test, aes(x = `Total Carbon (%)` , y = `Water retention (PWP)` ))+
  geom_point(data = d3, aes(x = Total.Carbon, y=PWP), shape = '.', alpha=0.8)+
  geom_line(color = "blue", size = 2, alpha = 0.7)+
  #geom_smooth(data = d2, aes(x = Total.Carbon, y=Water.Retention..15.bar...2mm), shape=".", alpha=0.3)+
  xlim(0,10)+ylim(0,50)
c.pwp

