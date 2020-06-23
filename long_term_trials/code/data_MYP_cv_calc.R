source("code/libraries.R")
# source("code/data_long_term_yield.R")

load("data/d.yield.RData")
## MYP analysis

# Calculate environmental index (EI) and add to dataframe
# EI = annual mean yield per hectare (Williams et al. 2016)
# calculated by study and crop

d %>%
  group_by(Crop) %>%
  mutate(Yield.var.by.crop = var(Yield.kg.per.hectare)) -> d

d %>%
  group_by(Paper,DOI,Study_name,Crop, end_obs) %>%
  mutate(Yield.weighted = 10000*Yield.kg.per.hectare/(var(.$Yield.kg.per.hectare) + Yield.var.by.crop),
         EI.by.year = mean(Yield.kg.per.hectare),
         EI.by.year.weighted = mean(Yield.weighted)) %>%
  ungroup() %>%
  as.data.frame(.) -> d

d$Trt.combo <- as.factor(d$Trt.combo)
d <- d[!is.na(d$Trt.combo),]

all.list <- split(x = d,f=interaction(d$Paper,d$Study_name, d$Crop,d$Units), drop = T) 

all.list <- lapply(all.list, function(x) droplevels(x))
all.list2 <- all.list

all.list <- lapply(all.list, FUN = function(z) {
  z<- nlme::groupedData(Yield.kg.per.hectare~EI.by.year|Trt.combo,
                        data=z,
                        labels=list(x="EI.by.year", y = "Yield"))})

all.list.2 <- lapply(all.list, FUN = function(z) { # Find out why this returns NA for some observations
z<- nlme::groupedData(Yield.weighted~EI.by.year.weighted|Trt.combo,
                      data=z,
                      labels=list(x="EI.by.year.weighted", y = "Yield.weighted"))})

all.list.pred <- lapply(all.list, function(x){
  tryCatch(predict(nlme::lme(Yield.kg.per.hectare ~ EI.by.year,
                    data=x,random = ~EI.by.year|Trt.combo, 
                    method='ML',na.action="na.omit",
                    control=lmeControl(opt = 'optim'))), error=function(e) NULL)})

all.list.pred.2 <- lapply(all.list2, function(x){
  tryCatch(predict(nlme::lme(Yield.weighted ~ EI.by.year.weighted,
                             data=x,random = ~1|Trt.combo,
                             method='ML',na.action="na.omit",
                             control=lmeControl(opt = 'optim'))), error=function(e) NULL)})

all.list.pred <-Filter(Negate(is.null), all.list.pred)  
all.list.pred <- lapply(all.list.pred, function(x) as.data.frame(cbind(names(x), x)))

all.list.pred.2 <-Filter(Negate(is.null), all.list.pred.2)
all.list.pred.2 <- lapply(all.list.pred.2, function(x) as.data.frame(cbind(names(x), x)))

all.list.pred.max <- plyr::ldply(all.list.pred)
all.list.pred.max$x <- round(as.numeric(as.character(all.list.pred.max$x)), digits = 4 )

all.list.pred.max.2 <- plyr::ldply(all.list.pred.2)
all.list.pred.max.2$x <- round(as.numeric(as.character(all.list.pred.max.2$x)), digits = 4 )

all.list.pred.max %>%
  group_by(.id,V1) %>%
  slice(which.min(x)) %>%
  rename(Paper.crop = .id,
         Trt.combo = V1, 
         MYP = x) -> d.MYP

d.MYP[d.MYP$MYP < 0, "MYP"] <- 0

all.list.pred.max.2 %>%
  group_by(.id,V1) %>%
  slice(which.min(x)) %>%
  rename(Paper.crop = .id,
         Trt.combo = V1,
         MYP.weighted = x) -> d.MYP.weighted 

d.MYP.weighted[d.MYP.weighted$MYP.weighted < 0, "MYP.weighted"] <- 0


  #group_by(.id) %>%
  #slice(which.max(x)) -> d.MYP

## CV analysis

cv <- function(x){(sd(x)/mean(x))}

d %>%
  group_by(Paper, Study_name, `Corresponding soil paper`, Trt.combo, Crop, Units) %>%
  summarise(Mean.yield.weighted = mean(Yield.weighted),
            CV.yield = cv(Yield.kg.per.hectare),
            CV.yield.weighted = cv(Yield.weighted)) %>% 
  group_by(Paper, Crop, Units) -> d.cv.summary

d %>% 
  group_by(Paper, Study_name, `Corresponding soil paper`, Crop, Units) %>%
  summarise(Mean.EI.weighted.all.trts.all.yrs = mean(EI.by.year.weighted)) -> d.EI

#slice(which.min(CV.yield)) 

d.cv.summary$Paper.crop <- paste(d.cv.summary$Paper, d.cv.summary$Study_name, d.cv.summary$Crop, d.cv.summary$Units, sep = ".")

# Climate summary
d %>%
  group_by(Paper, Study_name, AEZ, Trt.combo, Crop, Units) %>%
  summarise(Mean.tmin = mean(tmin),
            Mean.tmax = mean(tmax),
            Mean.ppt = mean(ppt),
            # Mean.soil = mean(soil),
            Mean.pet = mean(pet),
            # Mean.aet = mean(aet),
            # Mean.def = mean(def),
            # Mean.clim_PC1 = mean(clim_PC1),
            # Mean.clim_PC2 = mean(clim_PC2),
            # Mean.PDSI = mean(PDSI),
            Mean.SPEI.1 = mean(SPEI.1),
            Mean.SPEI.3 = mean(SPEI.3),
            Mean.SPEI.6 = mean(SPEI.6),
            Mean.SPEI.9 = mean(SPEI.9),
            Mean.SPEI.12 = mean(SPEI.12)) %>%
  group_by(Paper, Crop, Units) -> d.climate.summary

## Join yield stabilty dfs
  
d.cv.summary %>%
  left_join(d.climate.summary) %>%
  left_join(d.MYP) %>%
  left_join(d.MYP.weighted) %>%
  left_join(d.EI) -> d.yield.stability

d.yield.stability %>%
  group_by(Paper, Study_name, `Corresponding soil paper`, Trt.combo, Crop, Units) %>%
  mutate(MYP.weighted.percent = MYP.weighted/Mean.EI.weighted.all.trts.all.yrs) -> d.yield.stability

## Read in treatment codes for analysis

d.trt.codes <- read.xlsx("data/d.trts.all.papers.xlsx")
d.trt.codes <- d.trt.codes[, c(1,11,13:21)]

d.yield.stability %>%
  left_join(d.trt.codes) %>%
  distinct(.) -> d.yield.stability

save("d.yield.stability", file = "data/d.yield.stability.RData")
  
  
