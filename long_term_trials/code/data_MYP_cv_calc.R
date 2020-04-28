source("code/libraries.R")
source("code/data_long_term_yield.R")

## MYP analysis

# Calculate environmental index (EI) and add to dataframe
# EI = annual mean yield per hectare (Williams et al. 2016)
# calculated by study and crop

d %>%
  group_by(Paper,DOI,Study_name,Crop,end_obs) %>%
  mutate(EI.by.year = mean(Yield.kg.per.hectare)) %>%
  ungroup() %>%
  as.data.frame(.) -> d

d$Trt.combo <- as.factor(d$Trt.combo)
d <- d[!is.na(d$Trt.combo),]

all.list <- split(x = d,f=interaction(d$Paper,d$Study_name, d$Crop,d$Units), drop = T) 

all.list <- lapply(all.list, function(x) droplevels(x))

all.list <- lapply(all.list, FUN = function(z) {
  z<- nlme::groupedData(Yield.kg.per.hectare~EI.by.year|Trt.combo,
                        data=z,
                        labels=list(x="EI.by.year", y = "Yield"))})

all.list.pred <- lapply(all.list, function(x){
  tryCatch(predict(nlme::lme(Yield.kg.per.hectare ~ EI.by.year,
                    data=x,random = ~EI.by.year|Trt.combo, 
                    method='ML',na.action="na.omit",
                    control=lmeControl(opt = 'optim'))), error=function(e) NULL)})

all.list.pred <-Filter(Negate(is.null), all.list.pred)  

all.list.pred <- lapply(all.list.pred, function(x) as.data.frame(cbind(names(x), x)))
all.list.pred.max <- plyr::ldply(all.list.pred)
all.list.pred.max$x <- round(as.numeric(as.character(all.list.pred.max$x)), digits = 4 )

all.list.pred.max %>%
  group_by(.id,V1) %>%
  slice(which.min(x)) %>%
  rename(Paper.crop = .id,
         Trt.combo = V1, 
         MYP = x) -> d.MYP


  #group_by(.id) %>%
  #slice(which.max(x)) -> d.MYP

## CV analysis

cv <- function(x){(sd(x)/mean(x))}

d %>%
  group_by(Paper, Study_name, Trt.combo,Crop, Units) %>%
  summarise(Mean.yield = mean(Yield.kg.per.hectare),
            CV.yield = cv(Yield.kg.per.hectare)) %>%
  group_by(Paper, Crop, Units) -> d.cv.summary
#slice(which.min(CV.yield)) 

d.cv.summary$Paper.crop <- paste(d.cv.summary$Paper, d.cv.summary$Study_name, d.cv.summary$Crop, d.cv.summary$Units, sep = ".")

# Climate summary
d %>%
  group_by(Paper, Study_name, Trt.combo, Crop, Units) %>%
  summarise(Mean.tmin = mean(tmin),
            Mean.tmax = mean(tmax),
            Mean.ppt = mean(ppt),
            Mean.soil = mean(soil),
            Mean.pet = mean(pet),
            Mean.aet = mean(aet),
            Mean.def = mean(def),
            Mean.clim_PC1 = mean(clim_PC1),
            Mean.clim_PC2 = mean(clim_PC2),
            Mean.PDSI = mean(PDSI),
            Mean.SPEI.1 = mean(SPEI.1),
            Mean.SPEI.3 = mean(SPEI.3),
            Mean.SPEI.6 = mean(SPEI.6),
            Mean.SPEI.9 = mean(SPEI.9),
            Mean.SPEI.12 = mean(SPEI.12)) %>%
  group_by(Paper, Crop, Units) -> d.climate.summary

## Join yield stabilty dfs
  
d.cv.summary %>%
  left_join(d.climate.summary) %>%
  left_join(d.MYP) -> d.yield.stability

## Read in treatment codes for analysis

d.trt.codes <- read.xlsx("data/d.trts.all.papers.xlsx")
d.trt.codes <- d.trt.codes[, c(1,11,13:19)]

d.yield.stability %>%
  left_join(d.trt.codes) %>%
  left_join(d.trts[,c(1,11,12)]) %>%
  distinct(.) -> d.yield.stability

save("d.yield.stability", file = "data/d.yield.stability.RData")
  
  
