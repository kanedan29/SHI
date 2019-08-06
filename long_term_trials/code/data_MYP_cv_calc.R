source("code/libraries.R")
source("code/data_long_term_yield.R")

## MYP analysis

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

## Join yield stabilty dfs
  
d.cv.summary %>%
  left_join(d.MYP) -> d.yield.stability

## Read in treatment codes for analysis

d.trt.codes <- read.xlsx("data/d.trts.all.papers.xlsx")
d.trt.codes <- d.trt.codes[, c(1,11,13:19)]

d.yield.stability %>%
  left_join(d.trt.codes) %>%
  left_join(d.trts[,c(1,11,12)]) %>%
  distinct(.) -> d.yield.stability

save("d.yield.stability", file = "data/d.yield.stability.RData")
  
  
