source("code/libraries.R")
gs_ls()
d.raw <- gs_title("Long_term_yield _data")
gs_ws_ls(d.raw)
d.carbon.raw <- gs_read(ss=d.raw, ws = "Carbon")

d.carbon <- d.carbon.raw[,-20]
names(d.carbon)[1:5] <- c("Paper",
                   "DOI",
                   "Study_name",
                   "Years_of_study",
                   "Year_of_observation")

d.carbon %>% 
  fill(names(.)[c(1:2)]) %>% 
  group_by(DOI) %>%
  fill(names(.)[c(3:4,6:16,17,18)]) %>%
  separate(col = "Years_of_study", into = c("Year_started","Year_ended"), sep = "-") %>%
  ungroup() %>%
  mutate_if(grepl(names(.),pattern = "Yield|begin|end|start|length"), as.numeric) %>%
  mutate_if(is.character, as.factor) -> d.carbon

paste.drop.NA <- function(x, sep = ", ") {
  x <- gsub("^\\s+|\\s+$", "", x) 
  ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
  is.na(ret) <- ret == ""
  return(ret)
}

d.carbon$Trt.combo <- apply(d.carbon[,7:13], 1, paste.drop.NA)

## Merge trt.codes


d.carbon.trts <- read.csv("data/d.carbon.trts.csv")
str(d.carbon.trts)

d.carbon %>%
  inner_join(d.carbon.trts[,c(1,10,11,12)]) -> d.carbon

d.carbon <- d.carbon[!is.na(d.carbon$Trt.code),]


## Summarize within papers
d.carbon <- droplevels(d.carbon[-which(d.carbon$`Soil sample depth (cm)` %in% c(">115",">120")),])

d.carbon %>%
  dplyr::group_by(Paper) %>%
  filter(Year_of_observation == max(Year_of_observation)) %>%
  separate(`Soil sample depth (cm)`, into = c("Top.depth", "Bottom.depth"), sep = "-", remove = F) %>%
  mutate(Depth.increment=as.numeric(Bottom.depth) - as.numeric(Top.depth)) %>%
  do(mutate(., max.bottom = as.numeric(max(as.numeric((Bottom.depth)))))) %>%
  mutate(Depth.proportion =as.numeric(Depth.increment)/max.bottom) %>%
  mutate(Soil.kg.per.hectare = as.numeric(100000*Depth.increment)) -> d.carbon

d.carbon$Depth.proportion
d.carbon[is.na(d.carbon$Depth.proportion), "Depth.proportion"] <- 1

d.carbon <- d.carbon[as.numeric(d.carbon$Bottom.depth) < 50,]

## Filter out unusual C measurements, convert all SOC data to same units, assume BD of one

d.carbon %>%
  filter(
    `SOM or SOC` == "SOM"|
      `SOM or SOC` == "SOC"|
      `SOM or SOC` == "SOM (total)"|
      `SOM or SOC` == "SOC stock as equivalent soil mass"|
      `SOM or SOC` == "SOC stock"|
      `SOM or SOC` == "SOC content"|
      `SOM or SOC` == "SOC storage"|
      `SOM or SOC` == "SOC (total)"|
      `SOM or SOC` == "SOC Stock"|
      `SOM or SOC` == "TOC"|
      `SOM or SOC` == "Total C"|
      `SOM or SOC` == "Total SOC"|
      `SOM or SOC` == "SOC pool"
  ) -> d.carbon

d.carbon <- droplevels(d.carbon)

unique(d.carbon$`C Units`)
d.carbon <- droplevels(d.carbon[!d.carbon$`C Units` %in% "g kg-1 aggregates",])
d.carbon <- d.carbon[!d.carbon$`C Units` %in% "kg C m-2\n(on 450 kg m-2 soil)",]

d.carbon %>%
  mutate(SOC.g.kg = case_when(
    `C Units` == "%" ~ Amount/.1,
    #`C Units` == "kg C m-2\n(on 450 kg m-2 soil)" ~ Amount*1000/Soil.kg.per.hectare*1000,
    `C Units` == "kg m-2" ~ Amount*1000/Soil.kg.per.hectare*1000,
    `C Units` == "g kg-1" ~ Amount,
    `C Units` == "Mg ha-1" ~ Amount/Soil.kg.per.hectare*1000000,
    `C Units` == "T ha-1" ~ Amount/Soil.kg.per.hectare*1000000,
    `C Units` == "t ha-1" ~ Amount/Soil.kg.per.hectare*1000000
  )) -> d.carbon

d.carbon[d.carbon$`SOM or SOC` %in% c("SOM","SOM (total)"),"SOC.g.kg"] <- d.carbon[d.carbon$`SOM or SOC` %in% c("SOM","SOM (total)"),"SOC.g.kg"]*.58

##

d.carbon %>%
  group_by(Paper, Trt.combo, `Soil sample depth (cm)`) %>%
  mutate(SOC.g.kg.weighted = Depth.proportion*SOC.g.kg) %>%
  group_by(Paper, Trt.combo) %>%
  dplyr::summarise(SOC.g.kg.weighted =  sum(SOC.g.kg.weighted),
                   SOC.SD = sd(SOC.g.kg),
                   SOC.n = n()) -> d.carbon.summary

d.carbon.summary <- (d.carbon.summary[!d.carbon.summary$SOC.g.kg.weighted > 150,])

save("d.carbon.summary", file = "data/d.carbon.summary.RData")
