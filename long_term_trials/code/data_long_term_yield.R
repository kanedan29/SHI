source("code/libraries.R")

gs_ls()

d.raw <- gs_title("Long_term_yield _data")
gs_ws_ls(d.raw)
d.raw <- gs_read(ss=d.raw, ws = "Yield")

d <- d.raw[,-17]
names(d)[1:5] <- c("Paper",
                   "DOI",
                   "Study_name",
                   "Years_of_study",
                   "Year_of_observation")

## Clean it all up

d %>% 
  fill(names(.)[c(1:3)]) %>% 
  group_by(DOI) %>%
  fill(names(.)[c(4,6:16)]) %>%
  separate(col = "Years_of_study", into = c("Year_started","Year_ended"), sep = "-") %>%
  ungroup() %>%
  separate(Year_of_observation,c("begin_obs","end_obs"),remove = F) %>%
  mutate(end_obs = ifelse(is.na(end_obs), begin_obs,
                          ifelse(as.numeric(end_obs) < 67, paste("20",end_obs,sep=""),
                                 ifelse(as.numeric(end_obs) < 100,paste("19",end_obs, sep = ""),end_obs)))) %>%
  mutate(obs_length = as.numeric(end_obs) - as.numeric(begin_obs)) %>%
  mutate_if(grepl(names(.),pattern = "Yield|begin|end|start|length"), as.numeric) -> d

d <- d[d$obs_length <= 1,]
d <- d[!is.na(d$Yield),]
d <- d[-which(d$Units %in% "g kg-1"),]
d <- droplevels(d)
levels(d$Units)

# Make new column with yield in kg per hectare for all obsvervations
# Giese et al. 2014 grape paper, vines planted at 1993 vines/ha per methods section, multiply by 1993 to change to kg ha-1
# Papers with units in tons per hectare, assuming metric tons per hectare (i.e. Mg per hectare)
d <- d %>%
  mutate(
    Yield.kg.per.hectare = case_when(
      Units == "kg DM ha-1"| Units == "kg ha-1" | Units == "kg hm-2" ~ Yield,
      Units == "g m-2" ~ Yield*10,
      Units == "kg vine-1" ~ Yield*1993,
      Units == "Mg (biomass) ha-1"| Units == "Mg ha-1" |
        Units == "t DM ha-1"| Units == "t ha-1" | Units == "ton hm-2" ~ Yield * 1000))

paste.drop.NA <- function(x, sep = ", ") {
  x <- gsub("^\\s+|\\s+$", "", x) 
  ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
  is.na(ret) <- ret == ""
  return(ret)
}

d$Trt.combo <- apply(d[,9:15], 1, paste.drop.NA)



d$`Corresponding soil paper`[grepl(d$`Corresponding soil paper`, pattern = "Table|Figure")] <- 
  d$Paper[grepl(d$`Corresponding soil paper`, pattern = "Table|Figure")]


d.trts <- read.csv("d.trts.csv")
str(d.trts)

d %>%
  left_join(d.trts[,c(1,11,12,13)]) %>%
  distinct(.) -> d


## Export as XLSX, deal with unusal papers manually

#write.xlsx(d, file = "data/long_term_yield_data_04242019.xlsx")
