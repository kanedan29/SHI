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
  x <- gsub("^\\s+|\\s+$", "", x)  #what is this string?
  ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
  is.na(ret) <- ret == ""
  return(ret)
}

d$Trt.combo <- apply(d[,9:15], 1, paste.drop.NA)

unique(d$Crop)

# Condense crop list

d <- d %>%
  mutate(Crop = str_replace_all(Crop, pattern = 
                   c("Potato \\(marketable\\)|Potato tuber" = "Potato",
                   "Maize \\(mean yield\\)|Maize \\(grain\\)|Continuous Maize|Rotated Maize|Sole maize|Sole maize \\(no trees\\)" = "Maize",
                   "Vetch cover crop \\(aerial biomass\\)|Vetch \\(aerial\\)" = "Vetch",
                   "Barley cover crop \\(aerial biomass\\)" = "Barley \\(aerial\\)",
                   "Spring barley \\(grain\\)" = "Spring barley",
                   "Rotated soybean|Soybean grain" = "Soybean",
                   "Accumulated maize stover|Maize \\(dry matter\\)|Maize stalks and leaves \\(mean dry matter, 108 days after planting\\)|Maize stover" = "Maize \\(stover\\)",
                   "Maize \\(total plant biomass\\)" = "Maize biomass",
                   "Wheat grain" = "Wheat",
                   "Oats+" = "Oat",
                   "Maize silage dry matter|Maize silage" = "Maize \\(silage\\)",
                   "Barley residue|Barley straw" = "Barley \\(residue\\)",
                   "Wheat straw|Wheat residue" = "Wheat \\(residue\\)",
                   "Faba bean \\(grain\\)" = "Faba bean",
                   "Rape|Oil seed rape|Spring oil seed rape" = "Oilseed rape",
                   " \\(no trees\\)" = "")))

crop.review <- d %>%
  group_by(Crop) %>%
  summarize(number = n())

d$`Corresponding soil paper`[grepl(d$`Corresponding soil paper`, pattern = "Table|Figure")] <- 
  d$Paper[grepl(d$`Corresponding soil paper`, pattern = "Table|Figure")]

## Add new data

d.yield.new <- read.xlsx("data/AgEvidence_Oldfield_selected.xlsx", sheet = "yield")

d.yield.new <- d.yield.new %>%
  filter(Paper != "Campbell et al. 2007")

d.yield.new %>%
  separate(col = "obs.year", into = c("begin.obs", "end.obs"), sep = "-", remove = FALSE) %>%
  mutate(end.obs = if_else(is.na(end.obs), begin.obs, end.obs)) %>%
  mutate(obs.length = as.numeric(end.obs) - as.numeric(begin.obs)) %>%
  filter(obs.length <= 1) -> d.yield.new

d.yield.new %>%
  mutate(Yield.kg.per.hectare = case_when(
    yield.units == "Mg ha-1" | yield.units == "t ha-1" ~ yield*1000 
  )) %>%
  mutate(`Corresponding soil paper` = Paper) -> d.yield.new

names(d)
names(d.yield.new)

d.yield.new %>%
  rename("begin_obs" = "begin.obs",
         "end_obs" = "end.obs",
         "obs_length" = "obs.length",
         "Treatment_1" = "treatment.1",
         "Treatment_2" = "treatment.2",
         "Treatment_3" = "treatment.3",
         "Treatment_4" = "treatment.4",
         "Treatment_5" = "treatment.5",
         "Treatment_6" = "treatment.6",
         "Treatment_7" = "treatment.7",
         "Yield" = "yield",
         "Units" = "yield.units") -> d.yield.new

d.yield.new$obs.year <- as.numeric(d.yield.new$obs.year)
d.yield.new$begin_obs <- as.numeric(d.yield.new$begin_obs)
d.yield.new$end_obs <- as.numeric(d.yield.new$end_obs)

d %>%
  bind_rows(d.yield.new) -> d

d.trts <- read.xlsx("data/d.trts.all.papers.xlsx")
str(d.trts)

d %>%
  left_join(d.trts[,c(1,11,12,21)]) %>%
  distinct(.) -> d

d <- d[,-which(names(d) %in% c('id', 'obs.year', 'obs.type', 'lat', 'lon'))]

## Append climate data


load("data/meanmonthly_climate.RData")
d <- d %>%
  left_join(meanmonthly_climate, by = c("Paper" = "Paper", "Study_name" = "Study_name", "end_obs" = "year"))



d <- d %>%
  select(Paper, DOI, Study_name, Location, AEZ, lat, lon, `Corresponding soil paper`, 
          Year_started, Year_ended, begin_obs, end_obs, obs_length, 
          Treatment_1, Treatment_2, Treatment_3, Treatment_4, Treatment_5, Treatment_6, Treatment_7, Trt.combo,
          tmin, tmax, ppt, pet, SPEI.1.year.min, SPEI.1.year.max, SPEI.1.SD, SPEI.1, SPEI.3, SPEI.6, SPEI.9, SPEI.12,
          Crop, Yield, Units, Yield.kg.per.hectare)

d <- d[d$Yield.kg.per.hectare < 125000,]

d.trt.codes <- read.xlsx("data/d.trts.all.papers.xlsx")
d.trt.codes <- d.trt.codes[, c(1,11,13:20)]

d.yield.nonagg <- d %>%
  left_join(d.trt.codes) 

d.yield.nonagg <- d.yield.nonagg %>%
  filter(Irrigation != "y" | is.na(Irrigation))

save("d", "d.yield.nonagg", file = "data/d.yield.RData")


## Export as XLSX, deal with unusal papers manually

#write.xlsx(d, file = "data/long_term_yield_data_04242019.xlsx")
