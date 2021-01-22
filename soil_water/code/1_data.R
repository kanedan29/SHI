source("code/0_libraries.R")

d <- read_csv("data/Data_join_09242020_soc_only.csv")

## Filter and clean SOC data


## Liebig et al. paper has OC values in Mg/ha, need to figure out how to convert
## Nyamadzawo et al. 2003 does not specify depth and unclear if C numbers are from sediments or soils
d.soc <- d %>%
  filter(Paper != "Nyamadzawo et al. 2003", 
         grepl("OC|OM", `Data type`),
         Units != "Mg/ha",
         Units != "mg/ha") %>%
  mutate(Value =
           case_when(Units %in% "%" ~ Value * 10,
                     TRUE ~ .$Value)) %>%
  mutate(Value = case_when(`Data type` %in% "OM" ~ Value * 0.58, TRUE ~ .$Value), 
         Length_of_experiment = `Year of observation`- `Year experiment start`) %>%
  rename(SOC.g.kg = Value) %>%
  dplyr::select(-starts_with("Treatment"), -Units, -`Data type`) %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(`Year of observation`== max(as.numeric(`Year of observation`))) %>%
  separate(Depth, into = c("Top.depth", "Bottom.depth"), sep = "_", remove = F) %>%
  mutate(Depth.increment=as.numeric(Bottom.depth) - as.numeric(Top.depth)) %>%
  do(mutate(., max.bottom = as.numeric(max(as.numeric((Bottom.depth)))))) %>%
  mutate(Depth.proportion =as.numeric(Depth.increment)/max.bottom) %>%
  ungroup(.) %>%
  group_by(Paper, Trt.combo, `Method/Site`, Depth) %>%
  mutate(SOC.g.kg.weighted = Depth.proportion*SOC.g.kg) %>%
  group_by(Paper, Trt.combo, `Method/Site`) %>%
  dplyr::summarise(SOC.g.kg.weighted =  mean(SOC.g.kg.weighted))
hist(d.soc$SOC.g.kg.weighted)




## Filter and clean IR data

unique(d.raw$`Data type`)

d.ir <- d %>%
  filter(Paper != "Nyamadzawo et al. 2003",
         Paper != "Liebig et al. 2004",
         Paper != "Steele et al. 2012") %>%
  filter(grepl("IR", `Data type`)) %>%
  dplyr::select(-starts_with("Treatment"), -`Data type`, -Depth) %>%
  dplyr::group_by(Paper, `Method/Site`) %>%
  filter(`Year of observation`== max(as.numeric(`Year of observation`))) %>%
  rename(IR.units = Units, IR = Value) %>%
  mutate(Length_of_experiment = `Year of observation`- `Year experiment start`)

# 

d.trts <- read_csv("data/d.trts.csv")


d.mod <- d.ir %>%
  left_join(d.soc) %>%
  left_join(d.trts)



write_rds(d.mod, file = "data/d.mod.rds")

