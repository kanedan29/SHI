d <- read.csv(file = "data/Dan Kane query Nov 2017.csv")
taxon <- read.csv("data/Dan Kane query Nov 2017_taxonomy.csv", stringsAsFactors = F)
taxon$SSL_taxon_name[taxon$SSL_taxon_name %in% ""] <- NA
taxon$sampledas_taxon_name[taxon$sampledas_taxon_name %in% ""] <- NA

d2 <- merge(d, taxon)

#Permanent wilting point, water retention at 15 bar

data.frame(names(d2))
na_count <- data.frame(sapply(d2, function(y) sum(length(which(is.na(y))))))
d2 %>% mutate(Year = substr(labsampnum, start = 0,stop = 2)) -> d2

d2 %>%
  mutate(PAW = Water.Retention..1.3.Bar...2mm - Water.Retention..15.bar...2mm) %>%
  #filter(!is.na(PAW)) %>%
  filter(!is.na(Total.Sand)) %>%
  filter(!is.na(Total.Silt)) %>%
  filter(!is.na(Total.Clay)) %>%
  filter(!is.na(Total.Carbon)) %>%
  rename(Field_capacity = Water.Retention..1.3.Bar...2mm,
         PWP = Water.Retention..15.bar...2mm) -> d3
  
d3 <- d3[,c(1:9,10,16,18,20,27,35,74)]

d3 %>%
  left_join(taxon[,c(1,5:12)], by = "limspedon_id") -> d3

d3$lat <- as.numeric(d3$latitude.degrees + d3$latitude.minutes/60 + d3$latitude.seconds/3600)
d3$lon <- as.numeric(d3$longitude.degrees + d3$longitude.minutes/60 + d3$longitude.seconds/3600)*-1

d4 <- distinct(d3[,c(1,25,26)])
d4 <- d4[!is.na(d4$lat),]


d4 <- d4[d4$lon > -120,]


##Transform into a spatial points data frame and identify CRS
coordinates(d4) = ~ lon + lat
proj4string(d4) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')


plot(d4)
## Call map unit information from SDA ####

#soilgrids.r <- REST.SoilGrids("TAXOUSDA")
#soilgrids.Taxonomy <- lapply(1:1957, function(x){
 # tryCatch(GSIF::over(soilgrids.r, d4[x,]),error=function(e) NULL)
#})
#soilgrids.Taxonomy <- ldply(soilgrids.Taxonomy)
#soilgrids.Taxo.major <- soilgrids.Taxonomy[,c("TAXGOUSDAMajor","lon","lat")]

load("data/soilgridstaxo.RData")


d4 <- as.data.frame(d4) 
d4 %>%
  left_join(soilgrids.Taxo.major) %>%
  distinct(.) %>%
  right_join(d3) -> d3

orders <- read.csv(file = "data/orders.csv")

d3 %>%
  left_join(orders) -> d3
d3 <- droplevels(d3)
d3$Soil_order <- as.factor(d3$Soil_order)


## Remove all data except for what's in the top 100 cm

d3 <- d3[d3$lay_depth_to_bottom < 100,]
d3 <- d3[d3$Total.Carbon <= 10,]
d3 <- d3[!is.na(d3$limspedon_id),]

