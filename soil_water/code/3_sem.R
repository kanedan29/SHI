
# Import packages and data ------------------------------------------------



source("code/0_libraries.R")
detach("package::plyr")


library(piecewiseSEM)
library(nlme)


d.mod <- read_rds("data/d.mod.rds") %>%
  filter(Paper != "Nyalemegbe et al. 2011") %>% #checked back on this paper, and it's not clear what the units are, so removing
  filter(IR.units != "Time for 500 mL (s)")




# Clean data,  normalize IR to cm/h ---------------------------------------

d <- d.mod %>%
  ungroup(.) %>%
  rename(Site=`Method/Site`) %>%
  mutate(
    IR.cmh = case_when(
      IR.units == "inch/hr" ~ IR*2.54,
      IR.units == "x10^-3 cm/min" ~ (IR/1000)*60,
      IR.units == "x10^-2 cm/min" ~ (IR/100)*60,
      IR.units == "mm/h" ~ IR / 10,
      IR.units == "mm/hr" ~ IR / 10,
      IR.units == "ln(cm/h)" ~ exp(IR),
      IR.units == "cm/d" ~ IR / 24,
      IR.units == "mm per 1.5h" ~ (IR / 1.5) / 10,
      IR.units == "cm/3h" ~ IR / 3,
      IR.units == "mm/s" ~ (IR / 10) * 360,
      IR.units == "cm/min (steady state)" ~ IR * 60,
      TRUE ~ as.numeric(.$IR)
    )
  ) %>%
  group_by(Paper) %>%
  mutate(IR.compress = scale(IR)) %>%
  distinct(.)

# Convert into a long dataframe -------------------------------


d_long <- d %>% 
  pivot_longer(Tillage:Weed.mgmt, names_to = "Con_practice", values_to = "yes_no") %>%
  mutate(Paper_site = case_when(is.na(Site) ~ Paper, 
                                !is.na(Site) ~ paste(Paper, Site, sep = ", ")))


# SEM ----------------------------------------------------------

# org

d.sem <- d_long %>%
  filter(
    Con_practice %in%  c("Org.amend","Residue.mgmt"), 
    #Con_practice %in%  c("Cover","Rotation"), 
    #Con_practice %in%  c("Tillage"), 
         !is.na(yes_no), 
         !is.na(SOC.g.kg.weighted), 
         !is.na(IR),
    #Paper != "Moebius-Clune et al. 2008",
    #Paper != "Lal et al. 1978"
    #Paper != "Abdollahi and Munkholm 2014"
    ) %>%
  mutate(conprac = case_when(yes_no == "y" ~ 1, 
                             TRUE ~ 0),
         IR = IR.cmh) %>%
  rename("Org" = conprac,
         "SOC" = SOC.g.kg.weighted)

test2 <- psem(
  lme(SOC ~ Org,
      random = ~1|Paper_site, na.action = "na.omit", data = d.sem, weights = "Length_of_experiment"),
  lme(IR ~ Org+SOC,
      random = ~1|Paper_site, na.action = "na.omit", data = d.sem, weights = "Length_of_experiment")
)

summary(test2)

org = plot(
  test2,
  node_attrs = list(
    fillcolor = "white",
    shape = "rectangle",
    color = "gray",
    x = c(3, 4.5, 1.5),
    y = c(1.75, 1, 1),
    fontsize = 8,
    fixedsize = FALSE
  ),
  alpha = 0.05, 
  add_edge_label_spaces = FALSE
)


# cover ---------------------------------------------------------------------



# Swap practice set here

d.sem <- d_long %>%
  filter(
    #Con_practice %in%  c("Org.amend","Residue.mgmt"), 
    Con_practice %in%  c("Cover","Rotation"), 
    #Con_practice %in%  c("Tillage"), 
    !is.na(yes_no), 
    !is.na(SOC.g.kg.weighted), 
    !is.na(IR),
    #Paper != "Moebius-Clune et al. 2008",
    #Paper != "Lal et al. 1978"
    #Paper != "Abdollahi and Munkholm 2014"
  ) %>%
  mutate(conprac = case_when(yes_no == "y" ~ 1, 
                             TRUE ~ 0),
         IR = IR.cmh) %>%
  rename("Cover" = conprac,
         "SOC" = SOC.g.kg.weighted)

test2 <- psem(
  lme(SOC ~ Cover,
      random = ~1|Paper_site, na.action = "na.omit", data = d.sem, weights = "Length_of_experiment"),
  lme(IR ~ SOC + Cover,
      random = ~1|Paper_site, na.action = "na.omit", data = d.sem, weights = "Length_of_experiment")
)

summary(test2)

cover = plot(
  test2,
  node_attrs = list(
    fillcolor = "white",
    shape = "rectangle",
    color = "gray",
    x = c(3, 4.5, 1.5),
    y = c(1.75, 1, 1),
    fontsize = 8,
    fixedsize = FALSE
  ),
  alpha = 0.05, 
  add_edge_label_spaces = TRUE,
  digits = 2
)


# till --------------------------------------------------------------------



# Swap practice set here

d.sem <- d_long %>%
  filter(
    #Con_practice %in%  c("Org.amend","Residue.mgmt"), 
    #Con_practice %in%  c("Cover","Rotation"), 
    Con_practice %in%  c("Tillage"), 
    !is.na(yes_no), 
    !is.na(SOC.g.kg.weighted), 
    !is.na(IR),
    #Paper != "Moebius-Clune et al. 2008",
    #Paper != "Lal et al. 1978"
    #Paper != "Abdollahi and Munkholm 2014"
  ) %>%
  mutate(conprac = case_when(yes_no == "y" ~ 1, 
                             TRUE ~ 0),
         IR = IR.cmh) %>%
  rename("Till" = conprac,
         "SOC" = SOC.g.kg.weighted)


test2 <- psem(
  lme(SOC ~ Till,
      random = ~1|Paper_site, na.action = "na.omit", data = d.sem, weights = "Length_of_experiment"),
  lme(IR ~ SOC + Till,
      random = ~1|Paper_site, na.action = "na.omit", data = d.sem, weights = "Length_of_experiment")
)

summary(test2)

till = plot(
  test2,
  node_attrs = list(
    fillcolor = "white",
    shape = "rectangle",
    color = "gray",
    x = c(3, 4.5, 1.5),
    y = c(1.75, 1, 1),
    fontsize = 8,
    fixedsize = FALSE
  ),
  alpha = 0.05, 
  add_edge_label_spaces = TRUE,
  digits = 2
)


till
