test3 <- test %>%
  dplyr::select(conprac, Trt.combo, Paper, Site) %>%
  distinct() %>%
  right_join(test2) %>%
  distinct(.)


tillage.trt <- d_long %>%
  filter(Con_practice == "Tillage",!is.na(yes_no)) %>%
  dplyr::select("Con_practice",
                "Paper",
                "DOI/ISSN",
                "Year of observation",
                "Trt.combo",
                "Site",
                "yes_no",
                "SOC.g.kg.weighted",
                "IR.cmh") %>%
  mutate(Con_practice = as.character("Reduced tillage"))


cover.trt <- d_long %>%
  filter(Con_practice %in% c("Cover", "Rotation"),!is.na(yes_no)) %>%
  dplyr::select("Con_practice",
                "Paper",
                "DOI/ISSN",
                "Year of observation",
                "Trt.combo",
                "Site",
                "yes_no",
                "SOC.g.kg.weighted",
                "IR.cmh") %>%
  mutate(Con_practice = "Living cover")


org.trt <- d_long %>%
  filter(Con_practice %in% c("Org.amend", "Residue.mgmt"),
         !is.na(yes_no)) %>%
  dplyr::select("Con_practice",
                "Paper",
                "DOI/ISSN",
                "Year of observation",
                "Trt.combo",
                "Site",
                "yes_no",
                "SOC.g.kg.weighted",
                "IR.cmh") %>%
  mutate(Con_practice = "Organic amendments")



all.trt <- bind_rows(tillage.trt, cover.trt, org.trt)
write_csv(all.trt, "all_trt.csv")
