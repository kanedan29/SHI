source("code/0_libraries.R")


test <- read_excel(path = "data/data_export.xlsx") %>%
  #mutate_at(.vars = 14:21, .funs = function(x) as.character(x)) %>%
  pivot_longer(c(15,17,19, 21)) %>%
  filter(name == "IR") %>%
  dplyr::select(-BD.units, -SOC.units, -PR.units, -Depth) %>%
  rename(Units = IR.units, "Data type" = name, Value = value)

paste.drop.NA <- function(x, sep = ", ") {
  x <- gsub("^\\s+|\\s+$", "", x) 
  ret <- paste(x[!is.na(x) & !(x %in% "")], collapse = sep)
  is.na(ret) <- ret == ""
  return(ret)
}

test2 <- read_excel("data/UCS_SOM_Data_09242020.xlsx") %>%
  mutate(Value = as.numeric(Value))
  
test2$Trt.combo <- apply(test2[,4:8], 1, paste.drop.NA)



test3 <- bind_rows(test, test2) %>%
  distinct(.)

write_excel_csv(test3, path = "data/Data_join_09242020.csv")
