# Load cross-validation description files from FHWA

library(data.table)

source("dev/find_project_root.R")

sp_root = find_project_root("HPMS")
fhwa_dir = file.path(sp_root, "2023_update", "from_FHWA")

files = Sys.glob(file.path(fhwa_dir, "Spatial*.csv"))
data =  lapply(files, fread) 
names(data) = basename(files)

names(data[[1]])

names(data[[1]]) = c(
  "rule", "validation_type", "validation_session", "data_item", "data_item_name",
  "message", "update_date", "notes", "update_for_hpms9", "hpms9_change"
)

names(data[[2]])

names(data[[2]]) = c(
  "rule", "validation_session", "validation_type", "data_item", "data_item_name",
  "message", "update_date"
)

names(data[[3]])

names(data[[3]]) = c(
  "rule", "validation_type", "validation_session", "data_item", "data_item_name",
  "message", "update_date"
)

names(data[[4]])

names(data[[4]]) = c(
  "rule", "validation_session", "validation_type", "data_item", "data_item_name",
  "message", "update_date"
)

dt = rbindlist(data, use.names = TRUE, fill = TRUE, idcol = "source")

fwrite(dt, file.path(fhwa_dir, "cross_validation_2022.csv"))
