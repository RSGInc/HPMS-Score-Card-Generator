# Load cross-validation description files from FHWA

library(data.table)

source("dev/find_project_root.R")

sp_root = find_project_root("HPMS Scorecards")
fhwa_dir = file.path(sp_root, "2023_update", "from_FHWA")

files = Sys.glob(file.path(fhwa_dir, "Spatial*.csv"))
data =  lapply(files, fread) 
names(data) = basename(files)

names(data[["Spatial Join Full.csv"]])

names(data[["Spatial Join Full.csv"]]) = c(
  "rule", "validation_type", "validation_session", "dataitem", "dataitem_name",
  "message", "update_date", "notes", "update_for_hpms9", "hpms9_change"
)

names(data[["Spatial Join Inventory.csv"]])

names(data[["Spatial Join Inventory.csv"]]) = c(
  "rule", "validation_session", "validation_type", "dataitem", "dataitem_name",
  "message", "update_date"
)

names(data[["Spatial Join PM2.csv"]])

names(data[["Spatial Join PM2.csv"]]) = c(
  "rule", "validation_type", "validation_session", "dataitem", "dataitem_name",
  "message", "update_date"
)

names(data[["Spatial Join Traffic.csv"]])

names(data[["Spatial Join Traffic.csv"]]) = c(
  "rule", "validation_session", "validation_type", "dataitem", "dataitem_name",
  "message", "update_date"
)

dt = rbindlist(data, use.names = TRUE, fill = TRUE, idcol = "source")
dt[, .N, .(source, validation_session)]
dt[, .N, .()]

setkey(dt, validation_type, dataitem_name)

fwrite(dt, file.path(fhwa_dir, "validations_2022.csv"))
