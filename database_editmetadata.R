# Setup -------------------------------------------------------------------

rm(list = ls())
invisible(gc())
options(stringsAsFactors = F)

# Load libraries ----------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(readxl)
library(googlesheets4)

# Load functions and defaults -----------------------------------------------------------------

gs4_auth("thomas.creedy@gmail.com")
source("database_funcs.R")

# Load current db -----------------------------------------------------------------------------

coldetails <- getcoldetails(sheets.metaonly)

db <- map2(sheets.metaonly, coldetails$types, ~read_sheet(master, .x, col_types = .y)) %>% setNames(sheets.metaonly)
saveRDS(db, paste0("SITE-100_DB_backup_", str_replace(Sys.time(), " ", "_"), ".RData"))


# Check taxonomy ----------------------------------------------------------



# Push to main database -----------------------------------------------------------------------

# Backup
saveRDS(db, paste0("SITE-100_DB_backup_", str_replace(Sys.time(), " ", "_"), ".RData"))

# Clear data
map(sheets.metaonly, ~range_clear(master, .x, cell_rows(c(2, NA)), reformat = F))

# Push to database
sheet_append(master, db$metadata, sheets.metaonly)





# 
# # Collapse taxonomy -------------------------------------------------------
# 
# db$metadata %<>% 
#   rowwise() %>%
#   mutate(dummy = "NA", 
#          lowest_taxon = c_across(all_of(c(taxlevels[-1], "dummy"))) %>% 
#            na.omit %>% pluck(1),
#          lowest_taxon = na_if(lowest_taxon, "NA")) %>%
#   relocate(lowest_taxon, .before = species) %>%
#   select(-all_of(taxlevels[-1])) %>%
#   select(-dummy)
# 





#   
# db$metadata %>% filter(is.na(type)) %>% print(n = 100)
# metarn$nhmuk_id

# Generate and apply IDs to metadata ----------------------------------------------------------

# db$metadata %>% filter(is.na(nhmuk_id)) %>%
#   group_by(type) %>%
#   summarise(n = n())
# select(nhmuk_id, project_sample_id)
# 

# 
# 
# # Set dataset ---------------------------------------------------------------------------------
# 
# datasets <- list(
#   sarawut          = "1FOk6vracvTjLARmp9kV1z6GH9BPdsdkPuWbBA8lgqY0",
#   ecuador          = "1uO9wU7Tx9E6zfdtOeYDDAJBL_B8RzEOyswaaK7EpUzg",
#   frenchguiana     = "1EWIK3eb3lcA8t-TQTkbl_JgxSNxIyg7oOZGpLcvB_UU",
#   panama           = "1SOwpzdeugGdvF8Opbfd2ADo6cH8xI9wlyuWLt9nyyHc",
#   equatorialguinea = "1UVwC3Oo4Tpw2vvLbFU0JLf8wILmki1g3erkBiSvgHRI",
#   southafrica      = "1UdzAnLQQXVYsMZ3k4KoTF3gF3e84lcI08ZuNL1jflAw",
#   bookham          = "1ThDxZX2Rt_SBKjQWfUt6MsnzRMSeEjeMIQsh0PTnyQc",
#   borneo           = "1-mEnw37PsH8fjVd7DFFATC8VQR3h5lPV1wwKBzGZfKg",
#   ecuadorcarab     = "1tsj1UMtKP8rjplRdDM5KYb7iV0iJbhO1NuKksVxDl5k",
#   mex22b1          = "1JaTyXpwg-4KOta8UewxIC65fp3v9OVFIoSLilWms8O4",
#   oldpanama        = "1pqQXhgAjouLRsWJOwkLPkItwrhX1LeLDWb8FeESuJdQ", 
#   palestine18      = "1sp1YMfBrJtLjxuy-x2WHEwq7xQKJklshk9eyD7AbMGM",
#   mozambique       = "1UNP63992tjGb0gqsBfTRjl1EuGlogQJD1H25aKt_QWY",
#   danumvalley      = "1wQzBKXUuzyNIzrGtHS7ZyhKevQ_g1UcpR5eOLNEH06U",
#   frenchguiana2    = "1_mOGXj_mmsFOYIAFrkzb572r7lG3Gpu16tG7OBeJyUg",
#   panama2          = "1pWYPWIVhULFUFU6K8ah8m49fZAYmkdytsw27dX_bVzA"
# )
# 
# coldetails$types <- coldetails$types %>% str_replace("dddDd", "ccccc")
# 
# querycolumns <- c("longitude", "latitude", "elevation", "collection_date", "sample_time")
# 
# # Load data -----------------------------------------------------------------------------------
# 
# metalist <- map(datasets, function(d){
#   data <- map2(sheet, coldetails$types, ~read_sheet(d, .x, col_types = .y)) %>% setNames(sheet)
#   data$metadata %>%
#     filter(if_any(all_of(querycolumns), ~ !(is.na(.x) | .x == "")))
# })
# data <- list_rbind(metalist)
# 
# # Output data for correction ------------------------------------------------------------------
# 
# # data %>% select(plotid, latitude, longitude) %>% 
# #   unique %>%
# #   mutate(corrected_latitude = "", 
# #          corrected_longitude = "") %>%
# #   write.csv("coordinate_corrections.csv", row.names = F, quote = F)
# # 
# # data %>% select(project_sample_id, collection_date) %>% filter(!is.na(collection_date) | collection_date != "") %>% mutate(corrected_collection_date = "") %>% unique %>%
# #    write.csv("collection_date_corrections.csv", row.names = F, quote = F)
# 
# 
# # Integrate corrected data --------------------------------------------------------------------
# 
# coord_correct <- read.csv("coordinate_corrections.csv")
# date_correct <- read_sheet("1Ik2xda1YhtL2loeneCd1nRv0PwBjenX7epBmGA_Mxzw", "collection_date_corrections", col_types = "ccc") %>%
#   select(-project_sample_id) %>%
#   unique
# 
# dcorrect <- data %>%
#   select(nhmuk_id, project_sample_id, plotid, latitude, longitude, collection_date, elevation) %>%
#   left_join(coord_correct, by = c("plotid", "latitude", "longitude")) %>% 
#   left_join(date_correct, by = "collection_date") %>%
#   select(-c(latitude, longitude, collection_date)) %>%
#   rename_with(~ str_remove(.x, "corrected_"), starts_with("corrected_")) %>%
#   mutate(collection_date = ifelse(collection_date == "", NA, collection_date))
# 
# # Fix elevation -------------------------------------------------------------------------------
# 
# dcorrect <- dcorrect %>% 
#   mutate(elevation = str_remove_all(elevation, "[^0-9.-]") %>% as.numeric) 
# 
# 
# # Bring together ------------------------------------------------------------------------------
# 
# corrected <- db$metadata %>%
#   mutate(collection_date = as.character(collection_date)) %>% 
#   left_join(dcorrect %>% select(-plotid), 
#             by = c("project_sample_id", "nhmuk_id")) %>%
#   mutate(across(ends_with(".y"), function(.y) ifelse(is.na(.y), get(str_replace(cur_column(), ".y", ".x")), .y ))) %>%
#   select(-ends_with(".x")) %>%
#   rename_with(~ str_remove(.x, ".y"), ends_with(".y")) %>%
#   relocate(all_of(names(db$metadata))) %>%
#   mutate(collection_date = as.Date(collection_date))


