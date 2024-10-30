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

coldetails <- getcoldetails(sheets.meta)

db <- map2(sheets.meta, coldetails$types, ~read_sheet(master, .x, col_types = .y)) %>% setNames(sheets.meta)

# Set dataset ---------------------------------------------------------------------------------

newdatasets <- list(
  #sarawut          = "1FOk6vracvTjLARmp9kV1z6GH9BPdsdkPuWbBA8lgqY0",
  #ecuador          = "1uO9wU7Tx9E6zfdtOeYDDAJBL_B8RzEOyswaaK7EpUzg",
  #frenchguiana     = "1EWIK3eb3lcA8t-TQTkbl_JgxSNxIyg7oOZGpLcvB_UU",
  #panama           = "1SOwpzdeugGdvF8Opbfd2ADo6cH8xI9wlyuWLt9nyyHc",
  #equatorialguinea = "1UVwC3Oo4Tpw2vvLbFU0JLf8wILmki1g3erkBiSvgHRI",
  #southafrica      = "1UdzAnLQQXVYsMZ3k4KoTF3gF3e84lcI08ZuNL1jflAw",
  #bookham          = "1ThDxZX2Rt_SBKjQWfUt6MsnzRMSeEjeMIQsh0PTnyQc",
  #borneo           = "1-mEnw37PsH8fjVd7DFFATC8VQR3h5lPV1wwKBzGZfKg",
  #ecuadorcarab     = "1tsj1UMtKP8rjplRdDM5KYb7iV0iJbhO1NuKksVxDl5k",
  #mex22b1          = "1JaTyXpwg-4KOta8UewxIC65fp3v9OVFIoSLilWms8O4"
  #oldpanama        = "1pqQXhgAjouLRsWJOwkLPkItwrhX1LeLDWb8FeESuJdQ", 
  #palestine18      = "1sp1YMfBrJtLjxuy-x2WHEwq7xQKJklshk9eyD7AbMGM"
  #mozambique       = "1UNP63992tjGb0gqsBfTRjl1EuGlogQJD1H25aKt_QWY"
  #danumvalley      = "1wQzBKXUuzyNIzrGtHS7ZyhKevQ_g1UcpR5eOLNEH06U",
  #frenchguiana2    = "1_mOGXj_mmsFOYIAFrkzb572r7lG3Gpu16tG7OBeJyUg"
  #panama2          = "1pWYPWIVhULFUFU6K8ah8m49fZAYmkdytsw27dX_bVzA"
  qinling           = "1nV9G7W3nxFZWXlTFbWb4LtyDcmfEFafnSnf3jvwwnRk"
)

names(newdatasets)
ss <-  newdatasets$qinling

# Run checks on new data ---------------------------------------------------------------------

# 1. Does it read and have the right number of columns
new <- map2(sheets.meta, coldetails$types, ~read_sheet(ss, .x, col_types = .y)) %>% setNames(sheets.meta)

# 2. Check column names match expected
map2(new, coldetails$names, ~ names(.x)[names(.x) != .y])

# 3. Check all id-keys are unique (except for ASVs) including against existing data
data <- map2(db, new, ~ bind_rows(mutate(.x, source = "db"), mutate(.y, source = "new")))
map2(data[1:4], coldetails$idkeycols, ~ mutate(.x, across(all_of(.y), duplicated2, .names = "{.col}_dup")) %>%
       select(source, all_of(.y), ends_with("_dup")) %>%
       filter(if_any(ends_with("_dup"))) %>%
       arrange(!!sym(.y[1]), !!sym(.y[2]))
)

# 4. Check that there is at least one id-key for each row and that all ids are alphanumeric
(err <- map2(new[1:4], coldetails$idkeycols, ~ 
               select(.x, all_of(.y)) %>% 
               mutate(across(everything(), function(x) isalphanum(x) | is.na(x), .names = "{.col}_anum")) %>%
               unite("idkey", all_of(.y), remove = F) %>% 
               filter(!if_all(ends_with("_anum")) | idkey == "NA_NA") %>% 
               select(-idkey)))

write.csv(err$metadata, "metadata_errors.csv", row.names = F, quote = F)

# 5. Check that parent ids exist
(err <- map2(new, coldetails$idrefdata,  ~ select(.x, all_of(.y$column)) %>% unique() %>%
       mutate(across(all_of(.y$column), function(x) checkparentpresent(x, data, .y, cur_column()), .names = "{.col}_pres")) %>%
       filter(!if_all(ends_with("_pres"))) %>%
       ungroup() %>%
       unique()))

write.csv(err$DNA, "metadata_errors.csv", row.names = F, quote = F)


# Push to main database -----------------------------------------------------------------------

# Backup
saveRDS(db, paste0("SITE-100_DB_backup_", str_replace(Sys.time(), " ", "_"), ".RData"))

# Push to database
map2(new, names(new), ~ sheet_append(master, .x, .y))















# # 6. Check that sampling data are reasonable
# metasum <- data$metadata %>% filter(type == "principal") %>%
#   select(sampleid, plotid, precise_location, longitude, latitude, elevation, collection_date) %>%
#   group_by(pick(everything())) %>%
#   summarise(count = n()) %>%
#   ungroup()
# 
# metasum %>% print(n = 1000)
# 
# metasum %>% 
#   mutate(across(everything(), ~ if(all(is.na(.x))) {"ANA"} else {.x})) %>% 
#   filter(if_any(everything(), is.na) | count > 1)
# 
# metasum$collection_date %>% range(na.rm = T)
# 
