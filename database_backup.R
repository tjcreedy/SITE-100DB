# Setup -------------------------------------------------------------------

rm(list = ls())
invisible(gc())
options(stringsAsFactors = F)

# Load libraries ----------------------------------------------------------

library(dplyr)
library(purrr)
library(stringr)

# Load functions ----------------------------------------------------------

source("database_funcs.R")

# Set global variables ----------------------------------------------------


# Do full backup ----------------------------------------------------------

gs4_auth("thomas.creedy@gmail.com")

map2(sheets.all, getcoldetails(sheets.all)$types, ~read_sheet(master, .x, col_types = .y)) %>% 
  setNames(sheets.all) %>%
  saveRDS(paste("SITE-100_DB_backup_", str_replace(Sys.time(), " ", "_"), ".RData"))
