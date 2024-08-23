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

coldetails <- getcoldetails(sheets.mt)

db <- map2(sheets.mt, coldetails$types, ~read_sheet(master, .x, col_types = .y)) %>% setNames(sheets.mt)
saveRDS(db, paste0("SITE-100_DB_backup_", str_replace(Sys.time(), " ", "_"), ".RData"))

# Set new output and clear it -----------------------------------------------------------------

# outsheet <- "1eErXdFpyDIbDV4G7wRwZ7tseCA6_-AoAskqq29nqU2o"
# map(sheets.meta, ~range_clear(outsheet, .x, cell_rows(c(2, NA))))


# Read in metadata ----------------------------------------------------------------------

newdata <- read.table("newdata_2024-04-20/renaming_linked_results.txt", header = F, sep = "\t")

todelete <- newdata %>%
  pull(V2) %>%
  str_split(", *") %>%
  unlist()

#newrows <- data.frame(mt_id = newdata$V1, order = "Coleoptera", class = "Insecta")

# Remove data
db$mitogenomes <- db$mitogenomes %>%
    filter(!mt_id %in% todelete)

# Add data
#db$mitogenomes <- bind_rows(db$mitogenomes, newrows)

# Remove from database ------------------------------------------------------------------------
# DANGER DANGER DANGER
map(sheets.mt, ~range_clear(master, .x, cell_rows(c(2, NA)), reformat = F))

# Update database -----------------------------------------------------------------------------
map(sheets.mt, ~sheet_append(master, db[[.x]], .x))


