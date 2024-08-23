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

db$DNA %>% filter(is.na(dnaextract_id))

# Set dataset ---------------------------------------------------------------------------------

db$metadata %>%
  filter(type == "specimen") %>% nrow()

db$metadata %>%
  filter(type == "batch") %>%
  group_by(parent_project_sample_id) %>%
  summarise(n = n()) %>% summary()
  ggplot(aes(x = n)) + 
  geom_histogram()
