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

# Get current mt_id - genbank mappings ------------------------------------

db_idgb <- db$mitogenomes %>%
  filter(str_detect(mt_id, "^GBDL")) %>%
  select(mt_id, genbank_accession)

# Get mappings from sequence data -----------------------------------------
seq_idgb <- readLines("gbdl_gb_comp.txt")
starts <- which(str_detect(seq_idgb, "^=="))

seq_idgb <- pmap(list(start = starts, end = c(starts[-1]-1, length(seq_idgb))), ~ seq_idgb[..1:..2]) %>% 
  map(function(x){
    c(str_subset(x, "^LOCUS"), str_subset(x, "^ACCESSION")) %>%
      str_remove("^[A-Z]+ +") %>% str_remove(" +.*$")
  }) %>%
  do.call("rbind", .) %>% 
  as_tibble(.name_repair = "minimal") %>%
  setNames(c("mt_id", "seq_genbank_accession"))


seq_idgb %<>%
  arrange(seq_genbank_accession) %>%
  mutate(seq_genbank_accession = ifelse(str_detect(seq_genbank_accession, "^[A-Z]{2}_?[0-9]*$"), seq_genbank_accession, NA))
        

left_join(db_idgb, seq_idgb, by = "mt_id") %>%
  rowwise() %>%
  mutate(contains = str_detect(genbank_accession, seq_genbank_accession)) %>%
  filter(!contains)

left_join(db_idgb, seq_idgb, by = "mt_id") %>%
  filter(mt_id == "GBDL03095")
