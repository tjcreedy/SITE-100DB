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
source("~/repos/SITE-100DB/database_funcs.R")

# Load current db -----------------------------------------------------------------------------

coldetails <- getcoldetails(sheets.asvbait)

db <- map2(sheets.all, 
           getcoldetails(sheets.all)$types, 
           ~ read_sheet(master, .x, col_types = .y)) 


# Read new data and clean ---------------------------------------------------------------------

ASVs <- read_sheet("1QCSEU9ceCjc-D9B6czyScjBlgiDwZf4UWsUkWPJhwaE", "ASV_table")
baiting <- read_sheet("1QCSEU9ceCjc-D9B6czyScjBlgiDwZf4UWsUkWPJhwaE", "mitogenomes")

# ASVsfull <- read_excel("ASV_matching/SITE-100_Database_Prep_2024_Online_2024-05-15.xlsx", 
#                    sheet = "1_ASV_selection")
# 
# baiting <- read_excel("ASV_matching/SITE-100_Database_Prep_2024_Online_2024-05-15.xlsx", 
#                       sheet = "2_MMG_ASV_matching")


# Extract ASVs and baiting
new <- list(
  ASV = ASVs %>% select(project_readfile_id, asv_id, autopropose, project_sample_id) %>%
    mutate(readfile_id = NA) %>%
    relocate(readfile_id) %>%
    rename(match = autopropose),
  baiting = baiting %>% select(asv_id, mt_id) %>%
    filter(asv_id %in% ASVs$asv_id) %>%
    unique
)

# Extract taxonomy renames
taxrename <- ASVs %>% 
  #filter(autopropose == "select") %>% 
  filter(!is.na(image_id)) %>%
  select(project_sample_id, subfamily, family,  autopropose) %>%
  rename(match = autopropose)


# Overwrite existing ASV and baiting data with these tables -----------------------------------

data <- db
data$ASV <- new$ASV
data$baiting <- new$baiting


# Run checks on combined data -----------------------------------------------------------------

# 1. Consistent set of selections?
taxrename %>% pull(match) %>% table


# 2. Readfiles with multiple ASV selections that do not have different project_sample_ids
data$ASV %>% filter(match == "select") %>% 
  group_by(project_readfile_id, project_sample_id) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(project_readfile_id) %>%
  print_writeifrows("ASV_matching/readfile_multiple_ASVselect.csv")

# 3. Mitogenomes matching multiple ASVs
data$baiting %>%  unique %>% 
  mutate(mt_id_dup = duplicated2(mt_id)) %>%
  filter(mt_id_dup) %>%
  arrange(mt_id) %>%
  print_writeifrows("ASV_matching/mt_id_multiple_ASVconfirm.csv")

# 4. Readfiles in readfile sheet
data$ASV %>% mutate(project_readfile_id_present = project_readfile_id %in% data$readfile$project_readfile_id) %>%
  filter(!project_readfile_id_present) %>%
  arrange(project_readfile_id) %>%
  print_writeifrows("ASV_matching/readfile_id_notin_readfiles.csv")

# 5. mt_ids in metadata sheet
data$baiting %>% mutate(mt_id_present = mt_id %in% data$mitogenomes$mt_id) %>%
  filter(!mt_id_present) %>%
  arrange(mt_id) %>%
  print_writeifrows("ASV_matching/mt_id_notin_mitogenomes.csv")

# 6. inconsistent project_sample_id taxonomies
taxrename %>% 
  select(-match) %>%
  arrange(project_sample_id) %>%
  unique %>%
  #filter(image_id != "0") %>%
  group_by(project_sample_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  select(-n) %>%
  print_writeifrows("ASV_matching/project_sample_id_taxonomy_inconsistent.csv")

# 7. project_sample_ids in metadata
all(taxrename$project_sample_id %in% data$metadata$project_sample_id)

# image_id_match %>% filter(is.na(match)) %>%
#   arrange(image_id) %>%
#   unique() %>%
#   print_writeifrows("ASV_matching/image_id_notin_metadata.csv")
# image_id_match %>% print_writeifrows("ASV_matching/image_id_matching_full.csv")

# 8. asv_id with multiple selections
data$ASV %>%
  filter(match == "select") %>%
  arrange(asv_id) %>%
  group_by(asv_id) %>%
  mutate(n = n(), nuniquepsi = length(unique(project_sample_id))) %>%
  filter(n > 1) %>%
  filter(nuniquepsi > 1) %>%
  left_join(data$metadata %>% select(project_sample_id, morphospecies), by = "project_sample_id") %>%
  mutate(nuniquems = length(unique(na.omit(morphospecies)))) %>%
  filter(nuniquems != 1) %>%
  print_writeifrows("ASV_matching/asv_id_matches_multiple_project_sample_ids.csv")

# 9. image_ids or project_sample_ids map to same place as project_readfile_id

data$metadata %>% filter(project_sample_id == parent_project_sample_id)

data$ASV %>% filter(match == "select") %>%
  left_join(data$readfile %>% select(-c(run_id, notes)), by = c("readfile_id", "project_readfile_id")) %>%
  left_join(data$PCR %>% select(-starts_with("tag_"), -notes), by = c("uta_id", "project_uta_id")) %>%
  left_join(data$DNA %>% select(ends_with("dnaextract_id"), nhmuk_id, project_sample_id), by = c("dnaextract_id", "project_dnaextract_id")) %>%
  filter(project_sample_id.x != project_sample_id.y) %>% 
  left_join(data$metadata %>% select(project_sample_id, merged_to, type), by = c("project_sample_id.y" = "merged_to"), relationship = "many-to-many") %>% 
  group_by(asv_id) %>%
  group_map(function(.x, .y){ 
    filt <- .x %>% filter(project_sample_id.x == project_sample_id)
    if ( nrow(filt) > 0 ){
      filt$outcome <- "match"
      return(filt)
    } else {
      .x$outcome <- "nomatch"
      return(.x)
    }
    }, .keep = T) #%>%
  list_rbind()
  
