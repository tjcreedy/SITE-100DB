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
library(magrittr)
library(rlang)

# Load functions and defaults -----------------------------------------------------------------

gs4_auth("thomas.creedy@gmail.com")
source("database_funcs.R")



# Initial code for reading in old versions, no longer needed

# coldetails <- getcoldetails(sheets.metaonly)


#map2(sheets.metaonly, coldetails$type, ~read_sheet(master, .x, col_types = .y)) %>% setNames(sheets.metaonly)
# dbtax <- read_sheet(master, "taxonomy", col_types = "c")
# newmeta <- read_sheet("1QCSEU9ceCjc-D9B6czyScjBlgiDwZf4UWsUkWPJhwaE", "metadata", col_types = "c")
# 
# full_taxonomy <- bind_rows(
#   dbtax %>% 
#      select(all_of(taxlevels)),
#   newmeta %>% 
#     select(any_of(taxlevels)),
# ) %>% 
#   unique() %>%
#   arrange(!!!syms(rev(taxlevels)))
# 

# Functions

widetolong <- function(wide){
  wide %>%
    unique() %>%
    arrange(!!!syms(rev(taxlevels))) %>%
    mutate(id = 1:n()) %>%
    pivot_longer(!id, names_to = "child_rank", values_to = "child") %>%
    mutate(child_rank = factor(child_rank, levels = taxlevels)) %>%
    filter(!is.na(child)) %>%
    group_by(id) %>%
    mutate(parent = lead(child, default = "root")) %>%
    ungroup() %>%
    select(!id) %>%
    unique() %>%
    arrange(desc(child_rank), child, parent) %>% 
    select(child, parent, child_rank) %>%
    return()
}


longtowide <- function(long){
  exprows <- long %>% filter(!child %in% parent) %>% nrow
  long <- as.matrix(long %>% select(child, parent, child_rank))
  maketaxstrings <- function(long, prev) {
    unlist(lapply(1:nrow(prev$children), function(i){
      #i = 2
      taxonomy <- c(setNames(prev$children[i, "child"], prev$children[i, "child_rank"]), 
                    prev$taxonomy)
      nxt <- long[long[,"parent"] == prev$children[i, "child"], ,drop = F]
      if(nrow(nxt) == 0){
        return(list(data.frame(as.list(taxonomy))))
      } else {
        return(maketaxstrings(long, list(children = nxt, 
                                         taxonomy = taxonomy)))
      }
    }), recursive = F)
  }
  start <- list(children = long[long[,"parent"] == "root", ],
                taxonomy = c())
  output <- list_rbind(maketaxstrings(long, start)) %>%
    select(all_of(taxlevels)) %>%
    arrange(!!!syms(rev(taxlevels))) %>%
    as_tibble()
  
  if(nrow(output) != exprows){
    warn(message = paste("Warning: number of rows in wide table does not match",
                         "number of childless children in the long table. You","
                         probably have some taxa that can't be tracked up to",
                         "root"))
  }
  
  return(output)
}

listancestors <- function(long, taxon){
  if(taxon == "root"){
    return(c())
  }
  if(!taxon %in% long$child){
    return(paste0(taxon, '[missing]'))
  }
  p <- filter(long, child == taxon) %>% pull(parent)
  if(length(p) > 1){
    return(paste0(taxon, '[multiparent]', collapse = "&"))
  } else {
    return(c(p, listancestors(long, p)))
  }
}

# Read it the long version, run the checks on it and then write it again

longtax <- read_sheet(master, "taxonomylong", col_types = "c")
longtax$child_rank <- factor(longtax$child_rank, levels = taxlevels)

longtax %<>% mutate(issue = NA)

# 1. Is a child present more than once?

longtax %<>%
  arrange(child) %>%
  group_by(child) %>%
  mutate(n = n(),
         nparent = length(unique(parent)),
         nrank = length(unique(child_rank))) %>%
  ungroup() %>%
  mutate(issue = ifelse(nrank > 1, "DIFrank", 
                        ifelse(nparent > 1, "DIFparent",
                               ifelse(n > 1, "DUPchild", issue)))) %>%
  select(-starts_with("n"))
  

# 2. Is the parent rank too much higher than the child rank? 

parentranks <- longtax %>%
  select(-c(parent, issue)) %>%
  unique() %>%
  rename(parent = child, parent_rank = child_rank)

longtax %<>%
  left_join(parentranks, by = "parent") %>%
  mutate(rankdist = as.numeric(parent_rank) - as.numeric(child_rank),
         issue = ifelse(!is.na(issue), issue, 
                        ifelse(child_rank == "morphospecies", issue, 
                               ifelse(rankdist != 1, paste0("GAPrank=",rankdist),
                                      issue)))) %>%
  select(-c(parent_rank, rankdist))

range_clear(master, "taxonomylong", cell_rows(c(2, NA)), reformat = T)
sheet_append(master, longtax, "taxonomylong")

# 3. Are there any problems when tracking up through the tree?

longtax %<>%
  mutate(ancestor = map_vec(child, ~rev(listancestors(longtax, .x))[1]), # This will take a little while
         issue = ifelse(!is.na(issue), issue,
                        ifelse(str_detect("[", ancestor), paste0("ANC", ancestor), 
                               ifelse(ancestor != "root", "ANCnotroot", issue)))) %>%
  select(-ancestor)
  

# Read in the long taxonomy, convert it to wide and overwrite the wide taxonomy on the DB

longtax <- read_sheet(master, "taxonomylong", col_types = "c")
longtax$child_rank <- factor(longtax$child_rank, levels = taxlevels)

length(longtermchild)
wide <- longtowide(longtax)

widechild <- pmap(wide, function(...) na.omit(c(...))[1]) %>% unlist %>% unname
length(widechild)


range_clear(master, "taxonomy", cell_rows(c(2, NA)), reformat = T)
sheet_append(master, , "taxonomy")

# Read in the wide taxonomy, convert it to long and overwrite the long taxonomy on the DB

widetax <- read_sheet(master, "taxonomy", col_types = "c")
range_clear(master, "taxonomylong", cell_rows(c(2, NA)), reformat = T)
sheet_append(master, widetolong(widetax), "taxonomy")
