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



# Load current metadata version of taxonomy -------------------------------

coldetails <- getcoldetails(sheets.metaonly)


#map2(sheets.metaonly, coldetails$type, ~read_sheet(master, .x, col_types = .y)) %>% setNames(sheets.metaonly)
dbtax <- read_sheet(master, "taxonomy", col_types = "c")
newmeta <- read_sheet("1QCSEU9ceCjc-D9B6czyScjBlgiDwZf4UWsUkWPJhwaE", "metadata", col_types = "c")

full_taxonomy <- bind_rows(
  dbtax %>% 
     select(all_of(taxlevels)),
  newmeta %>% 
    select(any_of(taxlevels)),
) %>% 
  unique() %>%
  arrange(!!!syms(rev(taxlevels)))

full_taxonomy %<>%
  mutate(class = ifelse(!is.na(order), "Insect", NA),
         across(everything(), ~ na_if(.x, "#N/A")),
         across(everything(), ~ na_if(.x, "NA")),
         across(everything(), ~ na_if(.x, "Larvae")),
         across(everything(), ~ na_if(.x, "Not Coleoptera")),
         across(everything(), ~ na_if(.x, "NA")),
         across(everything(), ~ na_if(.x, "Unknown")))

full_taxonomy %<>%
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
  select(child, parent, child_rank)

# Checks ---------------------------------------------------------------

full_taxonomy <- read_sheet(master, "taxonomylong", col_types = "c")
full_taxonomy$child_rank <- factor(full_taxonomy$child_rank, levels = taxlevels)

full_taxonomy %<>% mutate(issue = NA)

# 1. Is a child present more than once?

full_taxonomy %<>%
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

parentranks <- full_taxonomy %>%
  select(-c(parent, issue)) %>%
  unique() %>%
  rename(parent = child, parent_rank = child_rank)

full_taxonomy %<>%
  left_join(parentranks, by = "parent") %>%
  mutate(rankdist = as.numeric(parent_rank) - as.numeric(child_rank),
         issue = ifelse(!is.na(issue), issue, 
                        ifelse(child_rank == "morphospecies", issue, 
                               ifelse(rankdist != 1, paste0("GAPrank=",rankdist),
                                      issue)))) %>%
  select(-c(parent_rank, rankdist))


# Create a wide version 

full_taxonomy %>%
  filter(parent == "Insect")

long <- as.matrix(full_taxonomy %>% select(child, parent, child_rank))

curtx <- "root"




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

output <- list_rbind(maketaxstrings(long, start))
output %>% as_tibble() %>%
  arrange(!!!syms(rev(taxlevels)))


range_clear(master, "taxonomylong", cell_rows(c(2, NA)), reformat = T)
sheet_append(master, full_taxonomy, "taxonomylong")
