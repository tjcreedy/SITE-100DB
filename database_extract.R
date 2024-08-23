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
saveRDS(db, paste("SITE-100_DB_backup_", str_replace(Sys.time(), " ", "_"), ".RData"))

# Set new output and clear it -----------------------------------------------------------------

outsheet <- "1eErXdFpyDIbDV4G7wRwZ7tseCA6_-AoAskqq29nqU2o"
map(sheets.meta, ~range_clear(outsheet, .x, cell_rows(c(2, NA))))

# Specify extraction --------------------------------------------------------------------------

# Create starter empty output
output <- map(db, ~ slice(.x, 0))

# Create an input list that specifies the starting table type and the subset of rows required
searchstart <- list(
  sheet = "metadata",
  subset = db$metadata %>%
    filter(country == "French Guiana" & type == "principal")
)

# Add these values to the output
output[[searchstart$sheet]] <- searchstart$subset

# Retrieve upstream tables, looping in metadata to get all parents of batches up to principals

# TODO

# Retrieve downstream tables, looping in metadata to get all children of batches down to specimens

i <- which(sheets.meta == searchstart$sheet)
while(!is.na(sheets.meta[i])){
    currows <- nrow(output[[sheets.meta[i]]])
    output[[sheets.meta[i]]] <- pmap(coldetails$idrefdata[[sheets.meta[i]]], function(...){
      row <- tibble(...)
      db[[sheets.meta[i]]] %>%
        filter(!!as.symbol(row$column) %in% na.omit(pull(output[[row$reference_sheet]], row$reference_column)))
    }) %>%
    bind_rows() %>%
    bind_rows(output[[sheets.meta[i]]]) %>%
    unique()
    
    if(nrow(output[[sheets.meta[i]]]) == currows) i <- i + 1
}

# ---------------------------------------------------------------------------------------------

map2(output, names(output), ~ sheet_append(outsheet, .x, .y))
output$DNA %>% names


# Remove from database ------------------------------------------------------------------------
# 
# map(output, nrow)
# 
# # Get indices
# rmi <- map(sheets, ~ map(idkeycols[[.x]], function(.y) pull(db[[.x]], .y) %in% pull(output[[.x]], .y ) & !is.na(pull(db[[.x]], .y))) %>%
#       do.call(cbind, .) %>% rowSums %>% {which(. > 0)})
# 
# getranges <- function(x, shift = F){
#   r <- split(x, cumsum(c(1, diff(x) != 1)))
#   if(shift) {
#     return (mapply(function(x, y) x - y, r, cumsum(c(0, sapply(r, length)[-length(r)]))))
#   } else {
#     return(r)
#   }
# }
# map(rmi, ~ sort(.x) %>% getranges)
# 
# db$DNA %>% filter(project_sample_id == "IAN25-0402-0004")
# db$PCR %>% filter(project_dnaextract_id == "BIBC_MSL1068_D04")
# db$readfile %>% filter(project_uta_id == "BIBC_MSL1068_D04")
