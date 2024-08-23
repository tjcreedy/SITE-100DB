# Load libraries ----------------------------------------------------------

library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(readxl)
library(googlesheets4)





# Load functions ----------------------------------------------------------

selfname <- function(x) setNames(x, x)
isunique <- function(x) x[!x == "" & !is.na(x)] %>% {length(unique(.)) == length(.)}
duplicated2 <- function(x) ( duplicated(x) | duplicated(x, fromLast = T) ) & !is.na(x) & x != ""
isalphanum <- function(x) str_detect(x, "^[A-Za-z][A-Za-z0-9_-]+$")
checkparentpresent <- function(x, data, refdata, col) {
  rd <- refdata %>% filter(column == col)
  (x %in% pull(data[[rd$reference_sheet]], rd$reference_column) )| is.na(x)
}
print_writeifrows <- function(x, file = "", n = 20, row.names = F, quote = F, ...){
  print(x, n = n)
  if(nrow(x) > 0){
    write.csv(x, file = file, row.names = row.names, quote = quote, ...)
  }
}

getcoldetails <- function(sheets){
  cols <- map(paste0(sheets, "_col"), ~read_sheet(master, .x, col_types = "c")) %>% setNames(sheets)
  list(cols = cols, 
       types = map(cols, ~pull(.x, gs4_code) %>% paste0(collapse = "")) %>% unlist,
       names = map(cols, ~pull(.x, column)),
       idkeycols = map(cols, ~ filter(.x, datatype == "id-key") %>% pull(column)),
       idrefdata = map(cols, ~ filter(.x, datatype == "id-reference") %>% 
                         select(column, starts_with("reference")) %>%
                         filter(!is.na(reference_sheet) & reference_sheet != "[external]"))) %>%
    return()
}

generate_renamer <- function(){
  rn <- read_sheet(master, "mtidrename") %>% select(ends_with("mt_id"))
  
  while(rn %>% filter(curr_mt_id %in% prev_mt_id) %>% nrow > 0){
    rn <- left_join(rn, rn, by = c("curr_mt_id" = "prev_mt_id")) %>%
      filter(!curr_mt_id %in% prev_mt_id) %>%
      mutate(curr_mt_id = ifelse(is.na(curr_mt_id.y), curr_mt_id, curr_mt_id.y)) %>%
      select(-curr_mt_id.y)
  }
  
  rn <- setNames(rn$curr_mt_id, rn$prev_mt_id)
  
}

# Load global values --------------------------------------------------------------------------

sheets.meta <- c("metadata", "DNA", "PCR", "readfile") %>% selfname()
sheets.asvbait <- c("ASV", "baiting") %>% selfname()
sheets.mt <- c("mitogenomes") %>% selfname()
sheets.all <- c(sheets.meta, sheets.asvbait, sheets.mt)

master <- "15zzniRh36X_5nq-wroOaPldPTTzRVwiytqdgksBZmKc"


