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

# Load functions ----------------------------------------------------------

source("database_funcs.R")
gs4_auth("thomas.creedy@gmail.com")


# Pull existing mitogenome metadata --------------------------------------------------

coldetails <- getcoldetails(sheets.mt)

db <- map2(sheets.mt, coldetails$types, ~read_sheet(master, .x, col_types = .y)) %>% setNames(sheets.mt)


# Do editing ----------------------------------------------------------------------------------

source <- "~/work/iBioGen_postdoc/MMGdatabase/"

sequences <- list.files(paste0(source, "gbmaster_2024-11-15/")) %>% str_remove(".gb")

newmeta <- bind_rows(
  read_excel(paste0(source, "newdata_2024-11-13/SITE-100 Metadata_mitogenome_241114_BZ_XmVuQd.xlsx"), sheet = 1),
  read_excel(paste0(source, "newdata_2024-11-13/SITE-100 Metadata_mitogenome_240122_QINL_1UE5Oq.xlsx"), sheet = 1)
) %>%
  rename(mt_id = db_id)

rn <- newmeta %>%
  filter(str_detect(library, "^BZ_")) %>%
  mutate(new_mt_id = paste0("ZIPC", sprintf("%03d", 1:n()))) %>%
  select(contains("mt_id"))
write.table(rn, paste0(source, "QINL-ZIPC_conv.txt"), row.names = F, quote = F, col.names = F)

newmeta %<>%
  left_join(rn, by = "mt_id") %>%
  mutate(mt_id = ifelse(is.na(new_mt_id), mt_id, new_mt_id)) %>%
  select(-new_mt_id)

outmeta <- bind_rows(db$mitogenomes, newmeta) %>%
  arrange(mt_id)

range_clear(master, "mitogenomes", cell_rows(c(2, NA)), reformat = T)
sheet_append(master, outmeta, "mitogenomes")

# SCRATCH -------------------------------------------------------------------------------------

# Move anything in Byrrhoidea that is not Byrrhidae to Dryopoidea
outmd <- inmd %>% 
  mutate(edit = superfamily == "Byrrhoidea" & (is.na(family) | family != "Byrrhidae")) %>% 
  mutate(superfamily = ifelse(edit, "Dryopoidea", superfamily),
         ncbi_may_edit = ifelse(edit, F, ncbi_may_edit)) %>%
  select(-edit)

# Do a metadata update

sequences <- list.files("gbmaster_2023-08-01/") %>% str_remove(".gb")

meta0706 <- read_excel("newdata_2023-07-31/SITE-100 Metadata_mitogenome_0706.xlsx")
meta0717 <- read_excel("newdata_2023-07-31/SITE-100 Metadata_mitogenome_0717.xlsx")

delete0801 <- meta0717 %>% filter(str_detect(chimera_notes, "Pre-merged")) %>% pull(db_id)
delete0731 <- meta0717 %>% filter(str_detect(chimera_notes, "(^merged)")) %>% pull(db_id)

meta07 <- bind_rows(
  meta0706 %>% filter(! db_id %in% meta0717$db_id),
  meta0717
)

biod3163plus <- inmd %>% mutate(n = str_remove(db_id, "^[A-Z]*") %>% as.numeric) %>%
  filter(str_detect(db_id, "BIOD") & n >=3163 )

missingbiod <- biod3163plus %>% filter(!db_id %in% meta07$db_id) %>% pull(db_id)
missingbiod %in% sequences
delete0801 <- c(delete0801, missingbiod)
delete0731 <- c(delete0731, missingbiod)

retainmd <- inmd %>% filter(!str_detect(db_id, "BIOD") | (str_detect(db_id, "BIOD") & as.numeric(str_remove(db_id, "^[A-Z]*")) < 3163))

missingmeta <- sequences[!sequences %in% readLines("deletefrom0801.txt")] %>%
  {.[! . %in% retainmd$db_id]} %>%
  {.[! . %in% meta07$db_id]}

missingmeta <- missingmeta[2:10]
delete0801 <- c(delete0801, missingmeta)
delete0731 <- c(delete0731, missingmeta)

writeLines(delete0801, "deletefrom0801.txt")
writeLines(delete0731, "deletefrom0731.txt")

sequences <- sequences[!sequences %in% delete0801]

outmd <- meta07 %>% filter(db_id %in% sequences) %>%
  bind_rows(retainmd) %>%
  arrange(db_id)

push(outmd)
outmd %>% pull(chimera_notes) %>% unique

md <- inmd %>% mutate(ncbi_may_edit = ifelse(superfamily == "Dryopoidea", F, ncbi_may_edit))
#outmd <- md %>% select(-edit) %>% arrange(db_id)
outmd <- md
# Push
push(outmd)
