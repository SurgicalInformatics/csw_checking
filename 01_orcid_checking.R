##### testing orcid pull #####

# provides the csv file to upload to REDCap registration admin
# to determine whether ORCIDs show a name or not

# ~15 mins to run on ~5,000 orcids

library(tidyverse)
library(rorcid)
library(RCurl)
library(shiny)
library(jsonlite)

fake_orcids = c("0000-0000-0000-0000",
                "1111-1111-1111-1111",
                "1234-1234-1234-1234",
                "1235-1235-1235-1235",
                "2222-2222-2222-2222",
                "2912-2912-2912-2912",
                "9999-9999-9999-9999")

# get ORCID IDs from REDCap -------
orcids_orig = postForm(uri='https://globalsurgery.redcap.bham.ac.uk/api/',
                        token=Sys.getenv("surgweek_teams"),
                        content='record',
                        format='csv',
                        rawOrLabel='label',
                        rawOrLabelHeaders='raw',
                        exportCheckboxLabel='false',
                        returnFormat='csv') %>% 
    read_csv()

# pivot to long format
orcid_long = orcids_orig %>% 
    select(-matches("mobile")) %>% 
    pivot_longer(
        cols = matches("orcid_|email_|firstname_|lastname_"),
        names_to = c(".value", "number"),
        names_pattern = "(.+)_(.+)",
        values_drop_na = TRUE) %>% 
    filter(! orcid  %in% fake_orcids) 

# for ORCID pull
orcid_list = orcid_long %>% 
    select(record_id, orcid) %>% 
    drop_na(orcid) 

redcap_orcids = orcid_long %>% 
    select(record_id, orcid, firstname, lastname, email) %>% 
    drop_na(orcid)


# ORCID pull
map_result = map(orcid_list$orcid, safely(function(x) orcid_id(x)))

# extract names from lists, put into tibbles
firstnames = map(map_result, possibly(
    function(x)
        paste0("", x$result[[1]]$name$`given-names`$value), otherwise = NA))
lastnames  = map(map_result, possibly(
    function(x)
        paste0("", x$result[[1]]$name$`family-name`$value), otherwise = NA))

# populate orcid tables
orcid_list$firstname = firstnames %>% unlist()
orcid_list$lastname  = lastnames %>% unlist()
orcid_list$pull_date = Sys.time()

save(orcid_list, file = "../csw_registration/latest_orcid_pull.rda")
load(file = "../csw_registration/latest_orcid_pull.rda")

# invalid orcids
orcids_blank = orcid_list %>% 
    filter(firstname == "") %>% 
    select(record_id, orcid) %>% 
    left_join(redcap_orcids) %>% 
    distinct() %>%
    group_by(record_id) %>% 
    summarise(invalid_orcids = paste0(orcid, " - ", firstname, " ", lastname, " (", email, ")", collapse = ", ")) %>% 
    mutate(orcids_valid = "0")

# valid orcids
orcids_yes = orcid_list %>% 
    filter(firstname != "") %>% 
    select(record_id) %>% 
    distinct() %>%
    mutate(invalid_orcids = "") %>% 
    mutate(orcids_valid = "1")

# for checking they match
orcids_all = orcid_list %>%
    distinct(record_id, .keep_all = TRUE)

# create tibble for import to REDCap
orcid_check_import = orcids_yes %>% 
    union_all(orcids_blank) %>% 
    group_by(record_id) %>%
    filter(orcids_valid == min(orcids_valid)) %>% 
    arrange(record_id)

# error check
stopifnot(nrow(orcid_check_import) == nrow(orcids_all))


# save as csv for export
write_csv(orcid_check_import, path = paste0("../csw_registration/orcid_pull_data/orcid_check_import_", Sys.Date(), ".csv"))




