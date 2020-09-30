library(tidyverse)
library(scales)
library(RCurl)
periods_orig = postForm(uri='https://globalsurgery.redcap.bham.ac.uk/api/',
                        token=Sys.getenv("surgweek_teams"),
                        content='record',
                        format='csv',
                        rawOrLabel='label',
                        rawOrLabelHeaders='raw',
                        exportCheckboxLabel='false',
                        returnFormat='csv') %>% 
    read_csv()



##### TIDY #####

# make long
collab_long = periods_orig %>%
    pivot_longer(
        cols = matches("orcid_|email_"),
        names_to = c(".value", "number"),
        names_pattern = "(.+)_(.+)",
        values_drop_na = TRUE)


# fewer columns for checking
collab_long_check = collab_long %>% 
    select(-duplicated:-admin_complete) 
# collapse variables for checking
collab_collapsed = collab_long_check %>% 
    mutate_at(vars(contains(c("specialty", "period__"))), ~case_when(. == "Unchecked" ~ 0,
                                                      . == "Checked" ~ 1,
                                                      TRUE ~ NA_real_)) %>% 
    unite(specialities, starts_with("specialty__"), remove = T, sep = "") %>% 
    unite(periods, starts_with("period__"), remove = T, sep = "")


##### MAIN CHECKS #####

# check for duplicate applications (same specialties within same hospital and period)
## this looks at exact duplicates - there will also be other duplicate specialties or periods
special_dupl = collab_collapsed %>% 
    count(hospital, specialities, periods, orcid, email) %>% 
    arrange(-n, orcid)

# missing ORCID
missing_orcid = collab_long %>% 
    filter(is.na(orcid)) %>% 
    select(hospital, email) %>% 
    distinct()






##### OTHER CHECKS #####

# number of attempted team registrations per hospital including NA team members
all_hosp_reg = periods_orig %>%
    group_by(hospital) %>% 
    count() %>% 
    arrange(-n) %>% 
    ungroup() %>% 
    mutate(total = sum(n))

# number of team registrations per hospital where at least one team member exists
complete_hosp_reg = periods_orig %>%
    filter_at(vars(orcid_1:email_5),any_vars(!is.na(.))) %>% 
    group_by(hospital) %>% 
    count() %>% 
    arrange(-n) %>% 
    ungroup() %>% 
    mutate(total = sum(n))

# check for duplicate:
## orcid across any hospital
orcid_dupl_1 = collab_collapsed %>% 
    group_by(orcid) %>% 
    count() %>% 
    arrange(-n)
## orcid within same hospital
orcid_dupl_2a = collab_collapsed %>% 
    # distinct(hospital, orcid, .keep_all = TRUE) %>% 
    add_count(hospital, orcid) %>% 
    arrange(-n, orcid, number)
## email within same hospital
orcid_dupl_2b = collab_collapsed %>% 
    # distinct(hospital, orcid, .keep_all = TRUE) %>% 
    add_count(hospital, email) %>% 
    arrange(-n, email)




