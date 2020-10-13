# pulling the data for checking dashboard

library(tidyverse)
library(RCurl)
library(readxl)
periods_orig = postForm(uri='https://globalsurgery.redcap.bham.ac.uk/api/',
                        token=Sys.getenv("surgweek_teams"),
                        content = 'record',
                        format ='csv',
                        rawOrLabel='label',
                        rawOrLabelHeaders='raw',
                        exportCheckboxLabel='false',
                        returnFormat = 'json') %>% 
    read_csv()

# Adding specialities and periods to the dashboard:
specialtyperiods_orig = postForm(uri='https://globalsurgery.redcap.bham.ac.uk/api/',
                                 token=Sys.getenv("surgweek_teams"),
                                 content='report',
                                 format='csv',
                                 report_id='252',
                                 rawOrLabel='raw',
                                 rawOrLabelHeaders='raw',
                                 exportCheckboxLabel='false',
                                 returnFormat = 'json') %>% 
    read_csv(col_types = cols(.default = col_character())) %>% 
    mutate(record_id = parse_number(record_id)) %>% 
    rename(specialty_id = specialty)

specialty_abbreviation <- read_excel("specialty abbreviation.xlsx", 
                                     col_types = c("text", "skip", "text"))

specialtyperiods  = specialtyperiods_orig %>% 
    separate_rows(specialty_id) %>% 
    filter_at(vars(all_of(c("specialty_id", "period"))), any_vars(! is.na(.))) %>% 
    left_join(specialty_abbreviation) %>% 
    select(-specialty_id) %>% 
    group_by(record_id, period) %>% 
    summarise(specialty = str_c(specialty, collapse = "-")) %>% 
    rename(Period = period, Specialty = specialty)

periods_check = periods_orig %>%
    mutate(
        # form_complete = if_else(is.na(period_available), "No", "Yes"),
        form_complete = case_when(
            fullname_submitter != "" &
                (registration == "20202020" |
                     is.na(registration)) &
                globalsurgcovidsurg_miniteam_application_portal_complete == "Complete" ~ "Yes",
            TRUE ~ "No"),
        period_choice_success = period_available %>% fct_recode("Yes" = "All good",
                                                                "No" = "Chosen period(s) already taken") %>% as.character(),
        duplicate_check_success = case_when(
            period_choice_success == "Yes" & duplicated == "All good" ~ "Yes",
            period_choice_success == "Yes" &
                duplicated == "Someone in multiple teams" ~ "No",
            TRUE ~ NA_character_),
        emails_unique_success = emails_unique %>% fct_recode("Yes" = "All Good",
                                                             "No" = "Email in use") %>% as.character(),
        orcids_valid_success = case_when(
            orcids_valid == "Yes" & period_choice_success == "Yes" ~ "Yes",
            orcids_valid == "No" & period_choice_success == "Yes" ~ "No",
            TRUE ~ NA_character_),
        accounts_created_success = case_when(accounts_created == "Accounts created" ~ "Yes", 
                                             emails_unique_success == "Yes" ~ "Not yet",
                                             TRUE ~ NA_character_)) %>%
    select(record_id,
        time,
        form_complete,
        period_choice_success,
        orcids_valid_success,
        duplicate_check_success,
        emails_unique_success,
        accounts_created_success) %>% 
    left_join(specialtyperiods)

stopifnot(nrow(periods_orig) == nrow(periods_check))




#save(periods_check, file = "periods_check.rda") 


