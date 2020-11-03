# pulling the data for checking dashboard

library(tidyverse)
library(RCurl)
library(readxl)

uri = "https://globalsurgery.redcap.bham.ac.uk/api/"

## pull of the registration team data
periods_orig = postForm(uri=uri,
                        token=Sys.getenv("surgweek_teams"),
                        content = 'record',
                        format ='csv',
                        rawOrLabel='label',
                        rawOrLabelHeaders='raw',
                        exportCheckboxLabel='false',
                        returnFormat = 'json') %>% 
    read_csv()

# Adding specialities and periods to the dashboard:
specialtyperiods_orig = postForm(uri=uri,
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

# pull report of users from REDCap report: https://globalsurgery.redcap.bham.ac.uk/redcap_v10.2.1/DataExport/index.php?pid=63&report_id=265
teams_orig = RCurl::postForm(
    uri = uri,
    token = Sys.getenv("surgweek_teams"),
    content = 'report',
    format = 'csv',
    report_id = '265',
    rawOrLabel = 'label',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv') %>%
    read_csv() %>%
    rename(
        skip_1 = skip1,
        skip_2 = skip2,
        skip_3 = skip3,
        skip_4 = skip4,
        skip_5 = skip5,
        deregister = skip_team)

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

## assigning clearer values for the dashboard
periods_check = periods_orig %>% 
    mutate(
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
        emails_unique_success = case_when(
            emails_unique == "All Good" & is.na(skip_team) & duplicate_check_success == "Yes" & orcids_valid == "Yes" ~ "Yes",
            emails_unique == "Email in use" ~ "No",
            TRUE ~ NA_character_),
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
        accounts_created_success,
        skip_team,
        emails_unique,
        orcids_valid) %>% 
    left_join(specialtyperiods)
