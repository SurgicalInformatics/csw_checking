# pulling the data for checking dashboard

library(tidyverse)
library(RCurl)

uri = "https://globalsurgery.redcap.bham.ac.uk/api/"

# pull of the registration times
reg_orig = postForm(
    uri = uri,
    token = Sys.getenv("surgweek_teams"),
    content = 'record',
    format = 'csv',
    rawOrLabel = 'label',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'json') %>%
    read_csv()


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

# assigning clearer values for the dashboard
status_values = reg_orig %>% 
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
        emails_unique_success = case_when(
            emails_unique == "All Good" & is.na(skip_team) & duplicate_check_success == "Yes" & orcids_valid == "Yes" ~ "Yes",
            emails_unique == "Email in use" ~ "No",
            TRUE ~ NA_character_),
        # emails_unique_success = emails_unique %>% fct_recode("Yes" = "All Good",
        #                                                      "No" = "Email in use") %>% as.character(),
        orcids_valid_success = case_when(
            orcids_valid == "Yes" & period_choice_success == "Yes" ~ "Yes",
            orcids_valid == "No" & period_choice_success == "Yes" ~ "No",
            TRUE ~ NA_character_),
        accounts_created_success = case_when(accounts_created == "Accounts created" ~ "Yes", 
                                             emails_unique_success == "Yes" ~ "Not yet",
                                             TRUE ~ NA_character_))

# recreating the usernames for counting purposes
teams_info = teams_orig %>%
    pivot_longer(
        cols = matches("email|skip|orcid|firstname|lastname"),
        names_to = c(".value", "number"),
        names_pattern = "(.+)_(.+)",
        values_drop_na = TRUE) %>%
    mutate(email = tolower(email)) %>%
    mutate(username = str_extract(email, "[^@]+") %>%
               tolower() %>%
               paste0(".sw") %>%
               str_remove("'")) %>%
    mutate(dag_number_reg = formatC(record_id, width = 4, flag = "0"))

# correcting usernames where duplicates have been created
users_orig = teams_info %>% 
    filter(is.na(deregister) & is.na(skip)) %>% 
    select(username, email, dag_number_reg, orcid, firstname, lastname) %>% 
    mutate(username = if_else(email == "michael.sander@chiru.med.uni-giessen.de", "michael.sander2.sw", username)) %>% 
    mutate(username = if_else(email == "elatif@optusnet.com.au", "elatif2.sw", username)) %>% 
    mutate(username = if_else(email == "axtroz4894@mail.ru", "axtroz48942.sw", username))

# # check for duplicates
# check = users_orig %>%
#     group_by(username) %>%
#     count()

# stopifnot(nrow(users_orig) == nrow(check))

# create status for summary data
summary = status_values %>%
    mutate(
        reg_status = case_when(
            accounts_created_success == "Yes" ~ "Accounts created",
            emails_unique_success == "Yes" ~ "Applications approved",
            form_complete == "Yes" &
                is.na(skip_team) & period_choice_success == "No" ~ "Periods issue",
            form_complete == "Yes" &
                is.na(skip_team) ~ "Other issue",
            TRUE ~ NA_character_),
        reg_status_extra = case_when(
            reg_status == "Accounts created" ~ NA_character_,
            form_complete == "No" ~ "Invalid registrations",
            skip_team == "No" ~ "De-registered/merged",
            TRUE ~ NA_character_))

# for first set of coloured boxes
summary_data = summary %>%
    drop_na(reg_status) %>%
    group_by(reg_status) %>%
    summarise(n = n()) %>%
    mutate(percent = (n / sum(n) * 100))

# for second set of coloured boxes
summary_data_extra = summary %>% 
    drop_na(reg_status_extra) %>%
    group_by(reg_status_extra) %>%
    summarise(n = n()) %>%
    mutate(percent = n / sum(n) * 100)