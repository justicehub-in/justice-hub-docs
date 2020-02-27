
library(glue)
library(googlesheets4)
library(DT)

data_report_path <- 'content/data-curation/data_report.md'
create_data_report_file <- fs::file_create(path = data_report_path)

yaml_metadata <- glue('
date: "{Sys.Date()}"
draft: false
linktitle: Data curation reports
menu:
  onboarding:
    name: Reports
    weight: 20
title: Data Reports
type: docs
highlight: false
weight: 20
')

# Read google form to access data reports
form_responses_link <- 'https://docs.google.com/spreadsheets/d/1OX1YcFadTx3IpUqae-7yhuZqt-ebAz9skD9hEab10ME/edit?usp=sharing'

# This is a public sheet, hence OAuth is not required
sheets_deauth()

# Read all form responses
form_responses <- sheets_read(ss = form_responses_link)

form_responses$org_alias <-
  form_responses$`Organisation Name` %>% unique() %>% stringr::str_to_lower() %>% stringr::str_trim()
form_responses$org_alias[form_responses$org_alias == 'veratech'] <- 'veratechIN'

# org_alias <- 'nipfp'

# Creating a data report dataframe

cols_to_consider <- c("Organisation Name", "Timestamp", "Title", "Google Drive URL", "Dataset issue report", "Data Issue Status")
master_report_df <- form_responses[,cols_to_consider]
master_report_df$Timestamp <- as.Date(master_report_df$Timestamp)
names(master_report_df)[which(names(master_report_df) == 'Timestamp')] <- 'Date added'

# master_report_dt <- 

# Add all contents to the index file
xfun::write_utf8(
  c(
    '---',
    yaml_metadata,
    '---',
    master_report_dt
  ),
  DT::datatable(master_report_df)
)
