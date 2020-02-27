library(googlesheets4)
library(glue)
library(stringr)
library(fs)
library(knitr)
library(kableExtra)
library(tidyverse)

source("assets/scripts/R/utils.R")

form_responses_link <- 'https://docs.google.com/spreadsheets/d/1OX1YcFadTx3IpUqae-7yhuZqt-ebAz9skD9hEab10ME/edit?usp=sharing'
base_dir <- here::here("content/data-curation")

# Column Details
dataset_columns <-
  c("Organisation Name",
    "Title",
    "Dataset URL",
    "Dataset issue report",
    "Data Issue Status"
  )


# This is a public sheet, hence OAuth is not required
sheets_deauth()

# Read all form responses
form_responses <- sheets_read(ss = form_responses_link)

form_responses$org_alias <-
  form_responses$`Organisation Name` %>% unique() %>% stringr::str_to_lower() %>% stringr::str_trim()
form_responses$org_alias[form_responses$org_alias == 'veratech'] <- 'veratechIN'


create_partner_report <- function(org_alias){
  
  org_details <- form_responses[form_responses$org_alias == org_alias,dataset_columns]
  organisation_name <- unique(org_details[,'Organisation Name'])
  
  
  # Data Report path[s]
  org_directory <- glue::glue("{base_dir}/{org_alias}")
  
  # Create directories to host the data report
  fs::dir_create(org_directory)
  
  datareport_path <- glue::glue("{org_directory}/_index.md")
  
  # Create _index.md
  create_index_file <- fs::file_create(path = datareport_path)

  
  # Write YAML metadata
  yaml_metadata <- glue::glue('
linktitle: Data reports | {organisation_name}
summary: List of curated datasets
title: {organisation_name}
date: "{Sys.Date()}"
lastmod: "{Sys.Date()}"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
menu:
  onboarding:
    name: {organisation_name}
') 

  title_with_link <- lapply(org_details$Title, title_to_link) %>% unlist()
    
  org_details$Title <- text_spec(org_details$Title, link = glue('{title_with_link}'))
  org_details$`Dataset URL` <-
    ifelse(
      !is.na(org_details$`Dataset URL`),
      text_spec(org_details$`Dataset URL`, link = org_details$`Dataset URL`),
      org_details$`Dataset URL`
    )
  org_details$`Dataset issue report` <-
    ifelse(
      !is.na(org_details$`Dataset issue report`),
      text_spec(
        org_details$`Dataset issue report`,
        link = org_details$`Dataset issue report`
      ),
      org_details$`Dataset issue report`
    )
  
  org_details_table <-
    org_details %>% select("Title",
                           "Dataset URL",
                           "Dataset issue report",
                           "Data Issue Status") %>% kable(escape = FALSE) %>%   kable_styling(bootstrap_options = c("condensed", "responsive"),
                                                                                          fixed_thead = T)
  
  # Add all contents to the index file
  xfun::write_utf8(
    c(
      '---',
      yaml_metadata,
      '---',
      org_details_table
    ),
    datareport_path
  )
  
  
}


orgs_in_db <- unique(form_responses$org_alias)
lapply(orgs_in_db, create_partner_report)
