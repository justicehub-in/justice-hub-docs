library(googlesheets4)
library(here)
library(glue)
library(stringr)
library(fs)
library(readr)
library(knitr)

form_responses_link <- 'https://docs.google.com/spreadsheets/d/1OX1YcFadTx3IpUqae-7yhuZqt-ebAz9skD9hEab10ME/edit?usp=sharing'
base_dir <- here::here("content/data-curation")

# Column Details
dataset_columns <- c("Title", "Description", "Keywords", "Data source details", "Total files", "Date of data collection/publication", "Do we maintain a data dictionary", "Is the data available in machine readable formats", "How was the data collected", "Geographical coverage", "Is raw data available", "Data timeline (From Year - To Year)", "Is the data still updated", "What is the data update frequency", "Language of the dataset", "Does the dataset have PII's (Personally Identifiable Information)", "Level of the dataset", "Google Drive Dataset URL", "Google Drive Data Dictionary URL"  ,"Dataset issue report", "Data Issue Status","Dataset Identifier")
research_links <- "Please share a few research links (if available) where the dataset was used (Use commas to enter multiple links)"
maintainer_details <- "Maintainer Email"
tags <- "Keywords"
data_processing_status <- "Can the data be shared on the JusticeHub as it is, or is some processing required ?"
comments <- "Any other comments"
cdl_facilitator <- "CivicDataLab Facilitator"


# # This is a public sheet, hence OAuth is not required
# sheets_deauth()

# Read all form responses
form_responses <- sheets_read(ss = form_responses_link)

form_responses$org_alias <-
  form_responses$`Organisation Name` %>% unique() %>% stringr::str_to_lower() %>% stringr::str_trim()
form_responses$org_alias[form_responses$org_alias == 'veratech'] <- 'veratechIN'

# org_alias <- 'nipfp'
create_data_report <- function(org_alias) {
  org_details <-
    form_responses[form_responses$org_alias == org_alias, ]
  for (i in 1:nrow(org_details)) {
    org_dataset_title <- org_details[, 'Title'][[i]]
    org_dataset_description <- org_details[, 'Description'][[i]]
    dataset_identifier <- org_details[, 'Dataset Identifier'][[i]]
    org_name <- org_details[, 'Organisation Name'][[i]]
    
    # Data Report path[s]
    org_directory <-
      glue::glue("{base_dir}/{org_alias}/{org_dataset_title}")
    
    # Create directories to host the data report
    fs::dir_create(org_directory)
    
    datareport_path <- glue::glue("{org_directory}/_index.md")
    
    # Create _index.md
    create_index_file <- fs::file_create(path = datareport_path)
    
    # Create YAML meta-data
    # menu_title <-
    #   as.Date(org_details$Timestamp[[i]]) %>% stringr::str_replace_all('-', '_') %>% glue::glue("_{i}")
    # menu_title <- paste0(org_alias, '_', menu_title)
    
    # Process Keywords
    all_tags <-
      org_details$Keywords[[i]] %>% stringr::str_split(pattern = ",") %>% unlist() %>%  stringr::str_trim()
    all_tags <- paste0("[", paste0(all_tags, collapse = ','), "]")
    
    # Write YAML metadata
    yaml_metadata <- glue::glue(
      '
linktitle: {org_dataset_title}
summary: {org_dataset_description}
weight: 1

# Page metadata.
title: {org_dataset_title}
date: "{as.Date(org_details$Timestamp)}"
lastmod: "{Sys.Date()}"
draft: false  # Is this a draft? true/false
toc: true  # Show table of contents? true/false
type: docs  # Do not modify.
tags: {all_tags}

# Add menu entry to sidebar.
# - name: Declare this menu item as a parent with ID `name`.
# - weight: Position of link in menu.
menu:
  onboarding:
    name: {dataset_identifier}
    weight: 1
    parent: {org_name}
'
    )
    
    # File Contents:
    # * Data details
    # * Tags
    # * Research Reports
    # * CDL Facilitator
    # * Comments
    
    #Add data report as table to the file
    data_details <-
      t(org_details[, dataset_columns]) %>% data.frame()
    data_details <- cbind(row.names(data_details), data_details)
    names(data_details) <- c('Variable', 'Description')
    
    md_data_table <-
      data_details %>% knitr::kable(row.names = FALSE, format = 'markdown')
    data_details_md_heading <- "### Dateset details"
    
    #  Add Tags
    tags_heading <- "### Keywords"
    tags_shortcode <- "{{< list_tags >}}"
    
    # Add links to research/publications
    if (!is.na(org_details[, research_links])[[i]]) {
      research_heading <- '### Research Links'
      research_content <-
        stringr::str_split(org_details[, research_links][[i]], pattern = ',') %>% unlist() %>% stringr::str_trim()
      research_content <- paste0("* ", research_content)
    } else {
      research_heading <- ''
      research_content <- ''
    }
    
    
    # Add CDL Facilitator details
    
    facilitator_heading <- "### CDL Facilitator"
    facilitator_alias <-
      org_details[, cdl_facilitator][[i]] %>% stringr::str_split_fixed(pattern = ' ', n = 2) %>% stringr::str_to_lower() %>% stringr::str_trim()
    facilitator_alias <- facilitator_alias[[1]]
    facilitator_content <-
      paste0('{{% mention "{', facilitator_alias, '}" %}}')
    
    # Add comments
    if (!is.na(org_details[, comments][[i]]))
    {
      comment_heading <- "### Note"
      comment_box <- paste0("{{% alert note %}}",
                            org_details[, comments][[i]],
                            "{{% /alert %}}")
    } else {
      comment_heading <- ""
      comment_box <- ""
    }
    
    
    # Add all contents to the index file
    xfun::write_utf8(
      c(
        '---',
        yaml_metadata,
        '---',
        data_details_md_heading,
        md_data_table,
        research_heading,
        research_content,
        comment_heading,
        comment_box,
        facilitator_heading,
        facilitator_content,
        tags_heading,
        tags_shortcode
      ),
      datareport_path
    )
    
  }
}

lapply(unique(form_responses$org_alias),create_data_report)
