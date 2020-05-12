# Format dataset titles as links
title_to_link <- function(title){
  if(!is.na(title)){
    title_link <-
      title %>% 
      str_replace_all(pattern = '[\\(\\)\\-\\:]', replacement = ' ') %>% 
      str_trim() %>% str_squish() %>% 
      str_replace_all(pattern = '[:space:]+', replacement = '-')
  } else {
    title_link <- title
  }
    return(title_link)
}

# Read all form responses
read_form_responses <- function(sheet_url="https://docs.google.com/spreadsheets/d/1GSa_hZkvdsd3YsvGUiiks08hVLRkrKWx4yb8MDI-2VU/edit?usp=sharing"){
  # designate project-specific cache
  options(gargle_oauth_cache = ".secrets",
          gargle_oauth_email = "apoorv@civicdatalab.in")
  # sheets_auth()
  form_responses_link <- sheet_url
  form_responses <- sheets_read(ss = form_responses_link)
  form_responses$`Date of data collection/publication` <- form_responses$`Date of data collection/publication` %>% as.character()
  form_responses$org_alias <-
    form_responses$`Organisation Name` %>% stringr::str_to_lower() %>% stringr::str_trim() %>% stringr::str_replace_all(" ","")
  form_responses$org_alias[form_responses$org_alias == 'veratech'] <- 'veratechIN'
  return(form_responses)
}

read_other_sheets <- function(sheet_url){
  # designate project-specific cache
  options(gargle_oauth_cache = ".secrets",
          gargle_oauth_email = "apoorv@civicdatalab.in")
  form_responses <- sheets_read(ss = sheet_url)
  return(form_responses)
}

# Create data reports for every dataset
create_data_report <- function(org_alias) {
  # Column Details
  dataset_columns <-
    c(
      "Title",
      "Description",
      "Data source details",
      "Dataset License",
      "Total files",
      "Date of data collection/publication",
      "Do we maintain a data dictionary",
      "Is the data available in machine readable formats",
      "How was the data collected",
      "Geographical coverage",
      "Is raw data available",
      "Data timeline (From Year - To Year)",
      "Is the data still updated",
      "What is the data update frequency",
      "Language of the dataset",
      "Does the dataset have PII's (Personally Identifiable Information)",
      "Level of the dataset",
      "Dataset URL",
      "Data Dictionary URL",
      "Dataset issue report",
      "Data Issue Status",
      "Dataset Identifier"
    )
  research_links <- "Please share a few research links (if available) where the dataset was used (Use commas to enter multiple links)"
  maintainer_details <- "Maintainer Email"
  tags <- "Keywords"
  data_processing_status <- "Can the data be shared on the JusticeHub as it is, or is some processing required ?"
  comments <- "Any other comments"
  cdl_facilitator <- "CivicDataLab Facilitator"
  
  org_details <-
    form_responses[form_responses$org_alias == org_alias, ]
  for (i in 1:nrow(org_details)) {
    org_dataset_title <- org_details[, 'Title'][[i]]
    org_dataset_title <- stringr::str_replace_all(org_dataset_title, pattern="[[:punct:]]", replacement = '')
    org_dataset_description <- org_details[, 'Description'][[i]]
    dataset_identifier <- org_details[, 'Dataset Identifier'][[i]]
    org_name <- org_details[, 'Organisation Name'][[i]]%>% stringr::str_to_title() %>% stringr::str_replace_all(pattern = " ",replacement = "")
    
    # Data Report path[s]
    org_directory <-
      glue::glue("content/data-curation/{org_alias}/{org_dataset_title}")
    
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
date: "{lubridate::with_tz(ymd_hms(org_details$Timestamp,tz = "Asia/Calcutta"),tz = "UTC")}"
lastmod: "{lubridate::with_tz(Sys.time(),tz = "UTC")}"
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

# Create data report summary for every data contributor
create_partner_report <- function(org_alias){
  # Column Details
  dataset_columns <-
    c("Organisation Name",
      "Title",
      "Dataset URL",
      "Dataset issue report",
      "Data Issue Status"
    )
  
  org_details <- form_responses[form_responses$org_alias == org_alias,dataset_columns]
  organisation_name <- unique(org_details[,'Organisation Name']) %>% stringr::str_to_title() %>% stringr::str_replace_all(pattern = " ",replacement = "")
  
  
  # Data Report path[s]
  org_directory <- glue::glue("content/data-curation/{org_alias}")
  
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
date: "{lubridate::with_tz(Sys.time(),tz = "UTC")}"
lastmod: "{lubridate::with_tz(Sys.time(),tz = "UTC")}"
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

# Create a report with all datasets and all orgs
all_data_report <- function(){
  
  datareport_path <- glue::glue("content/data-reports/_index.md")
  
  # Create _index.md
  create_index_file <- fs::file_create(path = datareport_path)
  
  yaml_metadata <- glue('
title: "Data Reports"
draft: no
date: "{lubridate::with_tz(Sys.time(),tz = "UTC")}"
linktitle: Justice Hub | Data Reports
summary: Data curation status for the Justice Hub
  legal datasets to be released in the first phase of the project
lastmod: "{lubridate::with_tz(Sys.time(),tz = "UTC")}"
type: post
')
  
  cols_to_consider <- c("Organisation Name", "org_alias","Timestamp", "Title", "Dataset URL", "Dataset issue report", "Data Issue Status")
  master_report_df <- form_responses[,cols_to_consider]
  master_report_df$Timestamp <- as.Date(master_report_df$Timestamp)
  names(master_report_df)[which(names(master_report_df) == 'Timestamp')] <- 'Date added'
  master_report_df$title_with_link <- lapply(master_report_df$Title, title_to_link) %>% unlist()
  master_report_df$title_with_link <- glue("../data-curation/{str_to_lower(master_report_df$org_alias)}/{master_report_df$title_with_link}")
  master_report_df$`Organisation Name` <- text_spec(master_report_df$`Organisation Name`, link = glue('../data-curation/{master_report_df$org_alias}'))
  master_report_df$Title <- text_spec(x = master_report_df$Title, link = master_report_df$title_with_link)
  
  master_report_df$`Dataset URL` <-
    ifelse(
      !is.na(master_report_df$`Dataset URL`),
      text_spec(master_report_df$`Dataset URL`, link = master_report_df$`Dataset URL`),
      master_report_df$`Dataset URL`
    )
  master_report_df$`Dataset issue report` <-
    ifelse(
      !is.na(master_report_df$`Dataset issue report`),
      text_spec(
        master_report_df$`Dataset issue report`,
        link = master_report_df$`Dataset issue report`
      ),
      master_report_df$`Dataset issue report`
    )
  
  master_report_df$org_alias <- NULL
  master_report_df$title_with_link <- NULL
  
  report_table <- master_report_df %>% knitr::kable(escape = FALSE)
  
  xfun::write_utf8(
    c(
      '---',
      yaml_metadata,
      '---',
      report_table
    ),
    datareport_path
  )
}

update_website <- function(){
  form_responses <<-  read_form_responses()
  all_orgs <- unique(form_responses$org_alias)
  lapply(all_orgs, create_partner_report)
  lapply(all_orgs, create_data_report)
  # all_data_report()
  # blogdown::build_site(method = 'html')
}

