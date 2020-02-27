library(stringr)

title_to_link <- function(title){
  
  if(!is.na(title)){
    title_link <-
      title %>% 
      str_replace_all(pattern = '[\\(\\)\\-]', replacement = ' ') %>% 
      str_trim() %>% 
      str_replace_all(pattern = '[:space:]+', replacement = '-') %>%
      str_trim(side = 'both')
  } else {
    title_link <- title
  }
    return(title_link)
}

# title <- 'Companies Registration Database (2009-2019)'
# title_to_link(title)
