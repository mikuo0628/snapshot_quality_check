generate_data_count <- function(
    mart   = c('CD', 'PAWS Linked Zone')[1],
    schema = c('phs_cd', 'na0014aa')[1],
    type   = c('pre_post', 'sa_su')[1]
) {
  
  require(tidyverse)
  require(phrdwRdata)
  require(withr)
  require(logger)
  require(lubridate)
  require(dbplyr)
  require(odbc)
  
  browser()
  
  conn <- 
    withr::local_db_connection(
      phrdwRdata::connect_to_phrdw(
        mart = mart,
        type = ifelse(type == 'pre_post', 'prod', 'su')
      )
    )
  
  
  list_dbs <- 
    if (type == 'pre_post') { c('SPEDW') } else { c('SAEDW', 'SUEDW') }
  
  list_dbs <- rlang::set_names(list_dbs)
  
  list_dbs %>% 
    map(~ odbc::odbcListObjects(conn, .x, schema)) %>% 
    map(~ pull(.x, name)) %>% 
    map(~ str_subset(.x, '_dim_', negate = T))
  
    
}

generate_data_count()