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
  
  func_env <- environment()
  
  conns <-
    {
      
      if (type == 'pre_post') {
        
        list(SPEDW = 'prod')
        
      } else {
        
        list(SAEDW = 'sa', SUEDW = 'su')
        
      }
      
    } %>%
    map(
      ~ withr::local_db_connection(
        phrdwRdata::connect_to_phrdw(
          mart = mart,
          type = .x
        ),
        .local_envir = func_env
      )
    )
  
  list_col_types <- 
    imap(
      conns,
      ~ phrdwRdata:::map_sql_view(
        conn = .x, catalog = .y, schema = schema,
        include_datatype = T
      )$map
    ) %>% 
    map(
      select,
      schema = schema_name,
      view   = view_name,
      col    = column_name,
      type   = type_name
    ) %>% 
    map(
      mutate,
      type =
        case_when(
          str_detect(type, 'int')           ~ 'int',
          str_detect(type, 'float|numeric') ~ 'float',
          str_detect(type, 'char|bit')      ~ 'char',
          str_detect(type, 'date')          ~ 'date',
          .default = type
        )
    )
  
  list_views <-
    conns %>% 
    imap(~ odbc::odbcListObjects(.x, .y, schema)) %>% 
    map(~ pull(.x, name)) %>% 
    map(~ str_subset(.x, '_dim_', negate = T))
  
  df_views <-
    list_views %>% 
    imap(~ tibble(view = .x, db = .y)) %>% 
    reduce(full_join, by = 'col')
  
  run_dt_tm <- Sys.time()
  
  system.time(
    df_count_results <- 
      df_views %>% 
      filter(!if_any(matches('^db'), is.na)) %>% 
      pmap(
        \(view, db){
          
          log_info(
            sprintf(
              '==== View processing %s /%s: %s',
              which(view == df_views$view),
              nrow(df_views),
              view
            )
          )
          
          lzy_tbl <- tbl(conns[[db]], in_schema(schema, view))
          cols    <- colnames(lzy_tbl) %>% str_subset('_(id|key)$', negate = T)
          
          tibble(
            view = view,
            col  = cols
          ) %>% 
            left_join(
              select(
                list_col_types[[db]],
                view, col, type
              ),
              by = c('view', 'col')
            ) %>% 
            mutate(n = rownames(.), .before = 1) %>% 
            {
              
              df_temp <- .
              n_max   <- nrow(df_temp)
              
              list_counts <- 
                pmap(
                  df_temp,
                  \(n, view, col, type) {
                    
                    try_results <- 
                      try(
                        
                        if (type == 'date') {
                          
                          select(lzy_tbl, all_of(col)) %>% 
                            collect() %>% 
                            mutate(across(everything(), lubridate::ymd)) %>% 
                            mutate(
                              .keep = 'none',
                              year  = year(!!sym(col)),
                              month = month(!!sym(col)),
                            ) %>% 
                            mutate(across(everything(), as.integer)) %>% 
                            count(!!!syms(names(.)))
                          
                        } else if (type == 'char') {
                          
                          select(lzy_tbl, all_of(col)) %>% 
                            count(!!sym(col)) %>% 
                            collect()
                          
                        } else if (type == 'int') {
                          
                          browser()
                          select(lzy_tbl, all_of(col)) %>% 
                            count(!!sym(col)) %>% 
                            collect()
                          
                        } else if (type == 'float') {
                          
                          select(lzy_tbl, all_of(col)) %>% 
                            collect() %>% 
                            summarise(
                              n       = n(),
                              min     = min(na.rm = T, !!sym(col)),
                              max     = max(na.rm = T, !!sym(col)),
                              mean    = mean(na.rm = T, !!sym(col)),
                              median  = median(na.rm = T, !!sym(col)),
                              sd      = sd(na.rm = T, !!sym(col)),
                              q_1     = quantile(na.rm = T, !!sym(col), probs = c(0.25)),
                              q_3     = quantile(na.rm = T, !!sym(col), probs = c(0.75)),
                              missing = sum(na.rm = T, is.na(!!sym(col)))
                            )
                          
                        }
                        
                      )
                    
                    check_msg <- 
                      sprintf(
                        '=== %s / %s %s: %s',
                        str_pad(n, width = nchar(n_max), side = 'left'),
                        n_max,
                        str_pad(type, width = 5, side = 'left'),
                        str_pad(col,
                                width = max(nchar(df_temp$col) + 2),
                                side  = 'right', 
                                pad   = '.')
                      )
                    
                    if (inherits(try_results, 'try-error')) {
                      
                      paste(check_msg, 'Retrieving data') %>% 
                        log_error()
                      
                    } else if (nrow(try_result) == 0) {
                      
                      paste(check_msg, 'No results') %>% 
                        log_warn()
                      
                    } else {
                      
                      paste(check_msg, 'Captured') %>% 
                        log_success()
                      
                    }
                    
                    return(try_results)
                    
                  }
                )
              
              mutate(df_temp, result = list_counts) %>% 
                select(-n)
              
            }
          
        }
      )
  )
  
  df_count_results <- df_count_results %>% mutate(run_dt_tm = run_dt_tm)
    
}



generate_data_count(mart = 'PAWS Linked Zone', schema = 'na0014aa')
generate_data_count(type = 'sa_su')

