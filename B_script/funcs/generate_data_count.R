generate_data_count <- function(
    mart            = c('CD', 'PAWS Linked Zone')[1],
    schema          = c('phs_cd', 'na0014aa')[1],
    type            = c('pre_post', 'sa_su')[1],
    cut_by_dates    = F
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
        conn    = .x, 
        catalog = .y, 
        schema  = schema,
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
          str_detect(type, 'int')                   ~ 'int',
          str_detect(type, 'float|numeric|decimal') ~ 'float',
          str_detect(type, 'char|bit')              ~ 'char',
          str_detect(type, 'time|date')             ~ 'date',
          .default = type
        )
    )
  
  if (isTRUE(cut_by_dates)) {
    
    list_col_dates <- 
      list_col_types %>% 
      map(filter, type ==  'date') %>% 
      map(filter, str_detect(col, 'birth|dt_tm', negate = T))
    
  }
  
  list_views <-
    conns %>% 
    imap(~ odbc::odbcListObjects(.x, .y, schema)) %>% 
    map(~ pull(.x, name)) %>% 
    map(~ str_subset(.x, '_dim_', negate = T))
  
  df_views <-
    list_views %>% 
    imap(~ tibble(view = .x, db = .y)) %>% 
    reduce(full_join, by = 'view', suffix = paste0('_', names(.))) %>% 
    nest(dbs = matches('^db_')) %>% 
    mutate(dbs = map(dbs, ~ unlist(.x))) %>% 
    filter(!map_lgl(dbs, ~ any(is.na(.x))))
  
  run_dt_tm <- Sys.time()
  
  system.time(
    df_count_results <- 
      df_views %>% 
      # filter(view == 'vw_lis_test') %>% 
      # filter(!is.na(db)) %>%
      pmap_dfr(
        \(view, dbs) {
          
          if (isTRUE(cut_by_dates)) {
            
            cut_by_dates <- 
              map_dfr(
                rlang::set_names(na.omit(dbs)),
                \(db) {
                  
                  lzy_tbl <- tbl(conns[[db]], in_schema(schema, view))
                  cols_date <- filter(list_col_dates[[db]], .data$view == .env$view)$col
                  
                  date_ranges <-
                    lzy_tbl %>% 
                    select(all_of(cols_date)) %>% 
                    collect() %>% 
                    as.list() %>% 
                    map(lubridate::ymd) %>% 
                    map(range, na.rm = T) %>% 
                    map(~ na_if(as.double(.x), c(Inf, -Inf))) %>% 
                    map(as_date) %>% 
                    map(set_names, c('from', 'to'))
                  
                  tibble(
                    db          = db,
                    view        = view,
                    col         = names(date_ranges),
                    date_ranges = date_ranges
                  ) %>% 
                    unnest_wider(date_ranges)
                  
                }
              )
            
          }
            
          if (is_tibble(cut_by_dates)) {
            
            cut_by_dates <-
              filter(cut_by_dates, .env$view == .data$view) %>% 
              mutate(across(matches('from|to'), as_date)) %>% 
              summarise(
                .by  = c(view, col),
                across(matches('from'), max, na.rm = T),
                across(matches('to'),   min, na.rm = T),
              ) %>% 
              select(col, matches('^(from|to)$')) %>% 
              drop_na() %>% 
              pivot_longer(
                cols = where(is.Date),
                # cols = matches('from|to'),
                names_to  = 'which',
                values_to = 'date'
              ) %>% 
              pmap(
                \(col, which, date) {
                  
                  paste(
                    col,
                    # ifelse(str_detect(which, 'from'), '>=', '<='),
                    ifelse(str_detect(which, 'from'), '>', '<'),
                    format(date, "'%Y-%m-%d'")
                  ) %>% 
                    rlang::parse_expr()
                  
                }
              )
            
          }
          
          map_dfr(
            rlang::set_names(na.omit(dbs)),
            \(db) {
              
              log_info(
                sprintf(
                  '==== View processing %s /%s: [%s].[%s]',
                  which(view == df_views$view),
                  nrow(df_views),
                  db, view
                )
              )
              
              lzy_tbl <- tbl(conns[[db]], in_schema(schema, view))
              cols    <- str_subset(colnames(lzy_tbl), '_(id|key)$', negate = T)
              
              # TODO: temp SA/SU STIBBI joins
              if (type == 'sa_su') {
                
                if (str_detect(view, 'organism|udf_all|body_site_amr')) {
                  
                  if (view == 'vw_lis_organism') {
                    
                    lzy_tbl <- 
                      lzy_tbl %>% 
                      left_join(
                        select(
                          tbl(conns[[db]], in_schema(schema, 'vw_lis_test')),
                          matches('((test|event)_id|(collection|surveillance)_date)$')
                        ),
                        by = 'test_id'
                      )
                    
                  } else {
                    
                    lzy_tbl <- 
                      lzy_tbl %>% 
                      left_join(
                        select(
                          tbl(conns[[db]], in_schema(schema, 'vw_phs_investigation')),
                          matches('((test|event)_id|(collection|surveillance)_date)$')
                        ),
                        by = 'disease_event_id'
                      )
                    
                  }
                  
                }
                
              }
              
              if (!is.null(cut_by_dates) & !isFALSE(cut_by_dates)) {
                
                lzy_tbl <- filter(lzy_tbl, !!!cut_by_dates)
                
              }
              
              tibble(
                db   = db,
                view = view,
                col  = cols
              ) %>% 
                # filter(col == 'patient_city') %>% 
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
                      \(n, db, view, col, type) {
                        
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
                              
                              try_int <- 
                                try(
                                  select(lzy_tbl, all_of(col)) %>% 
                                    count(!!sym(col)) %>% 
                                    collect()
                                )
                              
                              if (inherits(try_int, 'try-error')) {
                                
                                try_int <- 
                                  select(lzy_tbl, all_of(col)) %>% 
                                  collect() %>% 
                                  count(!!sym(col))
                                
                              }
                              
                              try_int
                              
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
                          
                        } else if (nrow(try_results) == 0) {
                          
                          paste(check_msg, 'No results') %>% 
                            log_warn()
                          
                        } else if (nrow(try_results) != 0) {
                          
                          paste(check_msg, 'Captured') %>% 
                            log_success()
                          
                        } else {
                          
                          browser()
                          
                        }
                        
                        return(try_results)
                        
                      }
                    )
                  
                  mutate(df_temp, result = list_counts) %>% 
                    select(-n)
                  
                }
              
            }
          )
          
        }
      )
  )
  
  return(mutate(df_count_results, run_dt_tm = run_dt_tm))
    
}
