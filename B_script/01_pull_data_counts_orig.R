############################################################################## #
#' Purpose: Extensible data checking tool for views
#'          - single click to collect data
#'          - RMD with adjustable threshold
#' Author:  Michael Kuo
#' R ver:   4.3+
#' Env:     PAWS Linked Zone
############################################################################## #



# Workspace setup ---------------------------------------------------------

sapply(
  c(
    'tidyverse',
    'withr',
    'logger',
    'lubridate',
    'phrdwRdata',
    'dbplyr'
  ),
  require,
  character.only = T
)

schema <- 'na0014aa'
files_rds <- 
  here::here(
    'C_output', paste0(schema, c(count ='_counts.rds', '_metrics.rds'))
  ) %>% 
  set_names(c('counts', 'metrics'))

n_to_keep <- 20

## Set up logging
log_ns <- paste(schema, 'snapshot', sep = '_')
log_threshold(TRACE, namespace = log_ns)
log_file <- here::here('E_log', paste(schema, 'snapshot.log', sep = '_'))
log_appender(appender_tee(log_file), namespace = log_namespaces())
log_layout(
  namespace = log_namespaces(),
  layout =
    logger::layout_glue_generator(
      paste(
        '{str_pad(level, 7, "right")}',
        '[{format(time, "%Y-%m-%d %H:%M:%S")}]',
        '{msg}'
      )
    )
)

log_info('===== Task start =====', namespace = log_ns)
get_logger_meta_variables(namespace = log_ns) %>% 
  purrr::keep(
    str_detect(names(.), '^(ns|time|r_vesrion|node|arch|user)$')
  ) %>% 
  iwalk(~ log_info(paste(str_pad(.y, 12, side = 'right', pad = '.'), .x)))


# Connect and count -------------------------------------------------------

withr::with_db_connection(
  list(conn = connect_to_phrdw(mart = 'PAWS Linked Zone')),
  {
    
    list_views <- 
      odbc::odbcListObjects(conn, 'SPEDW', schema)$name %>% 
      str_subset('_dim_', negate = T)
    
    list_lzy_tbls <- 
      list_views %>% 
      rlang::set_names() %>% 
      map(~ tbl(conn, dbplyr::in_schema(schema, .x)))
    
    df_col_types <- 
      c(
        'schemas',
        'views',
        'columns',
        'types'
      ) %>% 
      rlang::set_names() %>% 
      map(
        ~ odbc::dbGetQuery(
          conn,
          sprintf('SELECT * FROM sys.%s', .x)
        ) %>% 
          tibble::as_tibble()
      ) %>% 
      imap(
        ~ rename_with(
          .x,
          .cols = matches('^(name|type)$|date'),
          .fn = 
            \(names, type = .y) {
              
              type <- 
                ifelse(
                  type == 'indexes', 'index', stringr::str_sub(type, 1, -2)
                )
              
              if (rlang::is_empty(names)) return(names)
              return(paste0(type, '_', names))
              
            }
        )
      ) %>% 
      {
        
        left_join(
          left_join(
            inner_join(.$views, .$columns, by = 'object_id'),
            .$schemas,
            by = 'schema_id'
          ),
          .$types,
          by = 'system_type_id',
          relationship = 'many-to-many'
        ) %>% 
          select(schema_name, view_name, column_name, type = type_name) %>% 
          filter(schema_name == schema)
        
      }
    
    df_col_types <-
      df_col_types %>% 
      mutate(
        type = 
          case_when(
            str_detect(type, 'int')           ~ 'int',
            str_detect(type, 'float|numeric') ~ 'float',
            str_detect(type, 'char|bit')      ~ 'char',
            str_detect(type, 'date')          ~ 'date',
            .default = type
          )
      )
    
    system.time(
      df_count_results <- 
        list_lzy_tbls %>%
        imap_dfr(
          ~ {
            
            log_info(
              sprintf(
                '==== Processing %s / %s view: %s',
                which(.y == names(list_lzy_tbls)), 
                length(list_lzy_tbls), .y
              )
            )
            
            lzy_tbl <- .x
            cols    <- colnames(lzy_tbl) %>% str_subset('_(id|key)$', negate = T)
            
            tibble(
              view = .y,
              col = cols
            ) %>% 
              left_join(
                select(
                  df_col_types,
                  view = view_name,
                  col  = column_name,
                  type,
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
                    \(n, view, type, col) {
                      
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
                            
                            select(lzy_tbl, all_of(col)) %>% 
                              collect() %>% 
                              count(!!sym(col))
                            
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
                                  side = 'right',
                                  pad = '.')
                        )
                      
                      if (inherits(try_results, 'try-error')) {
                        
                        paste(check_msg, 'Retrieving data') %>% 
                          log_error()
                        
                      } else if (nrow(try_results) == 0) {
                        
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
    
    df_count_results <- 
      df_count_results %>% 
      mutate(
        run_dt_tm = Sys.time()
      )
    
  }
)



# Save output -------------------------------------------------------------

if (!file.exists(files_rds['counts'])) {
  
  saveRDS(df_count_results, file = files_rds['counts'])
  
} else {
  
  read_rds(files_rds['counts']) %>% 
    bind_rows(df_count_results) %>% 
    distinct() %>% 
    group_by(run_dt_tm) %>% 
    slice_max(run_dt_tm, n = n_to_keep) %>% 
    ungroup() %>% 
    saveRDS(files_rds['counts'])
  
}
    
