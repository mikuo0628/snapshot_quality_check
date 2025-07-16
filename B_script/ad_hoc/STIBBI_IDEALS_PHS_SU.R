############################################################################## #
#' Purpose: Compare IDEALS and STIBBI
#' Author:  Michael Kuo
#' R ver:   4.3+
#' Envir:   PAWS UAT
############################################################################## #


# Workspace setup ---------------------------------------------------------

require(tidyverse)
require(phrdwRdata)
require(lubridate)
require(dbplyr)
require(odbc)
require(withr)
require(logger)

n_to_keep <- 10
## Filters
source_system <- 'EMR'
phs_diseases  <- c('Human immunodeficiency virus (HIV) infection',
                   'Acquired immunodeficiency syndrome (AIDS)',
                   'Hepatitis C',
                   'Syphilis')

files_rds <- 
  here::here(
    'C_output',
    paste0(
      'IDEALS', c('_counts.rds', '_metrics.rds')
    )
  ) %>% 
  set_names(c('counts', 'metrics'))

# log setup
log_ns <- paste('IDEALS', 'snapshot', sep = '_')
log_threshold(TRACE, namespace = log_ns)
log_file <- here::here('E_log', paste('IDEALS', 'snapshot.log', sep = '_'))
if (!dir.exists(dirname(log_file))) dir.create(dirname(log_file))
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

log_info('===== Task Start =====', namespace = log_ns)
get_logger_meta_variables(namespace = log_ns) %>% 
  purrr::keep(
    str_detect(names(.), '^(ns|time|R_version|node|arch|user)$')
  ) %>% 
  iwalk(~ log_info(paste(str_pad(.y, 12, side = 'right', pad = '.'), .x)))



# Counts ------------------------------------------------------------------

withr::with_db_connection(
  list(conn = phrdwRdata::connect_to_phrdw(mart = 'PAWS UAT', type = 'su')),
  {
    
    df_data_type <- phrdwRdata:::map_sql_view(conn, include_datatype = T)$map
    
    df_data_type <-
      df_data_type %>% 
      select(
        schema = schema_name,
        view   = view_name,
        col    = column_name, 
        type   = type_name
      ) %>% 
      filter(
        schema %in% c('na0014aa', 'nd1031aa'),
        # str_detect(view, '_dim_', negate = T),
        str_detect(view, '_phs_'),
        str_detect(col, '(key|id)$', negate = T),
      ) %>% 
      filter(
        view %in% 
          reduce(
            map(split(., .$schema), ~ unique(pluck(.x, 'view'))),
            intersect
          )
      ) %>% 
      filter(type != 'sysname') %>% 
      mutate(
        type = 
          case_when(
            str_detect(type, 'int')                   ~ 'int',
            str_detect(type, 'float|numeric|decimal') ~ 'float',
            str_detect(type, 'char|bit')              ~ 'char',
            str_detect(type, 'time|date')             ~ 'date',
          )
      )
    
    df_output <- 
      imap_dfr(
        df_data_type %>% 
          split(.$schema),
        ~ {
          
          schema   <- .y
          df_views <- 
            group_by(.x, view) %>% 
            mutate(n = cur_group_id(), .before = 1) %>% 
            ungroup %>% 
            arrange(n)
          n_views  <- max(df_views$n)
          
          imap_dfr(
            split(df_views, df_views$view),
            ~ {
              
              view    <- .y
              df_cols <- 
                select(.x, col, type) %>% 
                group_by(col) %>% 
                mutate(n = cur_group_id(), .before = 1) %>% 
                ungroup() %>% 
                arrange(n)
              
              log_info(
                sprintf(
                  '==== View processing %s / %s: [%s].[%s]',
                  which(view == unique(df_views$view)),
                  n_views,
                  schema, view
                )
              )
              
              lzy_tbl_invs <-
                tbl(conn, in_schema(schema, 'vw_phs_investigation')) %>% 
                filter(source_system != 'EMR') %>% 
                filter(phs_disease %in% phs_diseases) %>% 
                distinct()
              
              lzy_tbl <- tbl(conn, in_schema(schema, view)) %>% distinct()
              
              lzy_tbl <- 
                if (view == 'vw_phs_investigation') {
                  
                  lzy_tbl_invs
                  
                } else if (view == 'vw_phs_client_risk') {
                  
                  lzy_tbl %>% 
                    inner_join(
                      lzy_tbl_invs,
                      by = 'client_id'
                    )
                  
                } else if (view == 'vw_phs_investigation_risk') {
                  
                  lzy_tbl %>% 
                    inner_join(
                      lzy_tbl_invs,
                      by = 'investigation_id'
                    )
                  
                } else {
                  
                  lzy_tbl %>% 
                    inner_join(
                      lzy_tbl_invs,
                      by = 'disease_event_id'
                    )
                  
                }
              
              list_counts <-  
                df_cols %>% 
                pmap(
                  \(n, col, type) {
                    
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
                              try(
                                select(lzy_tbl, all_of(col)) %>% 
                                  collect() %>% 
                                  count(!!sym(col))
                              )
                            
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
                        str_pad(n, width = nchar(nrow(df_cols)), side = 'left'),
                        nrow(df_cols),
                        str_pad(type, width = 5, side = 'left'),
                        str_pad(
                          col, 
                          width = max(nchar(df_cols$col)) + 2,
                          side  = 'right',
                          pad   = '.'
                        )
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
                      
                    } else { browser() }
                    
                  try_results
                  
                  }
                  
                )
              
              mutate(
                df_cols,
                .keep = 'none',
                col, type, 
                result = list_counts
              ) %>% 
                mutate(view = view, .before = 1)
              
            }
          ) %>% 
            mutate(schema = schema, .before = 1)
          
        }
      )
    
    
    df_output <- df_output %>% mutate(run_dt_tm = Sys.time())
    
  }
)



# Save counts -------------------------------------------------------------

if (!file.exists(files_rds['counts'])) {
  
  saveRDS(df_output, file = files_rds['counts'])
  
} else {
  
  read_rds(files_rds['counts']) %>% 
    bind_rows(df_output) %>% 
    distinct() %>% 
    group_by(run_dt_tm) %>% 
    slice_max(run_dt_tm, n = n_to_keep) %>% 
    ungroup() %>% 
    saveRDS(file = files_rds['counts'])
  
}



# Derive metrics ----------------------------------------------------------

df_metrics <- 
  df_output %>% 
  mutate(schema = factor(schema, levels = c('na0014aa', 'nd1031aa'))) %>%
  group_by(schema) %>%
  mutate(
    group =
      factor(
        ifelse(cur_group_id() == 1, 'baseline', 'test'),
        levels = c('baseline', 'test')
      )
  ) %>%
  ungroup() %T>% 
  {
    
    compare_groups <<- distinct(select(., run_dt_tm, schema, group))
    
  } %>% 
  mutate(
    col =
      case_when(
        str_detect(col, 'dt_tm$') ~ str_replace(col, 'dt_tm', 'date'),
        .default = col
      )
  ) %>% 
  arrange(view, col, group) %>% 
  select(-c(schema, run_dt_tm)) %>% 
  pivot_wider(
    names_from  = group,
    values_from = result
  ) %>% 
  {
    
    n_max <- nrow(.)
    
    list_metrics <- 
      pmap(
        mutate(., n = rownames(.), .before = 1),
        \(n, view, col, type, baseline, test) {
          
          log_info(
            sprintf(
              '==== %s / %s: %s.%s',
              str_pad(n, width = nchar(n_max), side = 'left'), n_max,
              view, col
            )
          )
          
          df_compare  <- list(baseline = baseline, test = test)
          col_check   <- NULL
          val_check   <- NULL
          missingness <- NULL
          
          if (type != 'float') {
            
            if (
              !any(
                map_lgl(
                  df_compare, ~ inherits(.x, 'try-error') | is.null(.x)
                )
              )
            ) {
              
              if (
                type == 'char' &
                any(map_lgl(baseline, ~ inherits(.x, 'character')))
              ) {
                
                df_compare <-
                  df_compare %>% 
                  # text cleaning
                  map(
                    ~ mutate(
                      .x,
                      across(where(is.character), \(x) str_trim(tolower(x)))
                    ) %>% 
                      group_by(
                        !!!syms(names(select(., where(is.character))))
                      ) %>% 
                      summarise(n = sum(n, na.rm = T))
                  )
                
              }
              
              df_compare <-
                df_compare %>% 
                reduce(
                  left_join,
                  by = str_subset(names(.[[1]]), '^(n|value)$', negate = T),
                  suffix = paste0('_', names(.))
                ) %>% 
                mutate(
                  diff = .[[length(.)]] - .[[length(.) - 1]]
                )
              
              df_compare <- 
                mutate(
                  df_compare,
                  perc = abs(diff / df_compare[[length(df_compare) - 1]])
                )
              
              val_check <- 
                filter(
                  df_compare,
                  if_any(matches('_(baseline|test)$'), is.na)
                ) %>% 
                select(-c(diff, perc)) %>% 
                rename('val' = 1) %>% 
                mutate(
                  status =
                    case_when(
                      is.na(n_baseline) ~ 'new',
                      is.na(n_test)     ~ 'missing'
                    )
                ) %>% 
                { if (nrow(.) == 0) NULL else { . } }
              
              missingness <- 
                filter(df_compare, is.na(df_compare[[1]])) %>% 
                { if (nrow(.) == 0) NULL else { . } }
              
            } else {
              
              col_check <- 
                df_compare %>% 
                modify(
                  .f = \(x) {
                    
                    if (inherits(x, 'try-error') | is.null(x)) return('missing')
                    return('present')
                    
                  }
                ) %>% 
                as_tibble() %>% 
                mutate(view = view, col = col, .before = 1)
              
            }
            
          } else {
            
            missingness <- df_compare$missing
            
          }
          
          list(
            df_compare  = df_compare ,
            col_check   = col_check,
            val_check   = val_check,
            missingness = missingness  
          )
          
        }
      )
    
    mutate(., metrics = list_metrics) %>% 
      select(-c(baseline, test)) %>% 
      unnest_wider(metrics, simplify = F) %>% 
      mutate(row = rownames(.), .before = 1)
    
  }
  


# Save metrics ------------------------------------------------------------

save.image(
  here::here(
    'C_output',
    paste0('IDEALS', '_metrics', '.rdata')
  )
)



# Render report -----------------------------------------------------------

system.time(
  df_metrics %>% 
    split(.$view) %>% 
    iwalk(
      ~ {
        
        df_metrics <- .x
        
        rmarkdown::render(
          input = here::here('B_script/data_quality_check_report.Rmd'),
          output_dir = here::here('C_output', 'IDEALS'),
          output_file =
            paste(
              # as.character(Sys.Date()),
              paste0( '[', as_date(compare_groups$run_dt_tm), ']', collapse = '.'),
              .y,
              'data_quality_check.html',
              sep = '_'
            ),
          envir = environment(),
          params =
            list(
              use_title = ''
            )
        )
        
      }
    )
)

