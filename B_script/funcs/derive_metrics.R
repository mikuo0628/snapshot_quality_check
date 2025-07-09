derive_metrics <- function(df_output, test_run = F) {
  
  require(tidyverse)
  require(lubridate)
  
  # check if this will be pre/post or sa/su
  # and reshape accordingly
  df_output <- 
    if (length(unique(df_output$db)) == 2) {
      
      slice_max(df_output, run_dt_tm, n = 1, by = c(db, view, col)) %>% 
        mutate(db = factor(db, levels = c('SAEDW', 'SUEDW'))) %>% 
        group_by(db)
      
    } else {
      
      slice_max(df_output, run_dt_tm, n = 2, by = c(db, view, col)) %>% 
        group_by(run_dt_tm)
      
    }
  
  df_output <-
    mutate(
      df_output, 
      group = 
        factor(
          ifelse(cur_group_id() == 1, 'baseline', 'test'),
          levels = c('baseline', 'test')
        )
    ) %>% 
    ungroup()
  
  if (test_run) {
    
    # insert errors
    
  }
  
  compare_groups <<- distinct(select(df_output, run_dt_tm, db, group))
  
  df_output %>% 
    select(-matches('db|run_dt_tm')) %>%
    arrange(view, col, group) %>% 
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
  
}

