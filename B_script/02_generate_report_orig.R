############################################################################## #
#' Purpose: Generate metrics for report and render Rmd
#'          - when run interactively, it will list all the timepoints, and 
#'            users will choose 2 to compare: first is the baseline, second is
#'            the test
#'          - when run as batch, it will choose the newest two time points to
#'            compare: earlier one is baseline, and latter is the test
#' Author:  Michael Kuo
#' R ver:   4.3+
############################################################################## #



# Workspace setup ---------------------------------------------------------

sapply(
  c(
    'tidyverse',
    'lubridate',
    'magrittr',
    'logger'
  ),
  require,
  character.only = T
)

test_run <- F

schema   <- 'na0014aa'
files_rds <- 
  here::here(
    'C_output', paste0(schema, c(count ='_counts.rds', '_metrics.rds'))
  ) %>% 
  set_names(c('counts', 'metrics'))

df_count_results <- read_rds(files_rds['counts'])

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

# Load funcs
source(here::here('B_script/insert_issues.R'))

# log_info('===== Task start =====', namespace = log_ns)
# get_logger_meta_variables(namespace = log_ns) %>% 
#   purrr::keep(
#     str_detect(names(.), '^(ns|time|r_vesrion|node|arch|user)$')
#   ) %>% 
#   iwalk(~ log_info(paste(str_pad(.y, 12, side = 'right', pad = '.'), .x)))


# Calculate metrics -------------------------------------------------------

df_metrics <-
  df_count_results %>% 
  {
    
    if (test_run) {
      
      bind_rows(
        filter(., run_dt_tm == max(run_dt_tm, na.rm = T)),
        filter(., run_dt_tm == max(run_dt_tm, na.rm = T)) %>% 
          mutate(run_dt_tm = run_dt_tm - days(1)) %>% 
          group_by(type) %>% 
          mutate(
            result =
              {
                
                rand_selects <- sample(length(type), 10)
                
                cat(
                  sprintf(
                    'Inserting issues in type %s for: \n%s',
                    unique(type),
                    paste('  - ', rand_selects, collapse = '\n')
                  ),
                  '\n'
                )
                
                result[rand_selects] <- 
                  result[rand_selects] %>% 
                  map(
                    ~ insert_issues(
                      .x, type = unique(type), 
                      .remove_col = sample(c(T, F), 1, prob = c(0.1, 0.9))
                    )
                  )
                
                
                result
                
              }
          ) %>% 
          ungroup
      )
      
    } else {
      
      slice_max(., run_dt_tm, n = 2, by = c(view, col))
      
    }
    
  } %>% 
  arrange(run_dt_tm) %>% 
  group_by(run_dt_tm) %>% 
  mutate(
    group = ifelse(cur_group_id() == 1, 'baseline', 'test')
  ) %>% 
  ungroup %T>% 
  { compare_groups <<- distinct(select(., run_dt_tm, group)) } %>% 
  select(-run_dt_tm) %>% 
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
              "==== %s / %s: %s.%s",
              str_pad(n, width = nchar(n_max), side = 'left'), n_max,
              view, col
            )
          )
          
          #1 count differences
          ## can count go down?
          #2 col differences
          #3 value differences
          #4 missingness
          
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
              
              df_compare <- 
                df_compare %>% 
                reduce(
                  left_join,
                  by = str_subset(names(.[[1]]), '^(n|value)$' , negate = T),
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
                )
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
            df_compare     = df_compare,
            col_check      = col_check,
            val_check      = val_check,
            missingness    = missingness
          )
          
        }
      )
    
    mutate(
      .,
      metrics = list_metrics
    ) %>% 
      select(-c(baseline, test)) %>% 
      unnest_wider(metrics, simplify = F) %>% 
      mutate(row = rownames(.), .before = 1)
      
  }



# Save metrics ------------------------------------------------------------

# df_metrics %>%
#   saveRDS(files_rds['metrics'])
save.image('test_run.rdata')



# Render report -----------------------------------------------------------

rmarkdown::render(
  input = here::here('B_script/data_quality_check_report.Rmd'),
  output_dir = here::here('C_output', schema),
  output_file =
    paste(
      # as.character(Sys.Date()),
      paste0( '[', as_date(compare_groups$run_dt_tm), ']', collapse = '.'),
      sprintf('data_quality_check_%s.html', ifelse(test_run, 'fake', 'real')),
      sep = '_'
    ),
  envir = environment(),
  params =
    list(
      use_title = ''
    )
)


