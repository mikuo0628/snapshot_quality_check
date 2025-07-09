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

require(tidyverse)
require(lubridate)
require(magrittr)
require(logger)

sapply(
  list.files(here::here('B_script/funcs'), full.names = T),
  source
)

test_run <- F

schema   <- 'na0014aa'
files_rds <- 
  here::here(
    'C_output', paste0(schema, c(count ='_counts.rds', '_metrics.rds'))
  ) %>% 
  set_names(c('counts', 'metrics'))

df_output <- read_rds(files_rds['counts'])

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

# log_info('===== Task start =====', namespace = log_ns)
get_logger_meta_variables(namespace = log_ns) %>%
  purrr::keep(
    str_detect(names(.), '^(ns|time|r_vesrion|node|arch|user)$')
  ) %>%
  iwalk(~ log_info(paste(str_pad(.y, 12, side = 'right', pad = '.'), .x)))



# Calculate metrics -------------------------------------------------------

df_metrics <- derive_metrics(df_output)



# Save metrics ------------------------------------------------------------

save.image(
  here::here(
    'C_output',
    paste0(schema, '_metrics', '.rdata')
  )
)



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


