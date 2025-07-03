############################################################################## #
#' Purpose: Extensible and optional data checking tool for data views
#'          - easy snapshot collection
#' Author:  Michael Kuo
#' R ver:   4.3+
#' Envs:    PAWS LZ
############################################################################## #


# Workspace setup ---------------------------------------------------------

require(tidyverse)
require(withr)
require(logger)
require(lubridate)
require(phrdwRdata)
require(dbplyr)

sapply(
  list.files(here::here('B_script/funcs'), full.names = T),
  source
)

# schema    <- 'na0014aa'
schema    <- 'phs_cd'
# mart      <- 'PAWS Linked Zone'
mart      <- 'CD'
type      <- c('pre_post', 'sa_su')[2]

files_rds <- 
  here::here(
    'C_output', 
    paste0(schema, c('_counts.rds', '_metrics.rds'))
  ) %>% 
  set_names(c('counts', 'metrics'))

# number of most recent snapshots to keep
n_to_keep <- 20

# log setup
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

log_info('===== Task Start =====', namespace = log_ns)
get_logger_meta_variables(namespace = log_ns) %>% 
  purrr::keep(
    str_detect(names(.), '^(ns|time|R_version|node|arch|user)$')
  ) %>% 
  iwalk(~ log_info(paste(str_pad(.y, 12, side = 'right', pad = '.'), .x)))



# Connect and count -------------------------------------------------------

df_output <- 
  generate_data_count(
    mart   = 'CD',
    schema = 'phs_cd',
    type   = 'sa_su'
  )



# Save output -------------------------------------------------------------

if (!file.exists(files_rds['counts'])) {
  
  saveRDS(df_output, file = files_rds['counts'])
  
} else {
  
  read_rds(files_rds['counts']) %>% 
    bind_rows(df_output) %>% 
    distinct() %>% 
    group_by(run_dt_tm) %>% 
    slice_max(run_dt_tm, n = n_keep) %>% 
    ungroup() %>% 
    saveRDS(file = files_rds['counts'])
  
}
