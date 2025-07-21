############################################################################## #
#' Purpose: Map out completely cube to SQL relationships for VPD
#' Author:  Michael Kuo
############################################################################## #



# Workspace setup ---------------------------------------------------------

sapply(
  c(
    'tidyverse',
    'phrdwRdata',
    'dbplyr',
    'magrittr',
    'withr',
    'logger',
    'readxl',
    'tokenizers'
  ),
  require, character.only = T
)

here::here('B_script/funcs/') %>% 
  list.files(full.names = T) %>% 
  walk(source)

name_comparison <- 'vpd'
dir_output <- here::here('C_output', name_comparison)

catalogs <-
  c(
    ctrl = 'SA_PHRDW_VPD'
    # ctrl = 'SU_PHRDW_Enteric_Panorama'
  )

schema <- 'nc0642aa'
cube   <- 'VPD'

conns_cube <- 
  map(
    catalogs,
    ~ connect_to_phrdw(
      .conn_str =
        list(
          cube =
            paste(
              sep = ';',
              # 'Data Source=SNPDBSBI001.phsabc.ehcnet.ca\\NPIDBSBICMB', # doesn't work
              'Data Source=SPRSASBI001.phsabc.ehcnet.ca\\PRISASBIM',
              paste0('Initial catalog=', .x),
              'Provider=MSOLAP',
              'Packet Size=32767'
            )
        )
    )
  )

cube_conn_str <- 
  list(
    cube =
      paste(
        sep = ';',
        "Data Source=SPRSASBI001.phsabc.ehcnet.ca\\PRISASBIM",
        paste0('Initial catalog=', catalogs['ctrl']),
        "Provider=MSOLAP",
        "Packet Size=32767"
      )
  )



# Maps --------------------------------------------------------------------

## Full maps of cube and sql
map_cube <- try(map(conns_cube, dmv_map_cube, cube = cube, is_visible = F))
map_sql  <- try(sql_map_view())
dfs_sql  <- 
  map_sql$map %>% 
  filter(schema_name == schema) %>% 
  select(-where(is.POSIXct), -schema_name) %>% 
  split(str_detect(.$view_name, 'dim')) %>% 
  set_names(c('fact', 'dim'))

## Join keys ----
## Dev's diagram may not be fully accurate
## Try to match join keys between views and dim views by text
map_join_keys <-
  map_sql$map %>% 
  filter(schema_name == schema) %>% 
  filter(str_detect(view_name, 'UAT', negate = T)) %>% 
  filter(str_detect(column_name, '(key|id)$')) %>% 
  # filter(str_detect(column_name, '(lha|case_id)', negate = T)) %>% 
  select(!where(is.POSIXct)) %>% 
{
  
  df_all <- .
  dfs_split <- 
    split(df_all, str_detect(df_all$view_name, 'dim')) %>% 
    set_names(c('fact', 'dim'))
  
  max_dist <- 0.53
  pmap_dfr(
    dfs_split$fact,
    # dfs_split$fact %>% filter(str_detect(column_name, 'postal_code_key')),
    \(...) {
      
      params <- rlang::list2(...)
      
      # q <- round(nchar(params$column_name) / 5) %>% 
      #   { if (. > 3 ) 3 else { . } }
      
      match_words <-
        dfs_split %>% 
        map(
          ~ fuzzy_match(
            x       = params$column_name,
            table   = .x$column_name,
            # q       = round(nchar(params$column_name) / 5),
            q       = 3,
            maxDist = max_dist,
            nomatch = 0,
            method  = 'jaccard',
            dict    =
              list(
                'age_key' = c(
                  "age_at_collection_key",
                  "age_at_receive_key",
                  "age_at_result_key",
                  "age_at_surveillance_reported_date_key",
                  "age_at_case_earliest_date_key",
                  "age_at_collection_key",
                  "age_at_receive_key",
                  "age_at_result_key",
                  "age_at_surveillance_reported_date_key",
                  "age_at_case_date_key"
                ),
                'package_key' = 'package_key'
              )
          )
        )
      
      bind_cols(
        as_tibble(params),
        map2_dfr(
          dfs_split, match_words, 
          ~ filter(.x, column_name %in% .y)
        ) %>% 
          select(
            join_key = column_name,
            join_view = view_name
          )
      )
      
    }
  ) %>% 
    left_join(dfs_split$fact, .) %>% 
    filter(view_name != join_view) %>% 
    mutate(
      distinct_key =
        pmap(list(view_name, column_name, join_key, join_view), c) %>% 
        map(sort)
    ) %>% 
    distinct(distinct_key, .keep_all = T) %>% 
    select(-where(is.list)) %>% 
    rename(view_key = column_name) #%>% 
    # view()
  
}

## Count keys ----
## Try to best match views and cols to the appropriate cube measure
map_count_keys <-
  tribble(
    ~ measure_name,                 ~ column_name,        
    "Case Count",                   'case_id',            
    # "Case Patient Count",           'case_id',            
    "Case Patient Count",           'patient_master_key',
    "LIS Patient Count",            'patient_master_key',
    "LIS Test Count",               'test_id',            
    "PHS Client Count",             'client_id',          
    "PHS Disease Event Count",      'disease_event_id',   
    # "Patient Count",                'patient_master_key', 
  ) %>% 
  left_join(
    map_sql$map %>% 
      select(view_name, column_name) %>% 
      distinct %>% 
      filter(view_name %in% unique(map_join_keys$view_name)) %>%
      filter(str_detect(column_name, '(id|key)$')) %>% 
      filter(
        !str_detect(
          column_name, 
          paste0(
            '(',
            paste(
              sep = '|',
              'earli',
              'conn',
              'collect',
              'prov',
              'container',
              'ha',
              'ord',
              'date',
              'invest',
              'corr',
              'loca',
              'age',
              'spec'
            ),
            ')'
          )
        )
      ),
    relationship = 'many-to-many'
  ) %>% 
  filter(
    map2_lgl(
      view_name, column_name,
      ~ .y %in% 
        filter(map_sql$map, schema_name == schema, view_name == .x)$column_name
    )
  )



# Build -------------------------------------------------------------------

system.time(
  df_match <- 
    map_cube$ctrl %>% 
    select(
      # measure_name,
      dimension_name, 
      hierarchy_name
    ) %>% 
    distinct %>% 
    # filter(hierarchy_name == 'PHS Classification Group') %>%
    {
      
      df_cube_temp <- mutate(., n = rownames(.), .before = 1)
      dict <- 
        list(
           'organism_level_1' = 'Genus',
           'organism_level_2' = 'Species',
           'organism_level_3' = 'Subspecies',
           'organism_level_4' = 'Serotype',
           'hsda'             = 'Health Service Delivery area',
           'lha'              = 'Local Health Area',
           'ha'               = 'Health Authority'
        )
      
      # check_dist %>%
      #   filter(cosine_column == jaccard_column) %>%
      #   select(
      #     dimension_name, hierarchy_name,
      #     matches('column'), where(is.numeric)
      #   ) %>%
      #   view
      # 
      # system.time(
      #   check_dist <-
      #     pmap_dfr(
      #       df_cube_temp,
      #       \(...) {
      # 
      #         dim_hier <- rlang::list2(...)
      #         q <- 4
      #         cat('\n')
      #         cat(
      #           do.call(sprintf, append(list('%s: [%s].[%s]'), dim_hier)), '\n'
      #         )
      # 
      #         cosine_dists <-
      #           fuzzy_match(
      #             a =
      #               paste(dim_hier[2:3], collapse = ' ') %>%
      #               janitor::make_clean_names(),
      #             b =
      #               dfs_sql$fact %>%
      #               pmap(paste, collapse = ' ') %>%
      #               map(janitor::make_clean_names) %>%
      #               unlist,
      #             q = q,
      #             method = 'cosine',
      #             dict = dict,
      #           )
      # 
      #         jaccard_dists <-
      #           fuzzy_match(
      #             a =
      #               paste(dim_hier, collapse = ' ') %>%
      #               janitor::make_clean_names(),
      #             b =
      #               dfs_sql$fact %>%
      #               pmap(paste, collapse = ' ') %>%
      #               map(janitor::make_clean_names) %>%
      #               unlist,
      #             q = q,
      #             method = 'jaccard',
      #             dict = dict,
      #           )
      # 
      #         list(
      #           cosine  = cosine_dists,
      #           jaccard = jaccard_dists
      #         ) %>%
      #           imap_dfc(
      #             ~ {
      #               type <- .y
      #               tibble(
      #                 min  = min(.x),
      #                 mean = mean(.x),
      #                 sd   = sd(.x),
      #                 result = dfs_sql$fact[which(.x == min(.x)), ]
      #               ) %>%
      #                 unnest(result) %>%
      #                 rename_with(
      #                   .fn =
      #                     ~ str_remove(.x, '_name') %>%
      #                     str_c(type, ., sep = '_')
      #                 )
      #             }
      #           ) %>%
      #           bind_cols(as_tibble(dim_hier[2:3]), .)
      # 
      #       }
      #     )
      # )
      
      pmap_dfr(
        df_cube_temp,
        \(...) {
          
          dim_hier <- rlang::list2(...)
          qgram    <- 3
          cat('\n')
          cat(do.call(sprintf, append(list('%s: [%s].[%s]'), dim_hier)), '\n')
          
          # 2. If could not match anything with dim and hier, check hier 
          #    against vw_dim
          # Must be full match: Jaccard == 0
          match_hier <-
            fuzzy_match(
              a = 
                paste(dim_hier$hierarchy_name, collapse = ' ') %>% 
                janitor::make_clean_names(),
              b = 
                dfs_sql$dim$column_name %>% 
                map(janitor::make_clean_names) %>% 
                unlist,
              q = qgram,
              method = 'jaccard',
              dict = dict,
            ) %>% { dfs_sql$dim[which(. == 0), ] } %>% 
            rename_with(
              .cols = matches('view|column'),
              .fn   = ~ paste('dim', .x, sep = '_')
            )
          
          # 3. If checking hier against vw_dim does not match anything, end;
          #    else, redo match_main but just with dim against vw_fact
          # Most similar: min(jaccard)
          if (nrow(match_hier) != 0) {
            
            match_main <-
              fuzzy_match(
                a = 
                  paste(dim_hier$dimension_name, collapse = ' ') %>% 
                  janitor::make_clean_names(),
                b = 
                  dfs_sql$fact %>% 
                  pmap(paste, collapse = ' ') %>% 
                  map(janitor::make_clean_names) %>% 
                  unlist,
                q = 3,
                method = 'jaccard'
              ) %>% { dfs_sql$fact[which(. == min(.)), ] } 
            
          } else { 
            
            match_hier <- match_hier[1, ] 
            
            # 1. Match with both dim and hier against vw_fact using jaccard.
            #    View name is used, so tokenize view and col and use word
            #    level similarity.
            match_main <-
              fuzzy_match(
                a = 
                  dim_hier[2:3],
                # paste(dim_hier[2:3], collapse = ' ') %>%
                # janitor::make_clean_names(),
                b = 
                  dfs_sql$fact %>% 
                  # filter(str_detect(column_name, 'classification_gr')) %>% 
                  # filter(str_detect(view_name, 'case')) %>%
                  # filter(str_detect(view_name, 'phs_inv')) %>% 
                  pmap(paste, collapse = ' ') %>% 
                  map(janitor::make_clean_names) %>% 
                  unlist,
                q = qgram,
                method = 'jaccard',
                dict = dict,
                consider_word = T
              ) %>% 
              { dfs_sql$fact[which(. ==  min(.)), ] }
              # { dfs_sql$fact[which(. ==  min(.) & . < 0.4243), ] }
            
          }
          
          match_final <- 
            list(match_main, match_hier) %>% 
            discard(is.null) %>% 
            reduce(bind_cols)
          
          if (nrow(match_final) == 0) {
            
            match_final <- match_final[1, ]
            cat('No match', '\n')
            
          } else {
            
            cat(
              do.call(
                sprintf,
                append(
                  list('Matched: [%s].[%s]<->[%s].[%s]'),
                  match_final
                )
              ),
              '\n'
            )
            
          }
          
          bind_cols(as_tibble(dim_hier[2:3]), match_final)
          
        }
      )
      
    }
)

df_match_dim <-
  df_match %>%
  split(!is.na(.$dim_view_name)) %>%
  set_names(c('ready', 'need_joining')) %>%
  {

    dfs <- .
    rename(
      dfs$need_joining,
      view_join_key = column_name,
      group_by_var  = dim_column_name,
    ) %>%
      left_join(
        select(map_join_keys, -schema_name),
        by =
          c('view_name'     = 'view_name',
            'view_join_key' = 'view_key',
            'dim_view_name' = 'join_view')
      ) %>%
      filter(!is.na(join_key)) %>%
      bind_rows(
        janitor::remove_empty(
          rename(dfs$ready, group_by_var = column_name), 
          'cols'
        ),
        .
      ) %>%
      mutate(schema_name = schema, .before = 1) %>% 
      mutate(
        group_by_var =
          pmap_chr(
            list(group_by_var, view_join_key, join_key),
            \(group_by_var, view_join_key, join_key) {
              
              if (identical(group_by_var, join_key)) {
                
                return(view_join_key) 
                
              } else { 
                
                return(group_by_var) 
                
              }
              
            }
          )
      )

  } %T>%
  { write_csv(., paste0(name_comparison, '_full_map_with_vw_dims.csv')) }

## Add count keys for measures ----
map_final <-
  df_match_dim %>% 
  filter(!is.na(view_name)) %>% 
  left_join(
    map_cube$ctrl %>% 
      select(measure_name, dimension_name, hierarchy_name) %>% 
      distinct
  ) %>% 
  left_join(
    rename(map_count_keys, count_key = column_name),
    by =
      c('measure_name' = 'measure_name',
        'view_name'    = 'view_name')
  ) %>% 
  drop_na(count_key)

map_final <- 
  map_final %>% 
  filter(
    !hierarchy_name %in% 
      c(
        "Disease Event ID",
        "AE ID",
        "TE ID",
        "Answer Row ID"   ,
        "Question Sort ID"
      )
  )
