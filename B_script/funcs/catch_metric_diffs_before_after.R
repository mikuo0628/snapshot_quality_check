catch_metric_diffs_before_after <- function(
    df_before,
    df_after,
    use_new_cols = F
) {
  
  list(
    before = df_before,
    after  = df_after
  ) %>% 
    purrr::reduce(
      full_join,
      by = c('row', 'view', 'col', 'type'),
      suffix = paste0('_', names(.))
    ) %>% 
    tidyr::nest(
      df_compare  = dplyr::matches('df_compare'),
      col_check   = dplyr::matches('col_check'),
      val_check   = dplyr::matches('val_check'),
      missingness = dplyr::matches('missingness'),
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.list),
        ~ purrr::map(
          .x, rename_with, \(x) stringr::str_extract(x, '(before|after)')
        )
      )
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.list),
        ~ purrr::map_lgl(
          .x,
          \(df) {
            
            list_objs <- unlist(df, F)
            
            compare <- 
              try(
                
                if (all(purrr::map_lgl(list_objs, is_tibble))) {
                  
                  list_objs %>% 
                    purrr::map(
                      dplyr::mutate,
                      dplyr::across(tidyselect::everything(), as.character)
                    ) %>% 
                    purrr::reduce(dplyr::all_equal) %>% 
                    isTRUE
                  
                } else if (all(purrr::map_lgl(list_objs, is.null))) {
                  
                  list_objs %>% 
                    purrr::reduce(identical)
                  
                } else if (all(purrr::map_lgl(list_objs, is.list))) {
                  
                  all(
                    purrr::pmap_lgl(
                      list_objs,
                      \(before, after) {
                        
                        if (all(purrr::map_lgl(list(before, after), is_tibble))) {
                          
                          list(before, after) %>% 
                            purrr::map(
                              dplyr::mutate,
                              dplyr::across(
                                tidyselect::everything(), as.character
                              )
                            ) %>% 
                            purrr::reduce(dplyr::all_equal) %>% 
                            isTRUE
                          
                        } else {
                          
                          identical(before, after)
                          
                        }
                        
                      }
                    )
                  )
                  
                } else { return(F) }
                
              )
            
            if (inherits(compare, 'try-error')) browser()
            
            return(compare)
              
          }
        ),
        .names = if (use_new_cols) { '{.col}_lgl' } else NULL
      )
    )
  
}