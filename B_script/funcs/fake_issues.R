fake_issues <- function(df, col = NULL, type, .remove_col = F) {
  
  if (.remove_col) return(NULL)
  
  if (type == 'float') {
    
    return(
      mutate(
        df,
        across(where(is.integer), ~ float(.x * runif(1, 0.90, 0.99))),
        across(where(is.double),  ~ .x * runif(1, 0.90, 0.99))
      )
    )
    
  } else if (type == 'date') {
    
    return(mutate(df, n = as.integer(n * runif(1, 0,9, 1.1))))
    
  }
  
  if (is.null(col)) {
    
    col <- 
      if (type == 'date') {
        
        names(df)[2]
        
      } else if (type %in% c('char', 'int')) {
        
        names(df)[1]
        
      }
    
  }
  
  try_insert <- 
    try(
      mutate(
        df,
        n_row = rownames(df),
        
        # add/remove some val
        across(
          !!sym(col),
          ~ case_when(
            .default = !!sym(col),
            nrow %in% 
              sample(
                nrow(df),
                max(c(floor(nrow(df) * 0.1), 1))
              ) & !is.logical(.x) ~
              sample(
                c(
                  NA,
                  ifelse(is.character(.x), '*FAKE_VAL*', T)
                ),
                1
              )
          )
        ),
        
        # fluctuate counts
        n =
          case_when(
            .default = n,
            n_row %in% 
              sample(
                nrow(df),
                max(c(floor(nrow(df) * 0.1), 1))
              ) ~ 
              as.integer(n - sapply(n * 0.9 * sample(c(1, -1), 1), sample, 1))
          )
      ) %>% 
        select(-n_row) %>% 
        group_by(!!!syms(names(.)[-length(.)])) %>% 
        summarise(n = sum(n, na.rm = T)) %>% 
        ungroup()
    )
  
  if (inherits(try_insert, 'try-error')) browser()
  
  return(try_insert)
  
}