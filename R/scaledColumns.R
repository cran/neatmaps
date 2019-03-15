#' @title Scale Between 0 and 1
#' 
#' @description 
#' \code{scaleColumns} scales the columns of a data frame object between the
#' values of 0 and 1 without changing the underlying distribution of the
#' columns.
#'   
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#'  
#' @param df The data frame of numerical values to be scaled.
scaleColumns <- function(df){
  
  scaled_df <- as.data.frame(
    apply(df, 2,
          function(x){
            (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    })
  )
  
  return(scaled_df)
  
}   