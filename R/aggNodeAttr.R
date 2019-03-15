#' @title Node Attribute Aggregater
#' 
#' @description 
#' \code{aggNodeAttr} creates a data frame that summarizes node attributes.
#' 
#' @param node_df A data frame containing all the characteristics of the nodes
#'  in the network. If there are n networks, a maximum of x nodes per network
#'  and y variables for each node, the data frame should have n rows and x*y
#'  columns. The column names of each variable should be written as follows: 
#'  var1, var2, ... , varX.
#' @param measure_of_cent A vector that contains the measures of centrality with
#'   which to summarize the node attributes. The supported measures are "mean"
#'   and "median". Note that missing values are excluded from the calculations.
#'   
#' @author Philippe Boileau , \email{philippe_boileau@@berkeley.edu}
#'   
#' @importFrom stats median
#' 
aggNodeAttr <- function(node_df, measure_of_cent = "mean"){
  
  # make sure that the arguments entered are of the right format
  if(class(node_df) != "data.frame" ||
     (("mean" %in% measure_of_cent) == FALSE) &&
      ("median" %in% measure_of_cent) == FALSE)
    stop("Please enter appropriate arguments.")
  measure_of_cent
  
  # get all the column names in the df
  var_names <- colnames(node_df)
  
  # remove all the numbers from the variable names, and remove all duplicates
  var_names <- gsub("[0-9]+$", "", var_names)
  var_names <- unique(var_names)
  
  # initialize the matrix of the summarized node attributes
  df <- matrix(nrow = nrow(node_df), 
               ncol = length(var_names)*length(measure_of_cent))
  
  # fill in each element of the matrix
  for(j in 1:length(var_names)){
    for(i in 1:nrow(node_df)) {
      
      # get the vector of elements to be used in calculation
      node_data <- c()
      for(each in grep(var_names[j], colnames(node_df)))
        node_data <- c(node_data, node_df[i, each])
      
      # for each measure of centrality, add the data to the df
      if("mean" %in% measure_of_cent && "median" %in% measure_of_cent){
        
        df[i, j] <- mean(node_data, na.rm = TRUE)
        df[i, j + length(var_names)] <- stats::median(node_data, na.rm = TRUE)
        
      } else if ("mean" %in% measure_of_cent)
        df[i, j] <- mean(node_data, na.rm = TRUE)
      else
        df[i,j] <- median(node_data, na.rm = TRUE)

    }
  }
  
  # turn the df matrix into a data frame
  df <- as.data.frame(df)
  
  # add the column names to the df
  if("mean" %in% measure_of_cent && "median" %in% measure_of_cent){
    
    df_colnames <- c(
      sapply(1:length(var_names),
             function(x) paste(var_names[x], "mean", sep = ".")),
      sapply(1:length(var_names),
             function(x) paste(var_names[x], "median", sep = ".")))
    colnames(df) <- df_colnames
    
  } else if ("mean" %in% measure_of_cent){
    df_colnames <- sapply(1:length(var_names), 
                           function(x) paste(var_names[x], "mean", sep = "."))
    colnames(df) <- df_colnames
  }
  else{
    df_colnames <- sapply(1:length(var_names),
                          function(x) paste(var_names[x], "median", sep = "."))
    colnames(df) <- df_colnames
  }
  
  # return the dataframe of aggreagted node attributes
  return(df)
}