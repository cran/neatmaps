#' @title Node Attribute Compactor
#' 
#' @description 
#' \code{compactNodeAttr} creates a data frame that summarizes node attributes.
#' 
#' @param node.df A data frame containing all the characteristics of
#'  the nodes in the network. If there are n networks, a maximum of x nodes
#'  per network and y variables for each node, the data frame should have n
#'  rows and x*y columns. The column names of each variable should be writen
#'  as follows: var1, var2, ... , varX.
#' @param measureOfCent A vector that contains the measures of centrality with
#'   which to summarize the node attributes. The supported measures are "mean"
#'   and "median".
#'   
#' @author Phil Boileau , \email{philippe.boileau@mail.concordia.ca}
#'   
#' @importFrom stats median
compactNodeAttr <- function(node.df, measureOfCent = "mean"){
  
  # make sure that the arguments entered are of the right format
  if(class(node.df) != "data.frame" ||
     (("mean" %in% measureOfCent) == FALSE) && ("median" %in% measureOfCent) == FALSE)
    stop("Please enter appropriate arguments.")
 
  # get all the column names in the df
  varNames <- colnames(node.df)
  # remove all the numbers from the variable names, and remove all duplicates
  varNames <- gsub("[0-9]+$", "", varNames)
  varNames <- unique(varNames)
  
  # initialize the matrix of the summarized node attributes
  df <- matrix(nrow = nrow(node.df), ncol = length(varNames) * length(measureOfCent))
  
  # fill in each element of the matrix
  for(j in 1:length(varNames)){
    for(i in 1:nrow(node.df)) {
      
      # get the vector of elements to be used in calculation
      nodeData <- c()
      for(each in grep(varNames[j], colnames(node.df)))
        nodeData <- c(nodeData, node.df[i, each])
      
      # for each measure of centrality, add the data to the df
      if("mean" %in% measureOfCent && "median" %in% measureOfCent){
        df[i, j] <- mean(nodeData, na.rm = TRUE)
        df[i, j + length(varNames)] <- stats::median(nodeData, na.rm = TRUE)
      } else if ("mean" %in% measureOfCent)
        df[i, j] <- mean(nodeData, na.rm = TRUE)
      else
        df[i,j] <- median(nodeData, na.rm = TRUE)

    }
  }
  
  # turn the df into a data frame
  df <- as.data.frame(df)
  # add the column names to the df
  if("mean" %in% measureOfCent && "median" %in% measureOfCent){
    colnames(df) <- c(sapply(1:length(varNames), function(x) paste(varNames[x], "mean", sep = ".")),
                      sapply(1:length(varNames), function(x) paste(varNames[x], "median", sep = ".")))
  } else if ("mean" %in% measureOfCent)
    colnames(df) <- sapply(1:length(varNames), function(x) paste(varNames[x], "mean", sep = "."))
  else
    colnames(df) <- sapply(1:length(varNames), function(x) paste(varNames[x], "median", sep = "."))
  
  
  return(df)
}