#' Consensus Matrix ECDFs
#' 
#' @description 
#' \code{consensusECDF} plots the empirical cumulative distribution functions
#' (ECDF) of the consensus matrices produced during the consensus clustering
#' step of the \code{\link{neatmap}} function.
#' 
#' @details 
#' This function visualizes the ECDFs of the consensus matrices for each each 
#' iteration of consensus clustering that is carried out as part of the 
#' \code{\link{neatmap}} function. 
#'
#' @param neatmap_res Output from the \code{\link{neatmap}} function.
#'
#' @author Philippe Boileau , \email{philippe_boileau@@berkeley.edu}
#'
#' @return Returns a ggplot depicting the ECDFs of each iteration of the
#' consensus clustering, i.e. one ECDF per number of clusters used in each
#' iteration.
#' 
#' @references For more information on the consensus matrices, see
#' \href{https://link.springer.com/article/10.1023\%2FA\%3A1023949509487}{Monti et al.}.
#' 
#' @export
#' 
#' @importFrom stats ecdf
#' @importFrom dplyr bind_rows
#' @import ggplot2
#'
#' @examples
#' #' # create the data frame using the network, node and edge attributes
#' df <- netsDataFrame(network_attr_df,
#'                     node_attr_df,
#'                     edge_df)
#' 
#' # run the neatmap code on df
#' neat_res <- neatmap(df, scale_df = "ecdf", max_k = 3, reps = 100, 
#'                     xlab = "vars", ylab = "nets", xlab_cex = 1, ylab_cex = 1)
#'                     
#' # create the ECDF plot
#' consensusECDF(neat_res)
consensusECDF <- function(neatmap_res){
  
  # only consider the second list from the neatmap output
  neatmap_res <- neatmap_res[[2]]
  
  # create a list to hold dataframes
  df_list <- list()
  
  # create a dataframe by flattening each consensus cluster matrix, getting the
  # ecdf associated with each entry and finally creating a new column
  # identifying the iteration to which it belongs
  for(i in 2:length(neatmap_res)){
   
    # flatten the consensus matrix and 
    values <- as.vector(neatmap_res[[i]]$consensusMatrix)
    
    # compute the ecdf of the consensus matrix
    Fn <- stats::ecdf(values)
    
    # remove any ties
    values <- unique(values)
    
    # pass it in to the ecdf
    ecdf_vals <- Fn(unique(values))
    
    # get the iterations label
    iter_lab <- rep(i, length(values))
    
    # create the dataframe and add it to the list of dataframes
    df_list[[i-1]] <- data.frame(values, ecdf_vals, iter_lab)
     
  }
  
  # bind the dataframe list into a single dataframe
  df <- dplyr::bind_rows(df_list)
  
  # create the ggplot
  p <- ggplot(df, aes(x = values, y = ecdf_vals, colour = factor(iter_lab))) +
    geom_line() + xlab("Consensus values") + ylab("ECDF") +
    scale_colour_discrete(name = "Number of clusters") +
    ggtitle("ECDF of Consensus Cluster Matrices") + theme_minimal()
  
  return(p)
}