#' Change in Area Under the ECDF
#'
#' @description
#' \code{consensusChangeECDF} plots the relative change in area under empirical 
#' cumulative distribution function for consecutive consensus cluster matrices
#' produced using the \code{\link{neatmap}} function.
#'
#' @param neatmap_res Output from the \code{\link{neatmap}} function.
#'
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu} 
#'
#' @references For more information on the consensus matrices, see
#' \href{https://link.springer.com/article/10.1023\%2FA\%3A1023949509487}{Monti et al.}.
#'
#' @return A ggplot of the change in consecutive area under the ECDFs of the 
#' consensus cluster matrices.
#' 
#' @export
#'
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
#' # visualize the relative change in AU ECDF of consecutive consensus cluster 
#' # iterations
#' consensusChangeECDF(neat_res)
consensusChangeECDF <- function(neatmap_res){
  
  # only consider the second list from the neatmap output
  neatmap_res <- neatmap_res[[2]]
  
  # the vector of area's under the ecdf for each iteration of the algorithm
  auc_vec <- rep(NA, length(neatmap_res) - 1)
  
  # compute the auc for each iteration of the algorithm
  for(i in 2:length(neatmap_res)){
    
    # flatten the consensus matrix and 
    values <- as.vector(neatmap_res[[i]]$consensusMatrix)
    
    # compute the ecdf of the consensus matrix
    Fn <- stats::ecdf(values)
    
    # remove any ties and order
    values <- unique(values)[order(unique(values))]
    
    # pass it in to the ecdf
    ecdf_vals <- Fn(values)
    
    # compute the area under the cdf
    upper <- values[2:length(values)]
    width <- upper-values[1:(length(values)-1)]
    rect <- width*Fn(values)[1:(length(values)-1)]
    auc_vec[i-1] <- sum(rect)
  }
  
  # compute the relative change in the auc_vec, adding the auc of the first
  # element as the first term in the relative change vector 
  rel_change <- c(auc_vec[1], diff(auc_vec)/(auc_vec[1:(length(auc_vec)-1)]))
  
  # create the data frame of the results
  iter <- seq(2, length(neatmap_res))
  df <- data.frame(iter, rel_change)
  
  # create the scree plot
  p <- ggplot(df, aes(x = iter, y = rel_change)) +
    geom_line() + geom_point() + 
    ylab("Relative change") + xlab("Number of clusters") + 
    ggtitle(paste0("Relative Change in Area Under the ECDF of \n",
                   "Consecutive Consensus Cluster Iterations")) +
    theme_minimal()
  
  return(p)
}