#' @title Create Heatmaps of Consensus Matrices
#' 
#' @description 
#' \code{consensusMap} produces a list of heatmaps from the consensus matrices
#' produced during the consensus clustering step of the \code{\link{neatmap}}
#' function.
#' 
#' @details 
#' This function will create a list of heatmaps of the consensus matrices
#' produced during the consensus clustering step of the \code{\link{neatmap}}
#' function. The default clustering method used in the heatmaps is hierarchical
#' clustering using the average linkage method, though other linkage methods
#' can be used. The consensus cluster matrix is used as a measure of similarity.
#' The heatmaps are produced using \code{\link[heatmaply]{heatmaply}}.
#' 
#' @references For more information on the consensus matrices, see
#' \href{https://link.springer.com/article/10.1023\%2FA\%3A1023949509487}{Monti et al.}. 
#' 
#' @param neatmap_res Output from the \code{\link{neatmap}} function.
#' @param link_method The agglomeration method to be used for hierarchical 
#'   clustering. Defaults to the average linkage method. See other methods in
#'   \code{\link[stats]{hclust}}.
#' 
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#' 
#' @importFrom heatmaply heatmaply
#' @importFrom ggplot2 scale_fill_gradient2
#' 
#' @export
#'
#' @return Returns of a list of heatmaps depicting the consensus matrices of 
#' each 
#'
#' @examples
#' # create the data frame using the network, node and edge attributes
#' df <- netsDataFrame(network_attr_df,
#'                     node_attr_df,
#'                     edge_df)
#' 
#' # run the neatmap code on df
#' neat_res <- neatmap(df, scale_df = "ecdf", max_k = 3, reps = 100, 
#'                     xlab = "vars", ylab = "nets", xlab_cex = 1, ylab_cex = 1)
#'                     
#' # create the list of heatmaps for each iteration
#' hm_list <- consensusMap(neat_res)
consensusMap <- function(neatmap_res, link_method = "average"){
  
  # only consider the second list from the neatmap output
  neatmap_res <- neatmap_res[[2]]
  
  # create a list that contains the heatmaps
  hm_list <- list()
  
  # loop through each iteration of the consensus clustering
  for(i in 2:length(neatmap_res)){
    
    # create the heatmap for the ith iteration
    hm_list[[i-1]] <- heatmaply::heatmaply(
      neatmap_res[[i]]$consensusMatrix,
      hclust_method = link_method,
      symm = TRUE,
      seriate = "mean",
      RowSideColors = data.frame(" " = neatmap_res[[i]]$consensusClass,
                                 check.names = FALSE),
      showticklabels = FALSE,
      key.title = "Consensus",
      scale_fill_gradient_fun = 
        ggplot2::scale_fill_gradient2(low = "white", 
                                      high = "blue", 
                                      limits = c(0, 1))
      )
  }
  
  # return the vector of heatmaps
  return(hm_list) 
}