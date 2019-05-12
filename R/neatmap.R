#' @title Explore Multi-Network Data
#'
#' @description
#' \code{neatmap} produces a heatmap of multi-network data and identifies stable
#' clusters in its variables.
#'
#' @details
#' This function allows users to efficiently explore their multi-network data
#' by visualizing their data with a heatmap and assessing the stability of the
#' associations presented within it. \code{neatmap} requires that the data 
#' frame be processed into an appropriate format prior to use. Data is then 
#' scaled (if necessary) using of the built in methods. See (list functions) for
#' further details on how to prepare multi-network data for use with 
#' \code{neatmap}. The heatmap is created using
#' \code{\link[heatmaply]{heatmaply}} and the consensus clustering is performed
#' using \code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}}
#' 
#' @references For more information on the consensus clustering, see
#' \href{https://link.springer.com/article/10.1023\%2FA\%3A1023949509487}{Monti et al.}. 
#' 
#' @param df a dataframe of network attributes containing only numeric values.
#' @param scale_df A string indicating whether the columns of the data frame
#'   should be scaled, and, if so, which method should be used. The options are
#'   "none", "ecdf", "normalize" and "percentize". If "none" is selected, then
#'   the columns are not scaled.  If "ecdf" is selected, then the columns are
#'   transformed into their empirical cumulative distribution. If "normalize" is
#'   selected, each column is centered to have a mean of 0 and scaled to have a
#'   standard deviation of 1. If "percentize" is selected, column values are 
#'   transformed into percentiles.
#' @param link_method The agglomeration method to be used for hierarchical 
#'   clustering. Defaults to the average linkage method. See other methods in
#'   \code{\link[stats]{hclust}}.
#' @param dist_method The distance measure to be used between columns and 
#'   between rows of the dataframe. Distance is used as a measure of similarity.
#'   Defaults to euclidean distance. See other options in 
#'   \code{\link[stats]{dist}}.
#' @param max_k The maximum number of clusters to consider in the consensus
#'   clustering step. Consensus clustering will be performed for max_k-1 
#'   iterations, i.e. for 2, 3, ..., max_k clusters. Defaults to 10.
#' @param reps The number of subsamples taken at each iteration of the consensus
#'   cluster algorithm. Defaults to 1000.
#' @param p_var The proportion of network variables to be subsampled during 
#'   consensus clustering. Defaults to 1. 
#' @param p_net The proportion of networks to be subsampled during consensus
#'   clustering. Defaults to 0.8.
#' @param cc_seed The seed used to ensure the reproducibility of the consensus 
#'   clustering. Defaults to 1. 
#' @param main_title The title of the heatmap.
#' @param xlab The x axis label of the heatmap.
#' @param ylab The y axis label of the heatmap.
#' @param xlab_cex The font size of the elements on the x axis.
#' @param ylab_cex The font size of the elements on the y axis.
#' @param heatmap_margins The size of the margins for the heatmap. 
#'   See \code{\link[heatmaply]{heatmaply}}.
#' 
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#'
#' @export
#' @importFrom heatmaply percentize heatmaply
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ConsensusClusterPlus ConsensusClusterPlus
#' 
#' @return A named list containing the heatmap of the multi-network data and a
#'   list of length max_k-1 where each element is a list containing the
#'   consensus matrix, the consensus hierarchical clustering results and the
#'   consensus class assignments. The list of results produced by the consensus
#'   clustering can be parsed using following functions in the
#'   \code{\link{neatmaps}} package: \code{\link{consClustResTable}},
#'   \code{\link{consensusECDF}} and \code{\link{consensusChangeECDF}}.
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
#' # extract the heatmap
#' heatmap <- neat_res$heatmap
#' 
#' # extract the consensus clustering results
#' consensus_res <- neat_res$consensus_clust
#' 
neatmap <- function(df, scale_df, link_method = "average", 
                    dist_method = "euclidean", max_k = 10,
                    reps = 1000, p_var = 1, p_net = 0.8, cc_seed = 100,
                    main_title  = "", xlab, ylab, xlab_cex = 1, ylab_cex = 1,
                    heatmap_margins = c(50, 50, 50, 100)){
  
  # check dataframe to make sure that it only contains numeric values
  if(FALSE %in% sapply(df, is.numeric))
    stop("Please input a dataframe that contains exclusively numeric values")
  
  # scale the data based on user selection
  if(scale_df == "ecdf")
    df <- scaleColumns(df)
  else if(scale_df == "normalize")
    df <- scale(df)
  else if(scale_df == "percentize")
    df <- heatmaply::percentize(df)
  
  # perform the consensus clustering on the scaled data frame
  results <- consensusClusterNoPlots(df,
                                     max_k = max_k,
                                     link_method = link_method,
                                     reps = reps,
                                     p_var = p_var,
                                     p_net = p_net,
                                     dist_method = dist_method,
                                     cc_seed = cc_seed)
  
  # create the heatmap
  hm <- heatmaply::heatmaply(df,
                             dist_method = dist_method,
                             hclust_method = link_method,
                             main = main_title,
                             seriate = "OLO",
                             xlab = xlab,
                             ylab = ylab,
                             margins = heatmap_margins,
                             scale_fill_gradient_fun = 
                               ggplot2::scale_fill_gradient2(low = "blue",
                                                             high = "red",
                                                             midpoint = 0.5),
                             cexRow = ylab_cex,
                             cexCol = xlab_cex)
  
  return(list(heatmap = hm,
              consensus_clust = results))
}
