#' @title Consensus Cluster Plus without Plots
#' 
#' @description 
#'   \code{consensusClusterNoPlots} is a wrapper function for
#'   \code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}}that suppresses
#'   the creation of the plots that are created automatically.
#' 
#' @param df A dataframe of network attributes containing only numeric values.
#'   The columns of the dataframe should likely be normalized.
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
#'  
#'  @author Philippe Boileau , \email{philippe_boileau@@berkeley.edu}
#'  
#'  @importFrom ConsensusClusterPlus ConsensusClusterPlus
#'  @importFrom grDevices png dev.off
consensusClusterNoPlots <- function(df, link_method, dist_method,
                                    max_k, reps, p_var, p_net, cc_seed){
  
  # create the tempfile to save the plots in (this works for Unix and Windows)
  ff <- tempfile()
  grDevices::png(filename=ff)
  
  # pass args to ConsensusClusterPlus
  res <- suppressMessages(ConsensusClusterPlus(as.matrix(df),
                                               maxK = max_k,
                                               innerLinkage = link_method,
                                               reps = reps,
                                               pItem = p_var,
                                               pFeature = p_net,
                                               clusterAlg = "hc",
                                               distance = dist_method,
                                               seed = cc_seed,
                                               plot = NULL))
  
  # delete the tempfile containing the plots
  grDevices::dev.off()
  unlink(ff)
  
  return(res)
}