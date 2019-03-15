#' @title Consensus Cluster Results in a Table
#' 
#' @description 
#' \code{consClustResTable} presents a table of the consensus cluster results.
#' The table presents the results of each iteration of the 
#' \code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}} algorithm, the
#' cluster consensus of each cluster and the list of the cluster elements with
#' their corresponding item consensus. The item consensus is taken with respect
#' to the variable's cluster allocation.
#' 
#' @param neatmap_res Output from the \code{\link{neatmap}} function.
#'   
#' @author Philippe Boileau , \email{philippe_boileau@@berkeley.edu}
#'   
#' @export
#' 
#' @references For more information on the consensus cluster and item consensus
#' statistics, see
#' \href{https://link.springer.com/article/10.1023\%2FA\%3A1023949509487}{Monti et al.}. 
#' 
#' @importFrom dplyr filter_at filter all_vars select left_join transmute pull
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
#' # get the consensus cluster results for each iteration
#' consensus_res_df <- consClustResTable(neat_res)
consClustResTable <- function(neatmap_res) {
  
  # only consider the second list from the neatmap output
  neatmap_res <- neatmap_res[[2]]
  
  # get the consensus cluster results
  cc_res <- calcICLNoPlots(neatmap_res)
  
  # extract the clusterConsensus dataframe
  clust_consensus_df <- cc_res$clusterConsensus %>% as.data.frame
  
  # extract the itemConsensus dataframe
  item_consensus_df <- cc_res$itemConsensus %>% as.data.frame
  
  # determine the variable classification of each iteration of the algorithm
  # and create a dataframe with a column listing the variables and a column 
  # listing the clustering results
  num_iters <- length(neatmap_res)
  clusters_df <- data.frame(
    "variables" = names(neatmap_res[[2]]$consensusClass)) 
  
  for(i in 2:num_iters){
    
    # iteration column name
    col_name <- paste0("cc_", i, "k")
    
    # add the clustering results to the data frame
    clusters_df[[col_name]] <- unname(neatmap_res[[i]]$consensusClass)
  }
  
  # add the list of the variables in each cluster of the clusterConsensus df
  var_list_vect <- c()
  for(i in 2:num_iters){
    for(j in 1:i){
      
      # extract the column of variables in cluster j of iteration i
      var_col <- clusters_df %>% 
                   dplyr::filter_at(i, dplyr::all_vars(. == j)) %>% 
                   dplyr::select(variables)
      
      # extract the item consensus in cluster j of iteration i
      item_cons_cols <- item_consensus_df %>% 
                          dplyr::filter(k == i, cluster ==j) %>% 
                          dplyr::select(item, itemConsensus)
      
      # join the var column with the item consensus column and join columns
      var_item_cons <- var_col %>% 
                         dplyr::left_join(item_cons_cols, 
                                          by = c("variables" = "item")) %>%
                         dplyr::transmute(variables = paste0(variables, " (",
                                          round(itemConsensus, 3),
                                          ")")) %>%
                         dplyr::pull() %>%
                         paste(collapse = ", ")
                         
      # add the list to the clust_consensus df
      var_list_vect <- c(var_list_vect, var_item_cons)
    }
  }
  
  # add the list of variables in each cluster and their cluster consensus to the 
  # consensus cluter df
  clust_consensus_df$variables <- var_list_vect
  
  # return the cluster consensus dataframe
  return(clust_consensus_df)
}