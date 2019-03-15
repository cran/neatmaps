#' @title Networks Data Frame
#' 
#' @description 
#' \code{netsDataFrame} produces data frames of collections of networks.
#' 
#' @details
#' The function produces data frames of collections of networks. The function
#' requires the input of three data frames: a data frame containing the graph
#' attributes, a data frame containing the node characteristics and a data frame
#' containing the edge list of each network. The rows in each of these data
#' frames must represent individual  networks, and must therefore have identical
#' row length. Measures of centrality used in the summarization of the node
#' attributes must also be furnished.
#' 
#' @param net_attr_df A data frame consisting of all of the networks' graph
#'   attributes. The first column should contain the name of the network, and 
#'   all other columns should be numeric. All empty entries should be filled as
#'   "NA".
#' @param node_attr_df A data frame consisting of all of the networks' nodes'
#'   attributes. All columns should be numeric. All empty entries should be 
#'   filled in as "NA".
#' @param edge_df A data frame consisting of the edge matrix for each ego
#'   network. Edges are assumed to be undirected and unweighted. 1 indicates the
#'   existence of an edge between nodes, 0 indicates the lack of an edge.
#' @param cent_measure A vector of the measures of centrality to be used for the
#'   summary of the node attributes data. The supported measures of centrality
#'   are: "mean" and "median".
#'   
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#'   
#' @export
#' @return The function returns a data frame that offers an overview of all of
#' the ego networks.
#'   
#' @examples
#' df <- netsDataFrame(network_attr_df,
#'                     node_attr_df,
#'                     edge_df)

netsDataFrame <- function(net_attr_df, node_attr_df, edge_df,
                              cent_measure = c("mean")){

  # Make sure that only data frames are being passed in to the three first arguments
  if(class(net_attr_df) != "data.frame" || class(node_attr_df) != "data.frame" ||
     class(edge_df) != "data.frame" || nrow(net_attr_df) != nrow(node_attr_df) ||
     nrow(net_attr_df) != nrow(edge_df))
    stop("Please make sure tha net_attr_df, node_attr_df and edge_df 
         are dataframes of the same length.")
  
  
  # define the data frame to be returned, set initially to net_attr_df
  df <- net_attr_df
  
  
  # create the igraph network list of all the networks
  net_list <- createNetworks(edge_df)
  
  # get the structural characteristics of each of network
  structure_df <- getStructureAttr(net_list)
  
  # get the measures of centrality of the node characteristics
  node_measures_df <- aggNodeAttr(node_attr_df, cent_measure)
  
  
  # combine all three data frames to get the final df
  df <- cbind(df, structure_df, node_measures_df)
  
  
  # return the final product
  return(df)
  
}