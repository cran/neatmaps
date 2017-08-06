#' @title Networks Data Frame
#' 
#' @description 
#' \code{netsDataFrame} produces data frames of collections of networks.
#' 
#' @details
#' The function produces data frames of collections of networks. The function
#' requires the input of three data frames: a data frame containing the graph
#' attributes, a data frame containing the node characteristics and a data frame
#' containing the edgelist of each network. The rows in each of these data frames
#' must represent individual  networks, and must therefore have identical row
#' length. Measures of centrality used in the summarization of the node attributes
#' must also be furnished.
#' 
#' @param net.attr.df A data frame consisting of all of the networks' graph
#'   attributes. The first column should contain the name of the network, and all
#'   other columns should be numeric. All empty entries should be filled as "NA".
#' @param node.attr.df A data frame consisting of all of the networks' nodes'
#'   attributes. All columns should be numeric. All empty entries should be filled
#'   in as "NA".
#' @param edge.df A data frame consisting of the edge matrix for each ego network.
#'   Edges are assumed to be undirected and unweighted. 1 indicates the existence 
#'   of an edge between nodes, 0 indicates the lack of an edge.
#' @param measuresOfCent A vector of the measures of centrality to be used for the
#'   summary of the node attributes data. The supported measures of centrality
#'   are: "mean" and "median".
#'   
#' @author Phil Boileau , \email{philippe.boileau@mail.concordia.ca}
#'   
#' @export
#' @return The function returns a data frame that offers an overview of all of the
#'   ego networks.
#'   
#' @examples
#' df <- netsDataFrame(net.attr.df = networkAttrDF,
#'                     node.attr.df = nodeAttrDF,
#'                     edge.df = edgeDF)

netsDataFrame <- function(net.attr.df, node.attr.df, edge.df,
                              measuresOfCent = c("mean")){

  # Make sure that only data frames are being passed in to the three first arguments
  if(class(net.attr.df) != "data.frame" || class(node.attr.df) != "data.frame" ||
     class(edge.df) != "data.frame" || nrow(net.attr.df) != nrow(node.attr.df) ||
     nrow(net.attr.df) != nrow(edge.df))
    stop("Please make sure tha net.attr.df, node.attr.df and edge.df 
         are dataframes of the same length.")
  
  
  # define the data frame to be returned, set initially to net.attr.df
  df <- net.attr.df
  
  
  # create the igraph network list of all the networks
  netList <- createNetworks(edge.df)
  
  # get the structural characteristics of each of network
  structure.df <- getStructureAttr(netList)
  
  # get the measures of centrality of the node characteristics
  node.measures.df <- compactNodeAttr(node.attr.df, measuresOfCent)
  
  
  # combine all three data frames to get the final df
  df <- cbind(df, structure.df, node.measures.df)
  
  
  # return the final product
  return(df)
  
}