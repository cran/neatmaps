#' @title Create Networks Using Edge Data Frame
#' 
#' @description 
#' \code{createNetworks} creates Igraph network objects using an edge data
#'   frame. This is important for computing structural properties of the 
#'   networks to be explored by \code{\link{neatmap}}.
#' 
#' @param edge_df A data frame where each row represents a different network and
#'   where each column represents a potential edge between node A and node B.
#'   The column names should be of the form "XA_B", where A and B are the node
#'   numbers in the network. If Node A or B do not exist in the specific
#'   network, the cell should have a value of NA. If there is no edge between A
#'   and B, place a value of 0. Avoid redundant column names since all edges are
#'   assumed to be undirected, e.g. avoid "XA_B" and "XB_A".
#'
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#' 
#' @importFrom igraph graph_from_edgelist
createNetworks <- function(edge_df) {
  
  # make sure entered values are of the right type
  if(class(edge_df) != "data.frame")
    stop("Please make sure that the arguments are of the correct type")

  # initialize the network list
  network_list <- list()
  
  # get the names of the columns in the edge_df data frame
  edges <- colnames(edge_df)
  
  # loop though all the networks in the data frame
  for(r in 1:nrow(edge_df)){
    
    # initialize the matrix to hold the edge list
    edge_list <- matrix(ncol = 2)
    
    # loop through all the columns in the df
    for(c in 1:ncol(edge_df)){
      
      # check if an edge exists between nodes 
      if(is.na(edge_df[r, c]) == FALSE && edge_df[r, c] != 0)
        edge_list <- rbind(edge_list,
                          matrix(as.numeric(
                            strsplit(gsub("X", "", edges[c]), "_")[[1]]),
                                 ncol = 2))
    }
    
    # remove the NA row
    edge_list <- matrix(edge_list[-1, ], ncol = 2)
    
    # get a graph from the edge list and add it to the network list
    network_list <- append(network_list, 
                           list(igraph::graph_from_edgelist(edge_list)))
  }
  
  return(network_list)
  
}