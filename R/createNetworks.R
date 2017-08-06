#' @title Create Networks Using Edge Data Frame
#' 
#' @description 
#' \code{createNetworks} creates Igraph network objects using an edge data frame
#' 
#' @param edge.df A data frame where each row represents a different
#'   network and where each column represents a potential edge between
#'   node A and node B. The column names should be of the form "XA_B",
#'   where A and B are the node numbers in the network. If Node A or B
#'   do not exist in the specific network, leave a value of NA. If there
#'   is no edge between A and B, place a value of 0. Avoid redundant 
#'   column names since all edges are assumed to be undirected, e.g. 
#'   "XA_B" and "XB_A".
#'
#' @author Phil Boileau , \email{philippe.boileau@mail.concordia.ca}
#' 
#' @importFrom igraph graph_from_edgelist
createNetworks <- function(edge.df) {
  
  # make sure entered values are of the right type
  if(class(edge.df) != "data.frame")
    stop("Please make sure that the arguments are of the correct type")

  # initialize the network list
  networkList <- list()
  # get the names of the columns in the edge.df df
  edges <- colnames(edge.df)
  
  # loop though all the networks in the df
  for(r in 1:nrow(edge.df)){
    
    # initialize the matrix to hold the edge list
    edgeList <- matrix(ncol = 2)
    
    # loop through all the columns in the df
    for(c in 1:ncol(edge.df)){
      
      # check if an edge exists between nodes 
      if(is.na(edge.df[r, c]) == FALSE && edge.df[r, c] != 0)
        edgeList <- rbind(edgeList, matrix(as.numeric(strsplit(gsub("X", "", edges[c]), "_")[[1]]), ncol = 2))
    }
    
    # remove the NA row
    edgeList <- matrix(edgeList[-1, ], ncol = 2)
    
    # get a graph from the edge list and add it to the network list
    networkList <- append(networkList, list(igraph::graph_from_edgelist(edgeList)))
  }
  
  return(networkList)
  
}