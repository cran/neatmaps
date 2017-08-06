#' @title Structural Attributes of Networks Data Frame
#' 
#' @description 
#' \code{getStructureAttr} produces a data frame of the structural attributes
#'   of a list of networks.
#'   
#' @param netList A list of Igraph network objects that represent the collection
#'   of networks.
#'   
#' @author Phil Boileau , \email{philippe.boileau@mail.concordia.ca}
#' 
#' @import igraph
getStructureAttr <- function(netList){
  
  # make sure that the argument is a list of igraph objects
  if(class(netList) != "list" ||
     sum(sapply(netList, class) == "igraph") != length(netList))
    stop("Please enter appropriate data as an argument.")
  
  # intialize the data frame that will contain all the information on
  # the structural characteristics of the data
  listLen <- length(netList)
  netID <- 1:listLen
  df <- as.data.frame(netID)
  
  # calculate the degree
  df$netDegree <- sapply(1:listLen, function(x) vcount(netList[[x]]))
  # calculate the density
  df$density <- sapply(1:listLen, function(x) graph.density(netList[[x]]))
  # count the number of components
  df$components <- sapply(1:listLen, function(x) count_components(netList[[x]]))
  # calculate the number of edges
  df$edgeCount <- sapply(1:listLen, function(x) ecount(netList[[x]]))
  # calculate the mean node degree
  df$meanNodeDegree <- (df$edgeCount * 2) / df$netDegree
  # calculate the constraint of the graphs
  df$constraint <- sapply(1:listLen, function(x) sum(constraint(netList[[x]]), na.rm = TRUE) /
                            df$netDegree[x])
  # calculate the hierarchy of the graphs
  df$hierarchy <- sapply(1:listLen, function(x) hierarchy(netList[[x]]))
  
  # remove the ego ID
  df$netID <- NULL
  
  return(df)
  
}