#' @title Hierarchy
#' 
#' @description
#' \code{hierarchy} calculates the hierarchy of a network
#' 
#' @param net An igraph object representing a network
#' 
#' @author Phil Boileau , \email{philippe.boileau@mail.concordia.ca}
#' 
#' @importFrom igraph constraint vcount ecount
hierarchy <- function(net){
  
  # get the constraints of the network, their sum and the number of nodes
  cons <- constraint(net)
  cons[is.nan(cons)] <- 0
  sumCons <- sum(cons)
  neigh <- vcount(net)
  
  # calculate the vectors needed to perform the calculation
  v1 <- cons / (sumCons / neigh)
  v2 <- log(cons/ (sumCons / neigh))
  v2[is.infinite(v2)] <- 0
  h <- sum(v1 * v2) / (neigh * log(neigh))
  
  if(ecount(net) == 0)
    h <- 0
  
  return(h)
  
}