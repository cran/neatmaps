#' @title Structural Attributes of Networks Data Frame
#' 
#' @description 
#' \code{getStructureAttr} produces a data frame of the structural attributes
#'   of a list of networks.
#'   
#' @param net_list A list of Igraph network objects that represent the collection
#'   of networks.
#'   
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#' 
#' @import igraph
getStructureAttr <- function(net_list){
  
  # make sure that the argument is a list of igraph objects
  if(class(net_list) != "list" ||
     sum(sapply(net_list, class) == "igraph") != length(net_list))
    stop("Please enter appropriate data as an argument.")
  
  # intialize the data frame that will contain all the information on
  # the structural characteristics of the data
  list_len <- length(net_list)
  net_id <- 1:list_len
  df <- as.data.frame(net_id)
  
  # calculate the degree
  df$net_degree <- sapply(1:list_len, function(x) vcount(net_list[[x]]))
  # calculate the density
  df$density <- sapply(1:list_len, function(x) graph.density(net_list[[x]]))
  # count the number of components
  df$components <- sapply(1:list_len, 
                          function(x) count_components(net_list[[x]]))
  # calculate the number of edges
  df$edge_count <- sapply(1:list_len, function(x) ecount(net_list[[x]]))
  # calculate the mean node degree
  df$mean_node_deg <- (df$edge_count * 2) / df$net_degree
  # calculate the constraint of the graphs
  df$constraint <- sapply(1:list_len, 
                          function(x){
                            sum(constraint(net_list[[x]]), na.rm = TRUE) /
                            df$net_degree[x]
                          })
  # calculate the hierarchy of the graphs
  df$hierarchy <- sapply(1:list_len, function(x) hierarchy(net_list[[x]]))
  
  # remove the ego ID
  df$net_id <- NULL
  
  return(df)
  
}