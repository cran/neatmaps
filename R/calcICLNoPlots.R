#' @title Consensus Cluster Plus without Plots
#' 
#' @description 
#'   \code{consensusClusterNoPlots} is a wrapper function for
#'   \code{\link[ConsensusClusterPlus]{ConsensusClusterPlus}} that suppresses
#'   the creation of the plots that are created automatically.
#' 
#' @param consensus_results Results of consensus clustering. The second item
#'   in the list returned by \code{\link{neatmap}}.
#'  
#' @author Philippe Boileau , \email{philippe_boileau@@berkeley.edu}
#'  
#' @importFrom ConsensusClusterPlus calcICL
#' @importFrom grDevices png dev.off
#' 
calcICLNoPlots <- function(consensus_results){
  
  # create the tempfile to save the plots in (this works for Unix and Windows)
  ff <- tempfile()
  grDevices::png(filename=ff)
  
  # pass consensus cluser results to calcICL
  res <- calcICL(consensus_results)
  
  # delete the tempfile containing the plots
  grDevices::dev.off()
  unlink(ff)
  
  return(res)
}