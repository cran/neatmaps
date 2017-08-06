#' @title Plot Dendrogram
#' 
#' @description 
#' \code{plotDendrogram} plots the dendrogram of the results of the analysis
#'   performed by the neatmap function.
#' 
#' @param dend The dendrogram object to plot.
#' @param results The results of the hierarchical clustering analysis performed
#'   on the data.
#' @param labelsCex The font size of the labels of the dendrogram.
#' @param pCex The font size of the p-value labels of the dendrogram.
#' @param pAlpha The level of significance chosen to detect significant clusters.
#' @param dendTitle The title of the dendrogram plot.
#' @param showSign A boolean indicating whether or not to add the p-values to the
#'   dendrogram.
#'   
#' @author Phil Boileau , \email{philippe.boileau@mail.concordia.ca}
#'   
#' @export
#' @import dendextend
#' @importFrom pvclust pvrect
#' @importFrom graphics plot
#' @importFrom graphics text
#'   
#' @return The dendrogram with the statistical analysis provided 
#'   by \link{pvclust}.
#' 
#' @examples
#' df <- netsDataFrame(net.attr.df = networkAttrDF,
#'                     node.attr.df = nodeAttrDF,
#'                     edge.df = edgeDF)
#' results <- neatmap(df, scale.df = "basic", mainTitle = "Heatmap", 
#'                     xlabel = "Chararacteritics", ylabel = "Networks",
#'                     link.method = "single", dist.method = "euclidean",
#'                     nBootRep = 10)
#' dendrogram <- results[[1]]
#' pvclustResults <- results[[3]]
#' dend <- plotDendrogram(dend = dendrogram, results = pvclustResults,
#'                        labelsCex = 0.5, pCex = 0.60, pAlpha = 0.95)
#'
plotDendrogram <- function(dend, results, labelsCex, pCex, pAlpha = 0.95, showSign = TRUE,
                           dendTitle = 
                             paste("Cluster Dendrogram with AU/BP values",
                                    "(%)\n AU Values Highlighted by Signif")){
  
  pDend <- dend
  res <- results
  dendextend::labels_cex(pDend) <- labelsCex
  
  if(showSign == TRUE){
    pDend %>% dendextend::pvclust_show_signif(results, show_type = "lwd", signif_type = "au") %>%
      graphics::plot(main = dendTitle)
    res %>% graphics::text(cex = pCex)
    res %>% pvclust::pvrect(alpha = pAlpha, border = 4)
  } else {
    pDend %>% graphics::plot(main = dendTitle)
    res %>% pvclust::pvrect(alpha = pAlpha, border = 4)
    
  }
  
}