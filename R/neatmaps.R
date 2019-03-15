#' \code{neatmaps} package
#'
#' A package for exploring multi-network data.
#'
#' See the README on
#' \href{ https://CRAN.R-project.org/package=neatmaps}{CRAN}
#' or \href{https://github.com/PhilBoileau/neatmaps/tree/master}{GitHub}
#'
#' @author Philippe Boileau, \email{philippe_boileau@@berkeley.edu}
#'
#' @docType package
#' @name neatmaps
NULL

# quiets concerns of R CMD check re: the .'s that appear in pipelines
# these global vars are in the consClustResTable.R file
silence_note <- c(".", "all_vars", "cluster", "item", "k", "variables",
                  "itemConsensus")
if(getRversion() >= "2.15.1")  utils::globalVariables(silence_note)