# -*- coding: UTF-8 -*-
#' Prepare graph data
#'
#' Creates the nodes, edges objects and the lists of schools, fields and selective exams required to build the interactive graph.
#' You can select on the graph or from the drop-down menu the sector activities corresponding to chosen school for example, visualize which school belongs to a chosen group exam"
#'
#'
#' @return A list containing:
#' \describe{
#'   \item{nodes}{A data frame describing graph nodes.}
#'   \item{edges}{A data frame describing graph edges.}
#'   \item{only_domaine}{A data frame describing the different sectors of activity.}
#'    \item{only_ecoles}{A data frame describing the different French schools.}
#'     \item{only_concours}{A data frame describing the different French selective exams for engineering and higher education
#'  institutions.}
#' }
#'
#' @examples
#' if(interactive()){
#' run_graph()
#' }
#' @import shiny
#' @import visNetwork
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import quarto
#' @export
run_graph <- function() {

#   # chemin vers le fichier Quarto
   file <- system.file("quarto", "cpge_fr.qmd", package = "cpge")
   if (!file.exists(file)) stop("Quarto file not found in the package")
   quarto::quarto_preview(file)

}



