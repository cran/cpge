#' Run Shiny Interactive graph
#'
#' Launch the interactive Shiny graph.
#'
#' @return a graph clustered by different exams and sectors of activity
#'  for French selective engineering schools and selective higher education
#'  institutions like Ecoles Normales Superieures or specialized university programs.
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import utils
#' @export run_graph
run_graph <- function() {

#   # chemin vers le fichier Quarto
   file <- system.file("quarto", "cpge_fr.qmd", package = "cpge")
   if (!file.exists(file)) stop("Quarto file not found in the package")
   quarto::quarto_preview(file)

}



