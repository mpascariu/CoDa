
#' onAttach
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nCoDa       : Compositional Data Mortality Model",
                        "\nAuthor     : Marius D. Pascariu",
                        "\nLast Update: July 19, 2018\n")
}
