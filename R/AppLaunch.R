#' Run the MonaRk App
#'
#' This function calls runApp on the directory containing all app files.
#'
#' @export
launchApp <- function() {
  appDir <- path.package("MonaRk", quiet = FALSE)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}