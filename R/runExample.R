
#' runExample
#'
#' @param example 
#'
#' @export
#'
#' @examples \dontrun{
#' DTshinyModule::runExample("01_simpleApp")
#' }
runExample <- function(example = "01_simpleApp") {
  examples <- paste(list.files(system.file("examples", 
                                           package = "DTshinyModule")))
  validExamples <- paste0("Valid examples are: \"", paste(examples, collapse = "\", \""), "\"")
  if (missing(example) || !nzchar(example)) {
    message("Please run `runExample()` with a valid example app as an argument.\n", 
            validExamples)
    return(invisible(NULL))
  }
  appDir <- system.file("examples", example, package = "DTshinyModule")
  shiny::runApp(appDir, display.mode = "normal")
}
