#' Declare non-standard evaluation variables
#'
#' These names are used inside ggplot2 aesthetics in the demo plots.
#'
#' @importFrom utils globalVariables
#' @keywords internal
#' @noRd
utils::globalVariables(c(
  "Expression",
  "Gene",
  "Sample",
  "group",
  "sd",
  "value",
  "x",
  "y"
))
