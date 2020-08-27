#' Data table
#'
#' See \code{\link{set}} for details.
#'
#' @name :=
#' @rdname set
#' @keywords internal
#' @export
#' @importFrom data.table :=
NULL

globalVariables(names = c(
  "activity",
  "age",
  "consumption_req",
  "current",
  "growth_length",
  "growth_nutrient",
  "growth_weight",
  "i",
  "n_body",
  "reserves",
  "reserves_diff",
  "reserves_max",
  "respiration",
  "status",
  "weight",
  "x",
  "y"))

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE
