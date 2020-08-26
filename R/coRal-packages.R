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

globalVariables(names = c("current", "i", "status", "x", "y"))

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE
