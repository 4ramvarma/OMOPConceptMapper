#' Escape SQL String Literals
#'
#' Helper function to safely escape single quotes in SQL string literals
#' by replacing them with double single quotes.
#'
#' @param x Character vector to escape
#'
#' @return Character vector with escaped single quotes
#' @keywords internal
.escape_sql_string <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

#' Validate CDM Schema Name
#'
#' Validates that the CDM schema name is a single non-empty string
#'
#' @param cdm_schema Character string representing the CDM schema
#'
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
.validate_cdm_schema <- function(cdm_schema) {
  if (!is.character(cdm_schema) || length(cdm_schema) != 1 || nchar(cdm_schema) == 0) {
    stop("cdm_schema must be a single non-empty character string")
  }
  return(TRUE)
}

#' Validate Connection Object
#'
#' Validates that a database connection object is not NULL
#'
#' @param connection DatabaseConnector connection object
#'
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
.validate_connection <- function(connection) {
  if (is.null(connection)) {
    stop("connection cannot be NULL. Please establish a database connection first.")
  }
  return(TRUE)
}
