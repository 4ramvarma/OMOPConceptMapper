#' Create and Establish Database Connection
#'
#' Creates connection details and establishes a connection to a Redshift database
#' using credentials from environment variables.
#'
#' @param server Character string. The server address. Defaults to SERVER_SERVERLESS env var.
#' @param username Character string. Database username. Defaults to USERNAME env var.
#' @param password Character string. Database password. Defaults to PASSWORD env var.
#' @param port Character string. Database port. Default is "5439".
#' @param path_to_driver Character string. Path to JDBC driver. Default is "~".
#'
#' @return A DatabaseConnector connection object
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- create_db_connection()
#' }
create_db_connection <- function(
    server = Sys.getenv("SERVER_SERVERLESS"),
    username = Sys.getenv("USERNAME"),
    password = Sys.getenv("PASSWORD"),
    port = "5439",
    path_to_driver = "~"
) {
  
  # Validate environment variables
  if (server == "" || username == "" || password == "") {
    stop("Required environment variables not set. Please set SERVER_SERVERLESS, USERNAME, and PASSWORD.")
  }
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "redshift",
    server = server,
    user = username,
    password = password,
    port = port,
    pathToDriver = path_to_driver
  )
  
  connection <- DatabaseConnector::connect(connectionDetails)
  message("Successfully connected to database")
  
  return(connection)
}