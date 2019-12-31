#' Archive or unarchive assignments on the server
#'
#' code{archive_asgmts} archives the assignments corresponding to the ID number(s)
#' provided.
#' code{unarchive_asgmts} unarchive the assignments corresponding to the ID number(s)
#' provided.
#'
#' @param ids Assignment IDs to archive or unarchive. This accepts a vector so multiple
#' assignments can be archived or unarchived at once.
#' @param server Prefix for the survey server. It is whatever comes before
#' mysurvey.solutions: [prefix].mysurvey.solutions.
#' @param user Username for the API user on the server.
#' @param password Password for the API user on the server.
#'
#' @export
#' @rdname archive_asgmts
#'
#' @return Assignments corresponding to the ID number provided are archived or
#' unarchived.
#' @examples
#' ## archive assignments with ID numbers 4, 18, 195
#' \dontrun{
#' archive_asgmts(ids = c(4, 18, 195), server = "lf2018",
#' user = "APIuser2018", password = "SafePassword123")
#' }
#' ## unarchive assignments with ID numbers 4, 18, 195
#' \dontrun{
#' unarchive_asgmts(ids = c(4, 18, 195), server = "lf2018",
#' user = "APIuser2018", password = "SafePassword123")
#' }

archive_asgmts <- function(ids = NULL, server = "", user = "", password = "")
{
  # -------------------------------------------------------------
  # CHECK ALL INPUTS
  # -------------------------------------------------------------

  # check that server, login, password, and data type are non-missing
  for (x in c("server", "user", "password")) {
    if (!is.character(get(x))) {
      stop(x, "has to be a string.")
    }
    if (nchar(get(x)) == 0) {
      stop(paste("The following parameter is not specified in the program:", x))
    }
  }

  # check if assignment IDs were provided
  if (is.null(ids)){
    stop("Assignment IDs to archive need to be specified.")
  }

  # check if all assignment IDs are numeric
  if (sum(sapply(suppressWarnings({as.numeric(ids)}), is.na)) > 0){
    stop("Assignment IDs must be a number.")
  } else {
    # if all are numeric, convert to numeric vector
    ids <- as.numeric(ids)
  }

  # build base URL for API
  server <- tolower(trimws(server))

  # check server exists
  server_url <- paste0("https://", server, ".mysurvey.solutions")

  # Check server exists
  tryCatch(httr::http_error(server_url),
           error=function(err) {
             err$message <- paste(server, "is not a valid server.")
             stop(err)
           })

  # -------------------------------------------------------------
  # Send API request
  # -------------------------------------------------------------

  # build base URL for API
  api_url <- paste0(server_url, "/api/v1")

  # function archive one assignment
  archive_id <- function(x, url=api_url){
    # build api endpoint
    endpoint <- paste0(url, "/assignments/", x, '/archive')

    resp <- httr::PATCH(endpoint, httr::authenticate(user, password))

    if (httr::status_code(resp)==200){
      message("Successfully archived assignment #", x)
    } else if (httr::status_code(resp)==401){
      stop("Invalid login or password.")
    } else {
      message("Error archiving assignment #", x)
    }
  }

  # archive all assignments in list
  # invisible to prevent sapply from printing to console
  invisible(sapply(ids, archive_id))
}

#' @export
#' @rdname archive_asgmts

unarchive_asgmts <- function(ids = NULL, server = "", user = "", password = "")
{
  # -------------------------------------------------------------
  # CHECK ALL INPUTS
  # -------------------------------------------------------------

  # check that server, login, password, and data type are non-missing
  for (x in c("server", "user", "password")) {
    if (!is.character(get(x))) {
      stop(x, "has to be a string.")
    }
    if (nchar(get(x)) == 0) {
      stop(paste("The following parameter is not specified in the program:", x))
    }
  }

  # check if assignment IDs were provided
  if (is.null(ids)){
    stop("Assignment IDs to unarchive need to be specified.")
  }

  # check if all assignment IDs are numeric
  if (sum(sapply(suppressWarnings({as.numeric(ids)}), is.na)) > 0){
    stop("Assignment IDs must be a number.")
  } else {
    # if all are numeric, convert to numeric vector
    ids <- as.numeric(ids)
  }

  # build base URL for API
  server <- tolower(trimws(server))

  # check server exists
  server_url <- paste0("https://", server, ".mysurvey.solutions")

  # Check server exists
  tryCatch(httr::http_error(server_url),
           error=function(err) {
             err$message <- paste(server, "is not a valid server.")
             stop(err)
           })

  # build base URL for API
  api_url <- paste0(server_url, "/api/v1")

  # -------------------------------------------------------------
  # Send API request
  # -------------------------------------------------------------

  # function archive one assignment
  unarchive_id <- function(x, url=api_url){
    # build api endpoint
    endpoint <- paste0(url, "/assignments/", x, '/unarchive')

    resp <- httr::PATCH(endpoint, httr::authenticate(user, password))

    if (httr::status_code(resp)==200){
      message("Successfully unarchived assignment #", x)
    } else if (httr::status_code(resp)==401){
      stop("Invalid login or password.")
    } else {
      message("Error unarchiving assignment #", x)
    }
  }
  # unarchive all assignments in list
  # invisible to prevent sapply from printing to console
  invisible(sapply(ids, unarchive_id))
}
