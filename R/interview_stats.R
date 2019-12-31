#' Get information about an interview
#'
#' \code{interview_stats} gets information about an interview, including:
#' \itemize{
#'   \item Interview key
#'   \item Number of unanswered questions
#'   \item Number of answered questions
#'   \item Number of questions with errors
#'   \item Number of questions with comments
#'   \item Current status of interview
#'   \item Number of rejections by HQ
#'   \item Number of rejections by Supervisor
#'   \item Assignment Id that the interview was generated from
#' }
#'
#' @param ids Interview Id. The interview Id can have dashes or not.
#' This accepts a vector of interview Ids. In exported data, this will be the variable
#' \emph{interview__id}.
#' @param server Prefix for the survey server. It is whatever comes before
#' mysurvey.solutions: [prefix].mysurvey.solutions.
#' @param user Username for the API user on the server.
#' @param password Password for the API user on the server.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
#' @return A data frame with statistics on the interviews associated with
#' the interview IDs provided.
#' @examples
#' \dontrun{
#' ## Vector of IDs to get stats for
#' int_ids <- c("5970be83582840df972082c578816dba", "47748cf7aa0641a692a1fffc508a435c",
#' "61ef820e5bac466e829355abe42bc137", "63053caafa424a8d993cc22be2c9baf0")
#'
#' ## Get statistics for those interviews
#' interview_stats(ids = int_ids, server = "lfs2018",
#' user = "APIuser2018", password = "SafePassword123")
#' }

interview_stats <- function(ids=NULL, server="", user="", password=""){

  ###====== CHECK INPUTS ======###
  # check that server, user, password are non-missing and strings
  for (x in c("server", "user", "password")) {
    if (!is.character(get(x))) {
      stop(x, "has to be a string.")
    }
    if (nchar(get(x)) == 0) {
      stop(paste("The following parameter is not specified in the program:", x))
    }
  }

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

  #== function to get stats for one interview
  single_int <- function(id, base_url, user_id, pass){
    # build api endpoint for interview stats
    endpoint <- paste0(base_url, "/interviews/", id, "/stats")

    resp <- httr::GET(endpoint, httr::authenticate(user_id, pass))

    if (httr::status_code(resp)==200){
      int_stats <- jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = TRUE)
      return(int_stats)
    } else if (httr::status_code(resp)==401){
      message("Invalid login or password.")
    } else {
      message("Error getting stats for interview: ", id)
    }
  }

  all_stats_df <- dplyr::bind_rows(lapply(ids, single_int, api_url,
                                          user, password)) %>%
    # rename output columns to be consistent with exported data
    dplyr::rename(interview__id = .data$InterviewId,
                  interview__key = .data$InterviewKey) %>%
    dplyr::select(.data$interview__id, .data$interview__key, dplyr::everything())

  return(all_stats_df)
}
