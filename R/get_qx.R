#' Get all templates imported on the server
#'
#' \code{get_qx} returns a data frame with all the questionnaires that are currently
#' imported on the server
#'
#' @param server Prefix for the survey server. It is whatever comes before
#' mysurvey.solutions: [prefix].mysurvey.solutions.
#' @param user Username for the API user on the server.
#' @param password Password for the API user on the server.
#'
#' @importFrom rlang .data
#' @export
#'
#' @return A data frame with information about the
#' imported questionnaires on the server.
#' @examples
#' \dontrun{
#' get_qx(server = "lf2018", user = "APIuser2018", password = "SafePassword123")
#' }

get_qx <- function(server="", user="", password="") {

  # trim and lower server prefix
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

  # build query
  endpoint <- paste0(api_url, "/questionnaires")

  # Send GET request to API
  data <- httr::GET(endpoint, httr::authenticate(user, password),
                    query = list(limit = 40, offset = 1))

  # If response code is 200, request was succesffuly processed
  if (httr::status_code(data)==200) {

    # save the list of imported templates from the API as a data frame
    qnrList <- jsonlite::fromJSON(httr::content(data, as = "text"), flatten = TRUE)
    qnrList_temp <- as.data.frame(qnrList$Questionnaires)

    if (qnrList$TotalCount <= 40) {
      # if 40 questionnaires or less, then do not need to call again
      # Extract information about questionnaires on server
      qnrList_all <- dplyr::arrange(qnrList_temp, .data$Title, .data$Version)
    } else {
      quest_more <- list(qnrList_temp)
      # If more than 40 questionnaires, run query again to get the rest
      nquery <- ceiling(qnrList$TotalCount/40)

      # send query for more questionnaires
      for(i in 2:nquery){
        data2 <- httr::GET(endpoint, httr::authenticate(user, password),
                     query = list(limit = 40, offset = i))

        qnrList_more <- jsonlite::fromJSON(httr::content(data2, as = "text"),
                                           flatten = TRUE)
        questList_more <- as.data.frame(qnrList_more$Questionnaires)
        # append loop df to list
        quest_more[[i]] <- questList_more
      }
      qnrList_temp <- dplyr::bind_rows(quest_more)
      qnrList_all <- dplyr::arrange(qnrList_temp, .data$Title, .data$Version)
    }
  } else if (httr::status_code(data) == 401) {   # login error
    stop("Incorrect username or password.")
  } else { # Issue error message
    stop("Encountered issue with status code ", httr::status_code(data))
  }
}
