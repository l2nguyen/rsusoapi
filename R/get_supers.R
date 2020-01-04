#' Get data frame of supervisors on the server
#'
#' \code{get_supers} Returns a data frame containing information about supervisors
#' account. Due to the current limitations of the API, only supervisors that are
#' \strong{not archived/locked} appear in the output data frame.
#' The data frame will have the following columns:
#' \itemize{
#' \item SuperName: User name of supervisors
#' \item SuperId: Unique ID of supervisors
#' \item IsLocked: If supervisor is currently locked/archived
#' \item CreationDate: Date the supervisor account was created
#' \item DeviceId: Unique Id of the tablet that the user was using on the last
#' synchronisation to server. If the user has never synced any data with the server
#' using a tablet, this variable will be null.
#' }
#'
#' @param server Prefix for the survey server. It is whatever comes before
#' mysurvey.solutions: [prefix].mysurvey.solutions.
#' @param user Username for the API user on the server.
#' @param password Password for the API user on the server.
#'
#' @importFrom rlang .data
#' @export
#'
#' @return A data frame  data frame containining information about supervisor accounts.
#' @examples
#' \dontrun{
#' get_supers(server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#' }

get_supers <- function(server, user, password) {
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

  # build query
  endpoint <- paste0(api_url, "/supervisors")

  # initial call to test API works
  data <- httr::GET(endpoint, httr::authenticate(user, password),
                    query= list(limit=40))

  # if API call works, get supervisor information
  if (httr::status_code(data) == 200) {
    # save the list of imported templates from the API as a data frame
    sups <- jsonlite::fromJSON(httr::content(data, as = "text"), flatten = TRUE)
    super_df <- sups$Users
    df_list <- list(super_df)
    # get total count for iteration
    total_count <- sups$TotalCount
  } else if (httr::status_code(data) == 401) {# login error
    stop("Incorrect username or password. Check login credentials.")
  } else {# any other error
    stop("Encountered issue with status code ", httr::status_code(data))
  }

  # if less than 40, return only data frame in list
  if (total_count<=40){
    sups_df <- df_list[[1]]
  } else{
    # use limit to figure out number of calls to make
    limit <- sups$Limit
    n_calls <- ceiling(total_count/limit)

    for (i in 2:n_calls){
      loop_resp <- httr::GET(endpoint, httr::authenticate(user, password),
                             query= list(limit=40, offset=i))

      if (httr::status_code(loop_resp) == 200) {
        # process response
        flat_loop <- jsonlite::fromJSON(httr::content(loop_resp, as = "text"),
                                        flatten = TRUE)
        loop_df <- flat_loop$Users
        # append to existing list of df
        df_list[[i]] <- loop_df
      } else {# any other error
        stop("Encountered issue with status code ", httr::status_code(loop_resp))
      }
    }
    # bind all dataframes with supervisor info together
    sups_df <- dplyr::bind_rows(df_list)
  }

  output_df <- sups_df %>%
    dplyr::select(.data$UserName, .data$UserId, .data$IsLocked,
                  .data$CreationDate, .data$DeviceId) %>%
    dplyr::rename(SuperId = .data$UserId, SuperName = .data$UserName)

  # return data frame with supervisors
  return(output_df)
}
