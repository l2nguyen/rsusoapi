#' Get information about interviewers on the server
#'
#' \code{get_interviewers} returns a data frame containing information about interviewer
#' account. Due to the current limitations of the API, only interviewers that are
#' \strong{not archived/locked} appear in the output data frame.
#' The data frame will have the following columns:
#' \itemize{
#' \item InterName: Username of interviewer
#' \item InterId: Unique ID of interviewer
#' \item SuperName: Username of interviewer's supervisor
#' \item SuperId: Unique ID of interviewer's supervisor
#' \item IsLocked: If interviewer is currently locked/archived
#' \item CreationDate: Date the interviewer account was created
#' \item DeviceId: Unique Id of the tablet that the interviewer was using on the last
#' synchronisation to server. If the user has never synced any data with the server
#' using a tablet, this variable will be null.
#' }
#'
#' @param super_names Usernames of the supervisors that you would like to get the list of
#' interviewers for. This accepts a vector of multiple supervisor usernames. To get data on
#' every interviewer account, leave \code{super_names} and \code{super_ids} null.
#' @param super_ids User Id of the supervisors that you would like to get the list of
#' interviewers for. This accepts a vector of multiple supervisor Ids. User Ids can have
#' dashes or not.
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
#' ## Get data on all interviewers that are currently not archived on server
#' all_ints <- get_interviewers(server = "lfs2018",
#' user = "APIuser2018", password = "SafePassword123")
#'
#' ## Using the usernames for supervisors
#' supervisors <- c("North_Supervisor", "East_Supervisor")
#'
#' interviewers <- get_interviewers(super_names = supervisors,
#' server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#'
#' ## Using user Ids for supervisors
#' supervisors <- c("59e60ce7-a6f9-4b0b-a5b3-ab6e8ce76464",
#' "60d28f7f-6195-4136-a8e8-a0c118218f0d")
#'
#' interviewers <- get_interviewers(super_ids = supervisors,
#' server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#' }

get_interviewers <- function(super_names=NULL, super_ids=NULL,
                             server, user, password){
  #=== BASIC CHECKS
  # check that server, login, password, and data type are non-missing
  for (x in c("server", "user", "password")) {
    if (!is.character(get(x))) {
      stop(x, "has to be a string.")
    }
    if (nchar(get(x)) == 0) {
      stop(paste("The following parameter is not specified:", x))
    }
  }

  # check that not both qx name and template id is specified
  if(!is.null(super_names) & !is.null(super_ids)){
    stop("Specify only either name or user IDs for supervisors.")
  }

  # build base URL for API
  server <- tolower(trimws(server))

  # build base URL for API
  api_url <- sprintf("https://%s.mysurvey.solutions/api/v1",
                     server)

  #==> GET DF OF SUPERVISORS
  all_supers <- get_supers(server, user, password)

  #=== HELPER FUNCTION ====#
  # function to check if supervisor exists
  sup_exists <- function(supervisor, df, stype="name"){
    # set variable name
    var_name <- ifelse(stype=="name", "SuperName", "SuperId")
    if (!(supervisor %in% data[[var_name]])){
      stop("User does not exist: ", supervisor)
    }
  }

  # function get supervisor ID if given name
  get_sup_id <- function(sup_name, df){
    id <- dplyr::filter(df, .data$SuperName==sup_name)$SuperId
    return(id)
  }

  # function to make API call for interiewers for a supervisor
  get_ints <- function(sup_id, base_url, user_id, pass){
    int_endpoint <- paste0(base_url, "/supervisors/", sup_id, "/interviewers")
    data <- httr::GET(int_endpoint, httr::authenticate(user_id, pass),
                      query= list(limit=40))

    if (httr::status_code(data) == 200) {
      # save the list of imported templates from the API as a data frame
      inters <- jsonlite::fromJSON(httr::content(data, as = "text"), flatten = TRUE)

      # get total counts
      total_count <- inters$TotalCount
      # data frame of interviewers
      ints_df <- inters$Users
    } else if (httr::status_code(data) == 401) {# login error
      stop("Incorrect username or password. Check login credentials.")
    } else {# any other error
      stop("Encountered issue with status code ", httr::status_code(data))
    }

    if (total_count == 0 | is.null(total_count)) {
      all_ints_df <- data.frame(
        IsLocked = NA,
        CreationDate = NA,
        DeviceId = NA)
    } else if (total_count>0 & total_count<=40){
      all_ints_df <- ints_df
    } else{
      df_list = list(ints_df)
      # use limit to figure out number of calls to make
      limit <- inters$Limit
      n_calls <- ceiling(total_count/limit)

      for (i in 2:n_calls){
        loop_resp <- httr::GET(int_endpoint, httr::authenticate(user_id, pass),
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

      # bind all data frames together to get full list
      all_ints_df <- dplyr::bind_rows(df_list)
    }

    # add supervisor Id
    all_ints_df$SuperId <- sup_id

    # return data frame of interviewers for supervisor
    return(all_ints_df)
  }

  #======== GET FULL LIST OF INTERVIEWERS ==========#
  # check all supervisor names or IDs specified exist
  if (is.null(super_names) & is.null(super_ids)) {
    ids_to_call <- dplyr::pull(all_supers, .data$SuperId)
  } else if (length(super_names)>0) {
    invisible(sapply(super_names, sup_exists, df = all_supers, stype="name"))
    # get IDs associated with users
    ids_to_call <- sapply(super_names, get_sup_id, df = all_supers)
  } else if (length(super_ids)>0) {
    invisible(sapply(super_ids, sup_exists, df = all_supers, stype="id"))
    # set IDs to get interviewers for
    ids_to_call <- super_ids
  } else {
    stop("Specify only either name or user IDs for supervisors.")
  }

  filtered_supers <- all_supers %>%
    dplyr::filter(.data$SuperId %in% ids_to_call) %>%
    dplyr::select(.data$SuperName, .data$SuperId)

  # get full list of interviewers
  full_df_list <- lapply(ids_to_call, get_ints, base_url=api_url,
                         user_id=user, pass=password)
  # bind list into one big data frame
  all_interviewers <- dplyr::bind_rows(full_df_list) %>%
    dplyr::rename(InterName = .data$UserName, InterId = .data$UserId) %>%
    # add supervisor information
    dplyr::inner_join(filtered_supers, by="SuperId") %>%
    # rearrange columns
    dplyr::select(.data$InterName, .data$InterId, .data$SuperName,
                  .data$SuperId, dplyr::everything())

  return(all_interviewers)
}