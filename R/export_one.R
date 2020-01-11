#' Start export of the data for a questionnaire
#'
#' \code{start_export} starts the export of a version of a questionnaire for a specified
#' data type
#'
#' @param qx_name Name of questionnaire. This is case sensitive and should match the name
#' of your questionnaire exactly. Do not specify both template_id and qx_name.
#' @param template_id Questionnaire Id for the template. Do not include any dashes.
#' Do not specify both template_id and qx_name.
#' @param version Version number of questionnaire
#' @param export_type Data type to export. Valid options are:  tablular, stata, spss,
#' binary, paradata. Default is tabular.
#' @param server Prefix for the survey server. It is whatever comes before
#' mysurvey.solutions: [prefix].mysurvey.solutions.
#' @param user Username for the API user on the server.
#' @param password Password for the API user on the server.
#'
#' @importFrom rlang .data
#' @export
#' @rdname start_export
#'
#' @return Starts the export for the questionnaire and returns the start time in UTC.
#' @examples
#' \dontrun{
#' start_time <- start_export(qx_name="Labour Force Survey Q1", version=4,
#' server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#' }
start_export <- function(qx_name=NULL, template_id=NULL, version=NULL,
                         export_type = "tabular", server, user, password){

  #== CHECK PARAMETERS
  if(is.null(qx_name) & is.null(template_id)){
    stop("Either qx_name or template_id must be specified.")
  }
  # check that not both qx name and template id is specified
  if(!is.null(qx_name) & !is.null(template_id)){
    stop("Specify only either qx_name or template_id.")
  }

  # check version is numeric
  if (!is.numeric(version)) {
    if (is.null(version)){
      stop("Specify version number.")
    } else if (is.na(as.numeric(version))) {
      stop("Version number ", version, " is not a number.")
    } else {
      version <- as.numeric(version)
    }
  }

  # Check output is a valid output data type
  if ((tolower(export_type) %in%
       c("tabular", "stata", "spss", "binary", "paradata")) == FALSE) {
    stop("Data type has to be one of the following: Tablular, STATA, SPSS, Binary, paradata"
         )
  }

  # check that server, user, password are non-missing and strings
  for (x in c("server", "user", "password")) {
    if (!is.character(get(x))) {
      stop(x, "has to be a string.")
    }
    if (nchar(get(x)) == 0) {
      stop(paste("The following parameter needs to be specified:", x))
    }
  }

  #==== build base URL for API
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

  # check if list of questionnaire already exists
  qnrList_all <- get_qx(server, user = user, password = password) %>%
    dplyr::mutate(TemplateId = gsub("-", "", .data$QuestionnaireId))

  # trim white space before
  if(!is.null(qx_name)){
    qx_name <- trimws(qx_name)
    message_name <- qx_name

    # Get ID of template to get export URL
    qx_match <- dplyr::filter(qnrList_all, .data$Title == qx_name, .data$Version == version)

  } else if (!is.null(template_id)){
    template_id <- trimws(template_id)
    # save name for message later
    message_name <- template_id

    qx_match <- dplyr::filter(qnrList_all, .data$TemplateId == template_id,
                              .data$Version == version)
  } else{
    stop("Either qx_name or template_id must be specified.")
  }

  if (nrow(qx_match) == 1) {
    qx_id <- qx_match$QuestionnaireIdentity
    # questionnaire variable for naming zip file later
    qx_var <- qx_match$Variable
  } else {
    stop("Could not find match on server. Check template_id or qx_name and version number.")
  }

  # -----------------------------------------------------------------------------
  # Request export files to be created
  # -----------------------------------------------------------------------------

  # standardise export type
  export_type <- tolower(trimws(export_type))

  export_url <- sprintf("%s/export/%s/%s",
                        api_url, export_type, qx_id)

  # post request to API
  start_query <- paste0(export_url, "/start")

  startExport <- httr::POST(start_query, httr::authenticate(user, password))

  if (httr::status_code(startExport)==200){
    # Get start time of export
    if (is.na(httr::headers(startExport)$date)) {
      # take the date in case the response header is missing
      start_time <- as.POSIXct(startExport$date,
                               format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    } else {
      start_time <- as.POSIXct(httr::headers(startExport)$date,
                               format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    }

    # convert start time into UTC for standardization with server response time
    start_time_utc <- lubridate::with_tz(start_time, "UTC")
    # print message
    message("Requesting data for ", message_name,
            " v", version,
            " to be compiled on server.")

    return(start_time_utc)

  } else if (httr::status_code(startExport) == 401) {   # login error
    stop("Incorrect username or password.")
  } else { # Issue error message
    stop("Encountered issue with status code ", httr::status_code(startExport))
  }
}
