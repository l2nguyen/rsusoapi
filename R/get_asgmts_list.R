#' Get the assignments on the server
#'
#' \code{get_asgmts_list} gets information about the assignments for a certain version of a
#' questionnaire. The user can find the questionnaire of interest using the template ID for
#' the questionnaire or the name of the questionnaire. The user can also filter the list
#' based on archived status and the interviewer/supervisor currently responsible
#' for the assignment.
#'
#' @param template_id Template ID for the questionnaire. This can either have dashes or not.
#' @param qx_name Name of the questionnaire. It may be more convenient to specify the
#' questinnaire name instead of the template ID. \emph{Do not specify both the template_id
#' and qx_name.}
#' @param version Version of questionnaire.
#' @param responsible Option to filter by the user responsible for the assignment. By
#' default, it will find all assignments for the questionnaire regardless of who is
#' responsible.
#' @param archived Option to search for archived assignments. If TRUE, the assignment list
#' will only show archived assignments. By default, the list will only show active
#' assignments.
#' @param output Desired output type for the list of assignments.
#' Options are: "df" for data frame, "tab" for tab delimited file or
#' "excel" for an Excel file. By default, it is a data frame.
#' @param output_path Name of the file that you would like to save the output as.
#' This \emph{must} be specified if the output type is tab or excel.
#' It is recommended to use forward slash (/) instead of backslash (\\).
#' @param server Prefix for the survey server. It is whatever comes before
#' mysurvey.solutions: [prefix].mysurvey.solutions.
#' @param user Username for the API user on the server.
#' @param password Password for the API user on the server.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
#' @return A data frame or an exported file with information about assignments on
#' the server.
#' @examples
#' \dontrun{
#' ## Using template ID
#' all_assignments <- get_asgmts_list(template_id = "68b8a85fd7a84c45b4bb0c2c1c19c53f",
#' version=3, server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#'
#' ## Using questionnaire name
#' all_assign_use_qx <- get_asgmts_list(qx_name = "Labour Force Survey 2018 Q1",
#'  version=3, server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#'
#' ## Filter by user responsible
#' super1_assignments <- get_asgmts_list(qx_name = qx_name = "Labour Force Survey 2018 Q1",
#'  version=3, responsible="Supervisor1",
#'  server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#'
#' ## Export to Excel
#' get_asgmts_list(qx_name = "Labour Force Survey 2018 Q1", version=3,
#' output = "excel", output_path="/User/bar/Documents/active_assignments.xlsx",
#' server = "lfs2018", user = "APIuser2018", password = "SafePassword123")
#' }

get_asgmts_list <- function(template_id = NULL, qx_name = NULL, version = NULL,
                            responsible = "", archived = FALSE,
                            output = "df", output_path = NULL,
                            server="", user="", password="")
{
  # -------------------------------------------------------------
  # CHECK ALL INPUTS
  # -------------------------------------------------------------
  # check that server, login, password are non-missing
  for (x in c("server", "user", "password")) {
    if (!is.character(get(x))) {
      stop(x, "has to be a string.")
    }
    if (nchar(get(x)) == 0) {
      stop(paste("The following parameter is not specified in the program:", x))
    }
  }

  # Check output is a valid output data type
  if (tolower(output) %in% c("df", "tab", "excel") == FALSE) {
    stop("Output has to be either df (data frame), tab, or excel.")
  }

  # confirm that output path was specified
  if ((tolower(output) %in% c("tab", "excel")) & is.null(output_path)) {
    stop("Specify output path for tab or excel output.")
  }

  # check if archived is logical
  if (!is.logical(archived)){
    stop("Specify either TRUE or FALSE for archived status.")
  }

  # check version is numeric
  if (!is.numeric(version)) {
    if (is.null(version)){
      stop("Specify version number.")
    } else if (is.na(as.numeric(version))) {
      stop("Version number ", version, " is not numeric.")
    } else {
      version <- as.numeric(version)
    }
  }

  if(is.null(qx_name) & is.null(template_id)){
    stop("Either qx_name or template_id must be specified.")
  }
  # check that not both qx name and template id is specified
  if(!is.null(qx_name) & !is.null(template_id)){
    stop("Specify only either qx_name or template_id.")
  }

  # -------------------------------------------------------------
  # Get template id if only questionnaire name is specified
  # -------------------------------------------------------------
  if (!is.null(qx_name) & is.null(template_id)){
    # trim white space around name
    qx_name <- trimws(qx_name)

    # get the list of questionnaires on the server
    all_qx <- susoapir::get_qx(server=server, user=user, password=password)

    # Get ID of template to get export URL
    qx_match <- dplyr::filter(all_qx, .data$Title==qx_name, .data$Version==version)

    if (nrow(qx_match)==1) {
      qx_id <- qx_match$QuestionnaireIdentity
    } else {
      stop("Could not find questionnaire on server. Check questionnaire name and version number is correct.")
    }
  }

  # -------------------------------------------------------------
  # Call API
  # -------------------------------------------------------------

  # build base URL for API
  server <- tolower(trimws(server))

  api_url <- sprintf("https://%s.mysurvey.solutions/api/v1",
                     server)

  # build api endpoint
  endpoint <- paste0(api_url, "/assignments")

  # build template id for query
  # if qx name specified, used qx_id
  if (!is.null(template_id)){
    qid = paste0(template_id, '$', version)
  } else if (!is.null(qx_name)){
    qid = qx_id
  }

  user_query <- list(questionnaireId = qid,
                     responsible = responsible,
                     showArchive = archived)


  # Send GET request to API
  data <- httr::GET(endpoint, httr::authenticate(user, password),
                    query = user_query)

  if (httr::status_code(data)==200){
    assignments <- jsonlite::fromJSON(httr::content(data, as = "text"))
    # get total count
    total_count <- assignments$TotalCount
    # get limit
    limit <- assignments$Limit
    # calculate number of calls that needs to be made
    n_calls <- ceiling(total_count/limit)
  } else if (httr::status_code(data)==401){
    stop("Invalid login or password.")
  } else {
    stop(paste0("API request failed with code ", httr::status_code(data)))
  }

  # function to transform the id vars from list into columns
  transform_id_vars <- function(df){
    Id_vars <- df %>%
      dplyr::select(.data$Id, .data$IdentifyingQuestions) %>%
      tidyr::unnest(cols = c(.data$IdentifyingQuestions)) %>%
      dplyr::select(-.data$Identity) %>%
      tidyr::spread(.data$Variable, .data$Answer)

    df_with_id <- dplyr::left_join(df, Id_vars, by = 'Id') %>%
      dplyr::select(-.data$ResponsibleId, -.data$QuestionnaireId,
                    -.data$IdentifyingQuestions)

    return(df_with_id)
  }

  # initiate empty list for output
  df_list <- list()
  # intialise progress bar
  progbar <- utils::txtProgressBar(min = 0, max = n_calls, style = 3)

  # Send post query for all the data
  for (i in 1:n_calls){
    # build user query with offset
    user_query_loop <- list(questionnaireId = qid,
                            responsible = responsible,
                            showArchive = archived,
                            offset = (20 * (i-1))
    )

    # Send GET request to API
    resp <- httr::GET(endpoint, httr::authenticate(user, password),
                      query = user_query_loop)

    # save the list of imported templates from the API as a data frame
    assignments <- jsonlite::fromJSON(httr::content(resp, as = "text"))

    utils::setTxtProgressBar(progbar, i)
    # if successful, add information to list of data frames
    if (httr::status_code(resp)==200){
      assignments_temp <- as.data.frame(assignments$Assignments)

      # transform identifyng variable from list to column
      assignments_id <- transform_id_vars(assignments_temp)

      df_list[[i]] <- assignments_id
    } else {
      stop(paste0("API request failed with code ", httr::status_code(resp)))
    }
  }

  # bind all output together into a big dataframe
  if (length(df_list)==1){
    all_assignments <- df_list[[1]]
  } else {
    all_assignments <- dplyr::bind_rows(df_list)
    }

  if (output == "tab"){
    readr::write_tsv(all_assignments, path=output_path)
  } else if (output == "excel") {
    writexl::write_xlsx(all_assignments, path=output_path,
                        format_headers=FALSE)
  } else {
    # return data frame if not exporting to tab or excel
    return(all_assignments)
  }
}
