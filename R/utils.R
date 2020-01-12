#== CHECKS USED FOR OTHER FUNCTIONS

# checks internet connection
check_internet <- function(){
  if (curl::has_internet()==FALSE){
  stop("No internet connection.", call. = FALSE)
  }
}

# checks server, user and password parameters are not empty
# check those parameters are strings
check_str_params <- function(variables){
  # check that server, user, password are non-missing and strings
  for (x in variables) {
    if (nchar(get(x)) == 0) {
      stop("The following parameter needs to be specified: ", x, call.=FALSE)
    } else if (!is.character(get(x))) {
      stop(x, "has to be a string.", call. = FALSE)
    }
  }
}

# check server exists
check_server <- function(url){
  # Check server exists
  tryCatch(httr::http_error(url),
           error=function(err) {
             err$message <- paste(url, "is not a valid server.")
             stop(err)
           })
}

# check only qx_name or template_id is specified
check_only_one <- function(var1, var2){
  # save name for error messages later
  name1 <- deparse(substitute(var1))
  name2 <- deparse(substitute(var2))
  if(is.null(var1) & is.null(var2)){
    stop("Either ", name1, " or ", name2, " must be specified.",
         call.=FALSE)
  }
  # check that not both qx name and template id is specified
  if(!is.null(var1) & !is.null(var2)){
    stop("Specify only either ", name1, " or ", name2, ".",
         call.=FALSE)
  }
}

# check version number is not null and numeric
check_version <- function(x){
  if (is.null(x)){
    stop("Specify version number.", call. = FALSE)
  } else if (!is.numeric(x)) {
    # check if numeric
      if (is.na(suppressWarnings(as.numeric(x)))) {
        stop("Version has to be a number.", call. = FALSE)
      } else {
        numeric_x <- as.numeric(x)
        return(numeric_x)
      }
  } else{ return(x) }
}