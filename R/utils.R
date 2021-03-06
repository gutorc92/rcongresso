if (getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Recovers a json from a URL using HTTP.
#'
#' @param full_link URL admitting GET HTTP requests.
#'
#' @return the json.
#'
#' @examples
#' pec241_json <- .get_json("https://dadosabertos.camara.leg.br/api/v2/proposicoes/2088351")
#'
#' @export
.get_json <- function(response){

  r <- httr::content(response, as = "text")
  r_json <- jsonlite::fromJSON(r, flatten = T)

  return(r_json)
}

#' Wraps an access to the congress API given a reletive path and query arguments.
#'
#' @param path URL relative to the API base URL
#' @param query Query parameters
#'
#' @export
.congresso_api <- function(path=NULL, query=NULL){

  ua <- httr::user_agent(.RCONGRESSO_LINK)
  api_url <- httr::modify_url(.API_LINK, path = path, query = query)

  resp <- httr::GET(api_url, ua, httr::accept_json())

  httr::stop_for_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }

  resp_json <- .get_json(resp)

  return(resp_json)
}

.remove_lists_and_nulls <- function(x){
  arr_null <- which(sapply(x, is.null))
  if (length(arr_null)){
    x <- x[-arr_null]
  }

  arr_lists <- which(sapply(x, is.list))
  if (length(arr_lists)){
    x <- x[-arr_lists]
  }

  tibble::as.tibble(x)
}

.empty_list_to_dataframe <- function(lista) {
  if (is.list(lista) && !length(lista)){
    as.data.frame(lista) %>%
      return()
  } else return(lista)
}

.to_tibble <- function(num) {
  if (is.null(num)) tibble::tibble()
  else tibble::tibble(num)
}

.is_gender <- function(gender){
  gender <- toupper(gender)
  return(gender %in% c("M", "F"))
}

.is_estate <- function(estate){
  estate <- toupper(estate)
  return(estate %in% c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
                       "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR",
                       "RS", "SC", "SE", "SP", "TO"))
}

parameters_url <- function(parameters, ...){
  values <- eval(substitute(alist(...)))
  print(values)
  url_parameters <- sapply(names(parameters), simplify = T, function(x){
    value <- values[[parameters[[x]]$parameter_function]]
    if("validate" %in% parameters[[x]]){
      stopifnot(parameters[[x]]$validate(value))
    }
    if(!is.null(value)){
      url_parameters <- paste(x, value, sep = "=")
      return(url_parameters)
    }
    return(NA)
  })
  url_parameters <- url_parameters[!is.na(url_parameters)]
  url_parameters <- paste(url_parameters, collapse = "&")
  return(url_parameters)

}
