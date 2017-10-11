#' Retrieves details about a deputy.
#'
#' @param dep_id deputy's ID.
#'
#' @return Dataframe containing details about the deputy.
#'
#' @examples
#' abel_mesquita_info <- fetch_deputado(178957)
#'
#' @export
fetch_deputado <- function(dep_id){
  id <- NULL
  tibble::tibble(id = dep_id) %>%
    dplyr::mutate(path = paste0(.DEPUTADOS_PATH, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados %>%
        .remove_lists_and_nulls()
    ) %>%
    dplyr::ungroup() %>%
    return()
}

#' Fetches expenditures from deputy with his/her parlamentary quota in
#' the last six months.
#'
#' @param dep_id deputy's ID.
#
#' @return Dataframe containing details about the deputy's expenditures.
#'
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(178957)
#'
#' @export
fetch_despesas_deputado <- function(dep_id) {
  id <- path <- NULL
  query <- list(ordem = "ASC", ordenarPor = "numAno")

  tibble::tibble(id = dep_id) %>%
    dplyr::mutate(path = paste0(.DEPUTADOS_PATH, "/", id, "/despesas")) %>%
    dplyr::group_by(id, path) %>%
    dplyr::do(
      .congresso_api(.$path, query)$dados
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(idDep = .$id) %>%
    dplyr::select(-path, -id) %>%
    return()
}


#' Retrieves deta about congressmen
#'
#' @param dep_id congressman's ID.
#' @param term legislature term.
#' @param estate congressmen's state.
#' @param party congressmen's party
#' @param gender congressmen's gender, it can be M(Male) or F(Female).
#' @param page number of pages.
#' @param items number of items by page.
#' @param orderby field to order the returnted list.
#'
#' @return Dataframe containing details about the deputies.
#'
#' @examples
#' all_deputies <- fetch_deputados()
#'
#' @export
fetch_deputados <- function(dep_id = NULL, term = NULL,  estate = NULL, party = NULL,
                            gender = NULL, page = NULL, items = NULL, orderby = NULL){
  parameters_list <- list("siglaSexo" = list("parameter_function" = "gender", "validate" = .is_gender),
                     "id" = list("parameter_function" = "dep_id"),
                     "siglaUf" = list("parameter_function" = "estate", "validate" = .is_estate),
                     "siglaPartido" = list("parameter_function" = "party"),
                     "ordenarPor" = list("parameter_function" = "orderby"))

  parameters <- parameters_url(parameters_list, dep_id, term, estate, party, gender, page, items, orderby)
  url <- paste(.DEPUTADOS_PATH, parameters, sep = "?")
  return(url)
  #return(.congresso_api(url_parameters)$dados)
}
