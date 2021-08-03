#' Extract postcode(s) from an address
#'
#' @param .data Input data set
#' @param address Address variable
#'
#' @return A tibble with the number postcodes found (5 digits) & the extracted postcode
#' @export
#'
#' @examples

parse_postcode <- function(.data, address) {

  .data %>%
    dplyr::mutate(postcode_count = stringr::str_count({{address}}, "\\d{5}")) %>%
    dplyr::mutate(postcode_final = purrr::map_chr(stringr::str_extract_all({{address}}, "\\d{5}"), ~stringr:: str_c(.x, collapse=","))) %>%
    dplyr::mutate(postcode_final = stringr::word(postcode_final, -1, sep = ","))

}
