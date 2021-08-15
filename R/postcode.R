#' Extract postcode from an address
#'
#' @description Extracts postcode (5 digits). If more than one postcode (5 digits) can be found, it will return the last postcode found.
#'
#'
#' @param .data Input data set
#' @param address Address variable
#'
#' @export
#'
#' @examples
#' parse_postcode("5, shah alam, 40150")


parse_postcode <- function(address) {

  extract_all <- purrr::map_chr(stringr::str_extract_all({{address}}, "\\d{5}"), ~stringr:: str_c(.x, collapse=","))
  extract_last <- stringr::word(extract_all, -1, sep = ",")

  extract_last
}
