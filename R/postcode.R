parse_postcode <- function(.data, column1) {

  .data %>%
    mutate(postcode_count = str_count({{column1}}, "\\d{5}")) %>%
    mutate(postcode_final = map_chr(str_extract_all({{column1}}, "\\d{5}"), ~ str_c(.x, collapse=","))) %>%
    mutate(postcode_final = word(postcode_final, -1, sep = ","))

}
