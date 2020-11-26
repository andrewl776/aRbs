#' Clean links given by the \code{scrape_links()} function.
#'
#' Filter links to only those ending in "winner" and clean for use in the
#' \code{get_arb_single()} function.
#'
#' @param links A \code{data.frame} of links with a column "url" containing
#' www.oddschecker.com URLs, usually the output of the \code{scrape_links()} function.
#'
#' @export
#'
#' @examples
#' links <- scrape_links("https://www.oddschecker.com/football")
#' clean_links(links)

clean_links <- function(links) {
  links %>%
    dplyr::filter(stringr::str_detect(url, pattern = "winner$")) %>%
    dplyr::select(url) %>%
    dplyr::mutate(url = ifelse(stringr::str_detect(url, pattern = "^\\/"),
                               paste0("https://www.oddschecker.com", url),
                               url)) %>%
    as.list() %>%
    data.table::transpose()
}
