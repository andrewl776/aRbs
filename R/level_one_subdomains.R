#' @keywords internal

level_one_subdomains <- function() {

  urls <- "https://www.oddschecker.com/" %>%
    scrape_links() %>%
    clean_links()


  subdomains <- urls[grepl(urls,
                           pattern = "^https:\\/\\/www\\.oddschecker\\.com\\/")] %>%
    stringr::str_remove("^https:\\/\\/www\\.oddschecker\\.com\\/") %>%
    strsplit("\\/") %>%
    purrr::map_chr(function(s) s[1])

  subdomain_counts <- subdomains %>%
    table() %>%
    sort(decreasing = TRUE)

  subdomains <- unique(subdomains)

  not_404 <- purrr::map_lgl(subdomains, function(s) {
    httr::http_status(httr::GET(
      paste0("https://www.oddschecker.com/", s)
    ))$category == "Success"
  })

  subdomain_counts <- subdomain_counts[not_404]
  subdomains <- subdomains[not_404]

  names(subdomains) <- subdomains %>%
    stringr::str_replace_all(pattern = "-", " ") %>%
    stringr::str_to_title()

  list(
    "subdomains" = subdomains,
    "subdomain_counts" = subdomain_counts
  )

}
