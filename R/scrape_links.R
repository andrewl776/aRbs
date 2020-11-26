#' Scrape all subdomain URLs of a domain
#'
#' @details Used in the call to \code{get_arbs()} to find subdomains that
#'  may offer events, from which we can find an arb.
#'
#' @param url A head domain URL, given as a string.
#'
#' @return A \code{data.frame}.
#'
#' @export
#' @examples scrape_links("https://www.oddschecker.com/football")

scrape_links <- function(url) {

  # Create an html document from the url
  webpage <- xml2::read_html(url)

  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  return(data.frame(link = link_, url = url_, stringsAsFactors = FALSE))
}
