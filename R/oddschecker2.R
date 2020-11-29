#' Find the odds of each outcome of an event from www.oddschecker.com
#'
#' Find the odds for each of the listed outcomes of an event on www.oddschecker.com, for
#' each of the listed bookies. This function is essentially a re-exported fix of the
#' function \code{oddschecker} from the {gambleR} package.
#'
#' @param event A www.oddschecker.com event path, given as a string. This is essentially
#'  an event URL with the "www.oddschecker.com/" base removed.
#
#' @export
#'

oddschecker2 <- function (event) {

  # Meta information
  URL <- paste0("http://www.oddschecker.com/", event)
  html <- xml2::read_html(URL)

  #location <- stringr::str_trim(stringr::str_extract(title, "^[^[:digit:]]*"))
  #time <- stringr::str_trim(stringr::str_extract(title, "[:digit:][:digit:]:[:digit:][:digit:]"))
  #name <- html %>% rvest::html_nodes(xpath = "//*[@id=\"betting-odds\"]/section[1]/div/div/div/div/p") %>%
  #  rvest::html_text() %>% stringr::str_trim()

  # Bookmakers (incl. those not offering odds)
  bookmakers <- sapply(html %>% rvest::html_nodes("table tr.eventTableHeader td"),
                       function(td) {
                         title <- rvest::html_nodes(td, css = ".bk-logo-click") %>%
                           rvest::html_attr("title")
                         ifelse(length(title) == 0, "", title)
                       })
  bookmakers <- bookmakers[bookmakers != ""]

  # Add non-bookie fix
  if (length(bookmakers) == 0) return(data.frame())

  # Find possible outcomes (as.vector)
  outcomes <- html %>%
    rvest::html_nodes("tr.diff-row.evTabRow.bc") %>%
    rvest::html_attr("data-bname")

  # Find title
  title <- html %>%
    rvest::html_nodes("h1") %>%
    rvest::html_text() %>%
    stringr::str_remove(" - Winner") %>%
    stringr::str_remove(" Betting Odds")

  # Odds table
  visible_odds <- html %>%
    rvest::html_nodes(".bc.bs, .np") %>%
    rvest::html_attr("data-o") %>%
    matrix(nrow = length(outcomes), byrow = TRUE,
           dimnames = list(outcomes, bookmakers)) %>%
    as.data.frame(stringsAsFactors = FALSE)


  make_all_odds_fractional <- function(string) {
    purrr::map_chr(string, function(.x) {
      if (grepl(.x, pattern = "^[0-9]+$")) {
        paste0(.x, "/", "1")
      } else {.x}
    })
  }

  visible_odds <- visible_odds %>%
    dplyr::mutate(dplyr::across(.fns = make_all_odds_fractional))
  rownames(visible_odds) <- outcomes

  # Return
  list("odds" = visible_odds, "title" = title)

}
