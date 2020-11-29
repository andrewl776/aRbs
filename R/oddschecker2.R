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


# TODO: Sort out this function entirely. Currently copied with minor
# edits from {gambleR}

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

  # contender <- min(which(bookmakers != "")) - 1
  # odds <- html %>% rvest::html_nodes("table.eventTable") %>% (function(s) s[[1]]) %>%
  #   rvest::html_table(fill = TRUE)
  # odds <- odds[-(1:which(odds[, contender] == "")[1]), ]
  # odds <- odds[!apply(odds, 1, function(d) all(d == "" | is.na(d))),
  #              ]
  # rownames(odds) <- odds[, contender]
  # names(odds) <- bookmakers

  ##############################################################################

  # Previously used code from {gambleR}. Keeping for now but will remove if not needed for fixes soon.

  # odds <- odds[, -seq(1, contender)]
  # odds <- odds[, sapply(odds, function(column) {
  #   any(column != "") && !all(is.na(column))
  # })]
  # for (n in 1:ncol(odds)) {
  #   odds[, n] <-  gsub("^([[:digit:]]+)$", "\\1/1", gsub("^SP$",
  #                                                        "", odds[, n]))
  # }

  ##############################################################################

  # Fix - currently odds don't seem to be filtered enough. Special offers are creeping in
  # and giving us extra outcomes i.e. rows with no odds but just text. We will try to fix here:
  # odds <- odds[, !is.na(names(odds))]
  #
  # odds <- odds %>%
  #   # Check along rows for odds format. We can have fractional, or integers less than 4 digits (otherwise some
  #   # code 3278223723 rows end up being kept)
  #   apply(FUN = function(s) {
  #     stringr::str_detect(s, pattern = "^[0-9]{1,9}\\/[0-9]+$") | stringr::str_detect(s, pattern = "^[0-9]{1,4}$")
  #   },
  #   MARGIN = 1) %>%
  #   as.data.frame() %>%
  #   # We then check which rows have a significant number of odds-looking entries (current threshold of 0.2)
  #   purrr::map_lgl(function(s) sum(s) >= 2) %>%
  #   # Then select these rows
  #   (function(s) odds[s, ])
  #
  # if (nrow(odds) == 0) {
  #   warning(title, " returned no rows that looked like odds.")
  #   return(data.frame())
  # } else if (nrow(odds) <= 2) {
  #   warning(paste0(title, " has only ", nrow(odds), " rows."))
  # }

}
