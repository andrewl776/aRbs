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
  URL <- paste0("http://www.oddschecker.com/", event)
  html <- xml2::read_html(URL)
  title <- html %>% rvest::html_nodes("h1") %>% rvest::html_text()
  location <- stringr::str_trim(stringr::str_extract(title, "^[^[:digit:]]*"))
  time <- stringr::str_trim(stringr::str_extract(title, "[:digit:][:digit:]:[:digit:][:digit:]"))
  name <- html %>% rvest::html_nodes(xpath = "//*[@id=\"betting-odds\"]/section[1]/div/div/div/div/p") %>%
    rvest::html_text() %>% stringr::str_trim()
  bookmakers <- sapply(html %>% rvest::html_nodes("table tr.eventTableHeader td"),
                      function(td) {
                        title <- rvest::html_nodes(td, css = ".bk-logo-click") %>%
                          rvest::html_attr("title")
                        ifelse(length(title) == 0, "", title)
                      })

  # Add non-bookie fix
  if (all(bookmakers == "")) return(data.frame())
  ####


  contender <- min(which(bookmakers != "")) - 1
  odds <- html %>% rvest::html_nodes("table.eventTable") %>% (function(s) s[[1]]) %>%
    rvest::html_table()
  odds <- odds[-(1:which(odds[, contender] == "")[1]), ]
  odds <- odds[!apply(odds, 1, function(d) all(d == "" | is.na(d))),
               ]
  rownames(odds) <- odds[, contender]
  names(odds) <- bookmakers
  odds <- odds[, -seq(1, contender)]
  odds <- odds[, sapply(odds, function(column) {
    any(column != "") && !all(is.na(column))
  })]
  for (n in 1:ncol(odds)) {
    odds[, n] <-  gsub("^([[:digit:]]+)$", "\\1/1", gsub("^SP$",
                                                       "", odds[, n]))
  }
  odds
}
