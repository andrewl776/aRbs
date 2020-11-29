#' Find all the arbitrage opportunities for a sport homepage
#'
#' Input a sport homepage URL as a string and return a list of arbitrage information.
#' This will also print dataframes with details of the arbitrage opportunities, including
#' the stake required for equal returns, the value of said equal returns, along with the
#' relevant bookie for each outcome.
#'
#' The workhorse function, clearly, is \code{get_arb_single}, however this function
#' implements it iteratively, after scraping and cleaning the required inputs for each
#' distinct call to \code{get_arb_single}. This includes finding all the sub-URLs
#' of the homepage. A progress bar is also used as the runtime is signficant.
#'
#' @param event_list A URL to a www.oddschecker.com sport homepage, given as a string.
#' @param in_play Logical. Should in-play arbitrage opportunities (arbs) also be returned?
#' These are not likely to be accurate arbs as some bookie's odds will not be up-to-date,
#' therefore default is \code{FALSE}.
#' @param ... Arguments passed to \code{get_arb_single}.
#'
#' @return A list, with arbitrage information printed as series of dataframes.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' get_arbs()
#' get_arbs("https://www.oddschecker.com/football") # equivalent to get_arbs()
#' }



## TODO:

# Find way to get subdomains all events
# Find way to filter subdomains to only events (or alternatively anti-break code for non-event subdomains)
# Query apis (look at betfair, abettor packages)


## Example event:

# "https://www.oddschecker.com/football/belarus/premier-league/dinamo-brest-v-isloch/winner)"

# Fetch results for all links ------------------------------------------------------------------

#' @importFrom dplyr `%>%`

get_arbs <- function(event_list = "https://www.oddschecker.com/football",
                     in_play = FALSE,
                     print_urls = FALSE)
{

  # Find individual events
  events <- event_list %>%
    scrape_links() %>%
    clean_links()

  # Create progress bar and start it visually in the console
  pb <- progress::progress_bar$new(total = length(events),
                                   format = paste0(" Finding arbitrage opportunities [:bar] :percent",
                                                   " ETA::eta. Elapsed time: :elapsed."))
  pb$tick(0)

  # Create logical vector detailing whether the events are in-play or not
  in_play_vec <- event_list %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"all-odds-click\", \" \" ))]") %>%
    as.character() %>%
    vapply(function(s) grepl(x = s, pattern = "in-play"), logical(1), USE.NAMES = FALSE)


  # Get all results and progress the progress bar
  results <- purrr::map(events, function(s) {
    pb$tick()
    get_arb_single(s, print_urls = print_urls)
  })


  for (i in seq_along(results)) {
    results[[i]] <- c(results[[i]], "in_play" = in_play_vec[2*i - 1])
  }

  printed_results <- results[vapply(results, function(s) s$Arb_Opp, logical(1))]

  # Set names of results for user
  names(printed_results) <- printed_results %>%
    purrr::map(function(s) s$title) %>%
    purrr::map(~ stringr::str_remove(.x, " - Winner$")) %>%
    vapply(function(s) stringr::str_remove(s, " Betting Odds$"), character(1))

  # Remove in play arbs if necessary
  if (in_play == FALSE) {
    printed_results <- printed_results[printed_results %>%
                                         vapply(function(s) !(s$in_play), logical(1))]
  }

  # Set new class of return object and invisibly return it
  class(printed_results) <- "arb"
  invisible(printed_results)
}



# # SCRAP -------------------------------------------------------------------



#
#
# abettor::loginBF("andrew.little.mail@gmail.com", "Sainties1")
# betfair::login(username = )
#
# # Betfair -----------------------------------------------------------------
#
#
# grepl(pattern = "^[[:digit:]]", "2/1")
#
# # Profit = (Investment / Arbitrage %) - Investment
#
# # Individual bets = (Investment x Individual Arbitrage %) / Total Arbitrage %
#
# # Stake = (total stake x implied probability) ? combined market margin
#
#
#
# # Implied probabilities
# imp_p_w <- c(0.4736842,
#              0.2857143,
#              0.2222222)
#
#
# stake <- 100 *
#
#   stake <- (100 * imp_p)/0.9816207
#
# win <- (stake * c(10/9, 5/2, 7/2)) + stake
# (profit * imp_p)
#
# exp_profit <-
#
#   # expected profit = ((profit * imp_p_w) - (stake * imp_p_l)) * 1/sum(imp_p_w)
#
#
#   imp_p_l <- sum(imp_p_w) - imp_p_w
#
#
#
#
# (53.617 + stake) * imp_p_w[1]
# stake[1] * imp_p_l[1]
#
















