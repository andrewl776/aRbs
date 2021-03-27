#' Find Arbitrage Opportunities
#'
#'
#' Input a sport homepage URL as a string and return a list of arbitrage information.
#' This will also print dataframes with details of the arbitrage opportunities, including
#' the stake required for equal returns, the value of said equal returns, along with the
#' relevant bookie for each outcome.
#'
#' The workhorse function, clearly, is \code{get_arb_single}, however \code{get_arbs}
#' implements it iteratively, after scraping and cleaning the required inputs for each
#' distinct call to \code{get_arb_single}. This includes finding all the sub-URLs
#' of the homepage. A progress bar is also used as the runtime is signficant.
#'
#' @param event A URL to a www.oddschecker.com sport homepage, given as a string.
#'
#' @param in_play Logical. Should in-play arbitrage opportunities (arbs) also be returned?
#' These are not likely to be accurate arbs as some bookie's odds will not be up-to-date,
#' therefore default is \code{FALSE}.
#'
#' @param print_urls Logical. Should the URL of the event(s) be printed to the console while
#' searching for arbitrage opportunities? Passed to \code{get_arb_single}.
#'
#' @param parallel Logical. Should iterative calls to \code{get_arb_single} be made in
#' parallel.
#'
#' @param debug Logical. If set to \code{TRUE}, \code{print_urls} will be turned on and
#' \code{parallel} will be turned off (so that URLs can be printed continuously).
#'
#' @return A list, with arbitrage information printed as series of dataframes.
#'
#'
#' @examples
#'
#' \donttest{
#' get_arbs()
#' get_arbs("https://www.oddschecker.com/football") # equivalent to get_arbs()
#' }
#'
#' @export
#' @importFrom dplyr `%>%`


## TODO: call all functions explicitly including from {aRbs} (this
## seemed to cause a bug earlier propogating from get_arb_single)



## Example event:

## "https://www.oddschecker.com/football/belarus/premier-league/dinamo-brest-v-isloch/winner)"

# Fetch results for all links ------------------------------------------------------------------



get_arbs <- function(event = "https://www.oddschecker.com/football",
                     in_play = FALSE,
                     print_urls = FALSE,
                     parallel = TRUE,
                     debug = FALSE)
{

  # Define args
  if (debug) {
    parallel <- FALSE
    print_urls <- TRUE
  }

  # Find individual events
  events <- event %>%
    scrape_links() %>%
    clean_links()

  # Create progress bar and start it visually in the console
  pb <- progress::progress_bar$new(total = length(events),
                                   format = paste0(" Finding arbitrage opportunities [:bar] :percent",
                                                   " ETA::eta. Elapsed time: :elapsed."))
  pb$tick(0)

  # If desired, run in parallel, otherwise run sequentially
  if (parallel) {
    num_cores <- parallel::detectCores()
    cl <- parallel::makeCluster(num_cores)

    parallel::clusterExport(cl,
                            "events",
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(aRbs)
    })

    results <- parallel::parLapply(cl, events, function(s) {
      pb$tick()
      Sys.sleep(0.7)
      aRbs::get_arb_single(s, print_urls = print_urls, in_play = in_play)
    })
    parallel::stopCluster(cl)
  } else {
    # Get all results and progress the progress bar
    results <- lapply(events, function(s) {
      pb$tick()
      get_arb_single(s, print_urls = print_urls, in_play = in_play)
    })
  }

  # Create logical vector detailing whether the events are in-play or not
  in_play_vec <- event %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"all-odds-click\", \" \" ))]") %>%
    as.character() %>%
    vapply(function(s) grepl(x = s, pattern = "in-play"), logical(1), USE.NAMES = FALSE)


  for (i in seq_along(results)) {
    results[[i]] <- c(results[[i]], "in_play" = in_play_vec[2*i - 1])
  }

  # Only keep arbs
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

  # Format stake to be reduced decimal places.
  # Note that we must convert from factor first.
  # This is also where we convert the rownames to an
  # outcome column
  printed_results <- printed_results %>% purrr::map(function(s) {
    rn <- rownames(s$best_choice)
    s$best_choice <- s$best_choice %>%
      dplyr::mutate(
        Stake = round(as.numeric(as.character(Stake)), 2)
      )
    s$best_choice$Outcome <- rn
    s$best_choice <- s$best_choice %>%
      dplyr::select(Outcome, dplyr::everything())
    s
  })

  # Sort by highest win
  arb_order <- printed_results %>%
    lapply(function(s) s$Win) %>% unlist() %>%
    sort(decreasing = TRUE) %>% names()

  printed_results <- printed_results[arb_order]

  # Set new class of return object and invisibly return it
  class(printed_results) <- "arb"
  invisible(printed_results)
}

# Set globals
utils::globalVariables(c("Outcome", "Stake"))
