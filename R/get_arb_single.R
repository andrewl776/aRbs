#' Find arbitrage opportunity information for an www.oddschecker.com event
#'
#' Find (among other information) the implied total probability of all listed
#' outcomes, Pr(Omega), and if an arb is available, the best bookie combination
#' to exploit the arb.
#'
#' This function assumes that the listed outcomes are the only ones that are possible.
#' Sometimes bookies may not offer odds on a realistic outcome, often if they feel like
#' it is extremely likely to happen and therefore would offer next to no returns for
#' any punter. This may mean that an outcome isn't listed and thus the function assumes it
#' isn't possible. Therefore, the results of this function may be incorrect and assume
#' a huge arbs is possible, when it isn't. It is therefore required that the user check
#' the given outcomes given in the results, to ensure they form a complete set. Often, this
#' is obvious however, as they are simply win, draw or lose.
#'
#' @param event A www.oddschecker.com event URL, given as a string. This will be of the form:
#' "https://www.oddschecker.com/football/english/championship/bournemouth-v-reading/winner"
#'
#' @param full A logical indicating whether or not the best bookie choices should
#' still be returned for the event, even if there is no arb available.
#'
#' @param print_urls Logical. Should the url of the event(s) be printed to the console while
#' searching for arbitrage opportunities?
#'
#' @export

get_arb_single <- function(event, full = FALSE, print_urls = FALSE) {

  # For debugging
  if (print_urls) {print.default(event)}

  # Find event title
  title <- xml2::read_html(event) %>%
    rvest::html_nodes("h1") %>%
    rvest::html_text()

  event <- stringr::str_remove(event, pattern = "https://www.oddschecker.com/")
  footy_odds <- oddschecker2(event)   # possible error here when we have an event that doesn't seem to go to a page (likely it has just been removed)

  # Bug where event no longer exists means oddschecker2(event) returns an empty
  # df (which in itself is a fix). Therefore, to avoid causing a breakage here
  # and further down the line:
  if (identical(footy_odds, data.frame())) {
    return(list(
      "title" = title,
      "p_omega" = Inf,
      "Win" = 0.1,
      "Arb_Opp" = FALSE,
      "best_choice" = data.frame()
    ))
  }

  # Shift columns for when outcome column is taken as first bookie column

  n_shifts <- is.na(names(footy_odds)) %>% sum

  # TODO: Deal with outcome column (sometimes given as rownames and sometimes as one of the columns!)
  if (n_shifts != 0) {
    for (i in 1:n_shifts) {names(footy_odds) <- names(footy_odds) %>% data.table::shift()}
    names(footy_odds)[1] <- "Outcome"
    #outcomes <- footy_odds[, 1]
  }

  #else {
  #  outcomes <- rownames(footy_odds)
  #}

  # Remove null divider column

  footy_odds <- footy_odds %>%
    dplyr::select(
      names(footy_odds)[names(footy_odds) >= 1]
    )

  # Remove non-odds rows

  # Find rows that are truly odds
  odds_TF <- footy_odds %>%
    apply(MARGIN = 1, FUN =
            function(s) {
              !all(
                !grepl(s, pattern = "\\d/\\d")
              )
            }
    )


  outcomes <- names(odds_TF)[odds_TF]


  footy_odds <- footy_odds %>%
    dplyr::filter(
      odds_TF
    )

  names(footy_odds) <- names(footy_odds) %>%
    as.list() %>%
    purrr::map(remove_form) %>%
    unlist()

  # Find implied probability of each outcome for each bookie
  bookie_outcome_p <-
    footy_odds[, names(footy_odds) != "Outcome"] %>%
    # Remove null columns (we use c(., "") so that we don't get NAs)
    dplyr::select_if(~ dplyr::n_distinct(c(., "")) != 1) %>%
    purrr::map(implied_probability)


  # Find implied probability for each bookie of each outcome
  outcome_bookie_p <- bookie_outcome_p %>%
    data.table::transpose()

  # Find the minimum implied probabilities (best odds) for each outcome in Omega
  best_p <- outcome_bookie_p %>%
    lapply(function(s) {
      min(c(s, 1), na.rm = TRUE)
    })



  # Find bookie name for best oddds
  which_bookie <- outcome_bookie_p %>%
    lapply(function(s) names(bookie_outcome_p)[which.min(s)])



  # Find the odds we want from that bookie
  which_bookie_odds <- which_bookie %>%
    lapply(function(s) footy_odds[, s])

  which_bookie_odds_single <- character(0)

  # TODO: Convert for loops to map()
  for (i in seq_along(which_bookie_odds)) {
    which_bookie_odds_single <- c(which_bookie_odds_single, which_bookie_odds[[i]][i])
  }

  which_bookie_odds_single <- which_bookie_odds_single %>%
    as.list()


  # Find implied probability of Omega
  impl_prob_omeg <- best_p %>%
    unlist() %>%
    sum()


  # Calculate optimum stakes and win from $100 bet (note: all elements of win should be equal if we round)
  stake <- implied_probability(unlist(which_bookie_odds_single)) * (100/impl_prob_omeg)

  win <- stake * {which_bookie_odds_single %>%
      vapply(function(s) 1/implied_probability(s), FUN.VALUE = 1)}



  #### TODO - Work out why we get to here with no odds? Even if there is no arb opp, we should still be able to find the best odds.

  # Combine results to nice list
  best_choice <- mapply(
    c,
    which_bookie,
    which_bookie_odds_single,
    stake,
    SIMPLIFY=FALSE
    # Convert from list to data.frame:
  ) %>%
    unlist() %>%
    matrix(nrow = 3) %>%
    t() %>%
    data.frame(row.names = outcomes %>% remove_form())

  # Add on dollar symbol and convert to currency. Use {base} here as
  # dplyr removes rownames
  # Round for neat reporting AFTER calculations. Convert to currency (note: encoding errors could occur here..)
  # best_choice$stake <- best_choice$stake %>%
  #   as.numeric() %>%
  #   formatC(37.1, digits = 2, flag = "0", format = "f") %>%
  #   paste0("$", .)



  # Supply names
  colnames(best_choice) <- c("Bookie",
                             "Odds",
                             "Stake")



  # Return
  if (impl_prob_omeg < 1 | full) {
    return(list(
      "title" = title,
      "p_omega" = impl_prob_omeg,
      "Win" = unique(round(win, 2)),
      "Arb_Opp" = impl_prob_omeg < 1,
      "best_choice" = best_choice
    ))
  } else {
    list(
      "title" = title,
      "p_omega" = impl_prob_omeg,
      "Win" = unique(round(win, 2)),
      "Arb_Opp" = impl_prob_omeg < 1
    )
  }

}
