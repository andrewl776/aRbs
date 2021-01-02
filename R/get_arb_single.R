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
#' @param print_urls Logical. Should the URL of the event(s) be printed to the console while
#' searching for arbitrage opportunities?
#'
#' @export

get_arb_single <- function(
  event, full = FALSE,
  print_urls = FALSE,
  in_play = TRUE
  ) {

  # For debugging
  if (print_urls) {print.default(event)}

  event <- stringr::str_remove(event, pattern = "https://www.oddschecker.com/")
  odds <- oddschecker2(event, in_play = in_play)   # possible error here when we have an event that doesn't seem to go to a page (likely it has just been removed)

  odds_df <- odds$odds
  title <- odds$title

  # Bug where event no longer exists means oddschecker2(event) returns an empty
  # df (which in itself is a fix). Therefore, to avoid causing a breakage here
  # and further down the line:
  if (identical(odds_df, data.frame())) {
    return(list(
      "title" = title,
      "p_omega" = Inf,
      "Win" = 0.1,
      "Arb_Opp" = FALSE,
      "best_choice" = data.frame(),
      "event" = event
    ))
  }

  # Shift columns for when outcome column is taken as first bookie column

  n_shifts <- is.na(names(odds_df)) %>% sum

  # TODO: Deal with outcome column (sometimes given as rownames and sometimes as one of the columns!)
  if (n_shifts != 0) {
    for (i in 1:n_shifts) {names(odds_df) <- names(odds_df) %>% data.table::shift()}
    names(odds_df)[1] <- "Outcome"
    #outcomes <- odds_df[, 1]
  }

  # Remove null divider column...
  odds_df <- odds_df %>%
    dplyr::select(
      names(odds_df)[names(odds_df) >= 1]
    ) %>%
    # ... and bookies not offering any odds
    dplyr::select_if(~ dplyr::n_distinct(c(., "")) != 1)

  # Find implied probability of each outcome for each bookie
  bookie_outcome_p <-
    odds_df %>%
    # Remove null columns (we use c(., "") so that we don't get NAs)
    purrr::map(implied_probability)


  # Find implied probability for each bookie of each outcome
  outcome_bookie_p <- bookie_outcome_p %>%
    data.table::transpose()

  # Find the minimum implied probabilities (best odds) for each outcome in Omega
  best_p <- outcome_bookie_p %>%
    lapply(function(s) {
      min(c(s, 1), na.rm = TRUE)
    })


  # Find bookie name for best odds
  which_bookie <- outcome_bookie_p %>%
    lapply(function(s) names(bookie_outcome_p)[which.min(s)])



  # Find the odds we want from that bookie
  which_bookie_odds <- which_bookie %>%
    lapply(function(s) odds_df[, s])

  which_bookie_odds_single <- character(0)

  # TODO: Convert for loops to map()
  for (i in seq_along(which_bookie_odds)) {
    which_bookie_odds_single <- c(which_bookie_odds_single, which_bookie_odds[[i]][i])
  }

  which_bookie_odds_single <- which_bookie_odds_single %>%
    as.list()


  # Find implied probability of Omega
  impl_prob_omeg <- best_p %>%
    as.numeric() %>%
    unlist() %>%
    sum()


  # Calculate optimum stakes and win from $100 bet (note: all elements of win should be equal if we round)
  stake <- implied_probability(unlist(which_bookie_odds_single)) * (100/impl_prob_omeg)

  returns_multiplier <- {which_bookie_odds_single %>%
      vapply(function(s) 1/implied_probability(s), FUN.VALUE = 1)}
  returns_multiplier <- returns_multiplier[!is.na(returns_multiplier)]

  win <- stake * returns_multiplier


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
    data.frame(row.names = rownames(odds_df))

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
      "best_choice" = best_choice,
      "event" = event
    ))
  } else {
    list(
      "title" = title,
      "p_omega" = impl_prob_omeg,
      "Win" = unique(round(win, 2)),
      "Arb_Opp" = impl_prob_omeg < 1,
      "event" = event
    )
  }

}
