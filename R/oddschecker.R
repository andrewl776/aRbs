library(tidyverse)
library(gambleR)
library(data.table)
library(devtools)



## TODO:
# Allow for non-fractional odds! Not all of them are in the right format (do we just switch to decimal??) 
# Update: We don't need to! gamblr::oddschecker() does this or us

# Work out how much to bet on each outcome (is th)
# Functionise fetch_p_omega so that it is just a wrapper for all the other functions
# Find way to get subdomains all events
# Find way to filter subdomains to only events (or alternatively anti-break code for non-event subdomains)
# Query apis (look at betfair, abettor packages)

# Find whether a string ends in Win, Draw, Lose characters

## Example event: 


"https://www.oddschecker.com/football/belarus/premier-league/dinamo-brest-v-isloch/winner)"


# Set-up functions
ends_in_form <- function(string) {
  string %>% 
    grepl(pattern = "[w, d, l]{5}$")
}

remove_form <- function(string) {
  stringr::str_sub(
    string = string,
    1,
    end = stringr::str_length(string) - 5 * ends_in_form(string)
  )
}


# Fetch results -------------------------------------------------------------

# Fetch implied probability of all possible events

get_arb_single <- function(event = "https://www.oddschecker.com/footall", full = FALSE) {
  
  
  # Find event title
  title <- xml2::read_html("https://www.oddschecker.com/football/germany/bundesliga/bayern-munich-v-borussia-monchengladbach/winner") %>% 
    rvest::html_nodes("h1") %>%
    rvest::html_text()
  
  event <- str_remove(event, pattern = "https://www.oddschecker.com/")
  footy_odds <- gambleR::oddschecker(event)
  
  
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
    select(
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
    filter(
      odds_TF
    )
  
  names(footy_odds) <- names(footy_odds) %>% as.list() %>% lapply(remove_form) %>% unlist()
  
  # Find implied probability of each outcome for each bookie
  bookie_outcome_p <- 
    footy_odds[, names(footy_odds) != "Outcome"] %>% 
    # Remove null columns (we use c(., "") so that we don't get NAs)
    dplyr::select_if(~ dplyr::n_distinct(c(., "")) != 1) %>%
    lapply(gambleR::implied.probability)
  
  
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
  for (i in seq_along(which_bookie_odds)) {
    which_bookie_odds_single <- c(which_bookie_odds_single, which_bookie_odds[[i]][i])
  }
  
  which_bookie_odds_single <- which_bookie_odds_single %>% 
    as.list()
  
  
  # Find implied probability of Omega
  impl_prob_omeg <- best_p %>% 
    unlist() %>%
    sum() 
  
  
  # Calculate optimum stakes and win from £100 bet (note: all elements of win should be equal if we round)
  stake <- implied.probability(unlist(which_bookie_odds_single)) * (100/impl_prob_omeg)
  
  win <- stake * {which_bookie_odds_single %>% 
      vapply(function(s) 1/implied.probability(s), FUN.VALUE = 1)}
  
  # Round for neat reporting AFTER calculations. Convert to currency (note: encoding errors could occur here..)
  stake <- stake %>% 
    round(2) %>% 
    formattable::currency(symbol = "?")
  
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





# Scrape and clean links ------------------------------------------------------------



scrapelinks <- function(url) {
  
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
  return(tibble(link = link_, url = url_))
}



clean_links <- function(links) {
  links %>% 
    filter(str_detect(url, pattern = "winner$")) %>% 
    select(url) %>%
    mutate(url = ifelse(str_detect(url, pattern = "^\\/"), 
                        paste0("https://www.oddschecker.com", url), 
                        url)) %>% 
    as.list() %>% 
    data.table::transpose()
}




# Fetch results for all links ------------------------------------------------------------------

get_arbs <- function(event_list = "https://www.oddschecker.com/football") {
  
  # Find individual events
  events <- event_list %>% 
    scrapelinks() %>% 
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
  results <- lapply(events, function(s) {
    pb$tick()
    get_arb_single(s)
  })
  
  n_arb_opps <- vapply(results, function(s) s$Arb_Opp, logical(1)) %>% 
    sum()
  
  
  for (i in seq_along(results)) {
    results[[i]] <- c(results[[i]], "in_play" = in_play_vec[2*i - 1])
  }
  
  printed_results <- results[vapply(results, function(s) s$Arb_Opp, logical(1))] 
  
  if (n_arb_opps == 0) {
    cat("\nNo arbitrage opportunities found")
  } else {
    
    cat(crayon::bold("\nThe following arbitrage opportunities were found: \n"))
    cat("------------------------------------------------------\n")
    cat("------------------------------------------------------\n\n")
    for (i in seq_along(printed_results)) {
      cat(printed_results[i][[1]]$title, "\n")
      if(printed_results[i][[1]]$in_play) message("This match is in-play\n") else cat("\n")
      print(printed_results[i][[1]]$best_choice)
      cat("\n\n£100 staked returns: \n")
      cat(printed_results[i][[1]]$Win)
      cat("\n")
      cat("------------------------------------------------------")
      cat("\n")
    }
    
  }
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
















