#' Print method for class arb
#'
#' An S3 method for printing objects of class \code{arb}. This will print the
#' \code{best_choice} elements from
#'
#' Prints the the best choices of arbitrage opportunities.
#'
#' @param arb An object of class \code{arb} - usually the output of a call to
#' \code{get_arbs()}
#'
#' @param ... Arguments passed to \code{base::print.default()}.
#'
#' @method print arb
#' @export


print.arb <- function(arb, ...) {

  if (length(arb) == 0) {
    cat("\nNo arbitrage opportunities found")
  } else {

    cat(crayon::bold("\n------------------------------------------------------"))
    cat(crayon::bold("\nThe following arbitrage opportunities were found: \n"))
    cat(crayon::bold("------------------------------------------------------\n\n\n"))
    for (i in seq_along(arb)) {

      arb[i][[1]]$best_choice$Stake <-
        arb[i][[1]]$best_choice$Stake %>%
        as.character() %>%
        as.numeric() %>%
        formatC(digits = 2, flag = "0", format = "f") %>%
        (function(s) paste0("$", s))

      cat(arb[i][[1]]$title, "\n")
      if(arb[i][[1]]$in_play) message("This match is in-play\n") else cat("\n")
      base::print.data.frame(arb[i][[1]]$best_choice, ...)
      cat("\n\n$100 staked returns: \n")
      cat("$", arb[i][[1]]$Win, sep = "", fill = TRUE)
      cat("\n")
      cat("------------------------------------------------------")
      cat("\n")
    }

  }

}
