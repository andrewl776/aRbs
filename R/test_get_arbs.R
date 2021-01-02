#' @keywords internal

test_get_arbs <- function(n = 10, in_play = FALSE, sub_domain = "football") {
  devtools::load_all()
  cat("Running get_arbs() ", n, " times...\n", fill = TRUE)

  replicate(n = n, {
    s_time <- system.time(
      arbs <- get_arbs(in_play = in_play,
                       event = paste0("https://www.oddschecker.com/", sub_domain))
    )
    cat("Number of arb opps found: ", length(arbs), "\nTime taken: ", s_time[[3]], " seconds.", "\n", fill = TRUE)
  })
}
