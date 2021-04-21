#' @keywords internal
#' @importFrom dplyr `%>%`

plot_downloads <- function(packages = "aRbs", per = "day", from = NULL, labels = FALSE) {

  per <- stringr::str_remove(per, "s$")


  packages_df <- packages %>%
    lapply(function(package) {
      dlstats::cran_stats(package) %>%
        dplyr::mutate("package" = paste0("{", package, "}"))
    }) %>%
    dplyr::bind_rows()

  if(!(is.null(from))) {
    if (!grepl("^[0-9]{4}-[0-9]{2}$", from)) {stop("`from` must be a string representing a date in format YYYY-MM")}
    packages_df <- packages_df %>% dplyr::filter(start > lubridate::ym(from))
  }

  packages <- paste0("{", packages, "}")

  package_plot <- packages_df %>%
    dplyr::mutate(dl_per_day = downloads/as.numeric(difftime(end, start, units = per)),
                  package = package) %>%
    ggplot2::ggplot(ggplot2::aes(x = start, y = dl_per_day, fill = package)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_discrete("Package") +
   # ggplot2::scale_x_date(date_minor_breaks = "2 weeks", date_labels = "%b %d") +
    ggplot2::labs(title = paste0("CRAN Downloads per Day for ",
                                 paste0(packages, collapse = ", "),
                                 " Package(s)"),
                  x = "Month",
                  y = "Downloads per Day") +
    ggplot2::theme(legend.position = "bottom")

  if (labels) {
    package_plot <- package_plot + ggplot2::geom_label(ggplot2::aes(label = round(dl_per_day, 1)))
  }

  package_plot
}
