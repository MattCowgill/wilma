#' @import ggplot2
#' @importFrom rlang .data

make_graph <- function(chart_data,
                       start_date = min(chart_data$date),
                       end_date = max(chart_data$date)) {
  chart_data %>%
    dplyr::filter(.data$date >= start_date &
      .data$date <= end_date) %>%
    ggplot(aes(x = .data$date, y = .data$value, col = stringr::str_wrap(.data$series, 120))) +
    geom_line() +
    ggiraph::geom_point_interactive(aes(tooltip = round(.data$value, 2)),
      size = 3,
      colour = "white",
      fill = "white",
      alpha = .01
    ) +
    theme_minimal(
      base_size = 14,
      base_family = "Lato"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.direction = "vertical"
    )
}
