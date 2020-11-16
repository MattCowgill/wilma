test_that("make_graph() makes a graph", {
  chart_data <- dplyr::filter(data, .data$series_id == "A84423050A")
  plot <- make_graph(chart_data)

  expect_is(plot, "ggplot")
})
