test_that("make_graph() makes a graph", {
  chart_data <- data$A2599049K
  plot <- make_graph(chart_data)

  expect_is(plot, "ggplot")
})
