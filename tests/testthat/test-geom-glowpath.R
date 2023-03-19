test_that("geom_glowline works", {
  p <- ggplot2::ggplot(
    ggplot2::economics_long[1:50, ],
    ggplot2::aes(date, value01, colour = variable)
  ) +
    geom_glowline()
  expect_snapshot(ggplot2::layer_data(p))
})
