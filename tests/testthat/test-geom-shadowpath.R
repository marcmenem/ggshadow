test_that("geom_shadowpath works", {
  p <- ggplot2::ggplot(
    ggplot2::economics_long[1:50, ],
    ggplot2::aes(date, value01, colour = variable)
  )

  p_shadowpath <- p + geom_shadowpath()
  expect_snapshot(ggplot2::layer_data(p_shadowpath))

  p_shadowline <- p + geom_shadowline()
  expect_snapshot(ggplot2::layer_data(p_shadowline))
})
