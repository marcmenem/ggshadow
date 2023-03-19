test_that("geom_shadowpoint works", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
  p <- p + geom_shadowpoint()
  expect_snapshot(ggplot2::layer_data(p))
})
