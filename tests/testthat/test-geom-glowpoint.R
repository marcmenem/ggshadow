test_that("geom_glowpoint works", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
  p <- p + geom_glowpoint()
  expect_snapshot(ggplot2::layer_data(p))
})
