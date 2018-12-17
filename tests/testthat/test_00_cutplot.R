coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000

corner <- c(1, 2, 4, 3)
plot <- rep("plot1", 4)


context("cut plot")
test_that("cut plot", {
  expect_is(cutPlot(coord, plot, corner), "data.frame")

  expect_length(names(cutPlot(coord, plot, corner)), 7)

  expect_true(all(table(cutPlot(coord, plot, corner)$subplot) == 4))
  expect_true(all(table(cutPlot(coord, plot, corner, gridsize = 20)$subplot) == 4))

  coord <- rbind(coord, data.frame(X = c(0, 100, 0, 100), Y = c(0, 0, 100, 100)) + 6000)
  corner <- rep(c(1, 2, 4, 3), 2)
  plot <- c(plot, rep("plot2", 4))

  cut <- cutPlot(coord, plot, corner, dimX = c(200, 100), dimY = c(200, 100))

  expect_equal(unique(cut$plot), c("plot1", "plot2"))
  expect_true(all(table(cut$subplot) == 4))
  expect_true(all(table(cut$plot) == c(16, 4)))
})

test_that("cut plot error", {
  expect_error(cutPlot(coord, plot[1:3]))
  expect_error(cutPlot(coord, plot, corner[1:3]))
  expect_error(cutPlot(coord, plot, corner, gridsize = c(100, 10)))
  expect_error(cutPlot(coord, plot, corner, dimX = 10))
  expect_error(cutPlot(coord, plot, corner, dimY = 10))
  expect_error(cutPlot(coord, plot, corner, dimY = c(200, 10)))
  expect_error(cutPlot(coord, plot, corner, dimX = c(200, 10)))
})
