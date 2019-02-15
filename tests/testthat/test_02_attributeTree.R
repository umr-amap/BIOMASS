set.seed(2)

xy <- data.frame(x = runif(10, min = 0, max = 200), y = runif(10, min = 0, max = 200))

# cut the plot in multiple part
coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
coord[1:4, ] <- coord[1:4, ] + 5000
coord[5:8, ] <- coord[5:8, ] + 6000
corner <- rep(c(1, 2, 4, 3), 2)
plot <- rep(c("plot1", "plot2"), each = 4)

cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)


# The attribute the plot
plot <- rep(c("plot1", "plot2"), 5)

context("attribute the trees")
test_that("attribute the trees", {
  a <- attributeTree(xy, plot, cut)

  expect_is(a, "character")
  expect_length(a, nrow(xy))

  expect_equal(
    a,
    c(
      "plot1_0_1", "plot2_1_0", "plot1_1_1", "plot2_0_0",
      "plot1_1_0", "plot2_1_1", "plot1_0_1", "plot2_1_0",
      "plot1_0_0", "plot2_1_0"
    )
  )
})

test_that("attribute the trees error", {
  expect_error(attributeTree(xy[1:5, ], plot, cut), "Your plot vector")
  expect_error(attributeTree(xy, plot, as.matrix(cut)), "data frame")

  plot[plot == "plot2"] <- "plot1"

  xy <- rbind(xy, c(0, 300))
  xy <- rbind(xy, c(300, 0))
  xy <- rbind(xy, c(300, 300))

  plot <- rep(c("plot1", "plot2"), each = 4, length.out = nrow(xy))
  
  expect_warning(attributeTree(xy, plot, cut), "not assigned")
  
  sub = suppressWarnings(attributeTree(xy, plot, cut))
  sub = rev(sub)[1:3]

  expect_true(all(is.na(sub)))
})
