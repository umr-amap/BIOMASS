xy <- data.frame(x = runif(200, min = 0, max = 200), y = runif(200, min = 0, max = 200))


# cut the plot in multiple part
coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
coord[1:4, ] <- coord[1:4, ] + 5000
coord[5:8, ] <- coord[5:8, ] + 6000
corner <- rep(c(1, 2, 4, 3), 2)
plot <- rep(c("plot1", "plot2"), each = 4)

cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)


# The attribute the plot
plot <- rep(c("plot1", "plot2"), 100)
# attribute the trees to subplot
attributeTree(xy, plot, cut)
