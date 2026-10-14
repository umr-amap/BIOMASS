data("NouraguesPlot201")
data("NouraguesCoords")
data("NouraguesTrees")

test_that("plotPolygonFit works with each method", {
  for (m in c("exact", "affine", "procrustes")) {
    fit <- plotPolygonFit(
      point_data = NouraguesCoords, method = m,
      proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
      plot_col = "Plot", draw_plot = FALSE)
    expect_equal(nrow(fit$corner_coord), 16)
    expect_equal(nrow(fit$polygon), 4)
    expect_false(anyNA(fit$corner_coord[, c("x_proj", "y_proj")]))
  }
})

test_that("plotPolygonFit averages repeated measurements with method 'exact'", {
  fit <- plotPolygonFit(
    point_data = NouraguesPlot201, method = "exact",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    draw_plot = FALSE)
  expect_equal(nrow(fit$corner_coord), 4)
  expect_length(fit$sd_coord, 1)
})

test_that("plotPolygonFit projects longitude/latitude coordinates", {
  fit_ll <- plotPolygonFit(
    point_data = NouraguesCoords, method = "procrustes",
    lonlat_col = c("Long", "Lat"), rel_col = c("Xfield", "Yfield"),
    plot_col = "Plot", draw_plot = FALSE)
  fit_proj <- plotPolygonFit(
    point_data = NouraguesCoords, method = "procrustes",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    plot_col = "Plot", draw_plot = FALSE)
  expect_true(grepl("+proj=utm +zone=22", fit_ll$UTM_code, fixed = TRUE))
  expect_false(anyNA(fit_ll$corner_coord[, c("x_proj", "y_proj", "long", "lat")]))
  # Same corners as with the projected coordinates, to within a few metres
  ll <- fit_ll$corner_coord[order(fit_ll$corner_coord$plot_ID, fit_ll$corner_coord$x_rel, fit_ll$corner_coord$y_rel), ]
  pr <- fit_proj$corner_coord[order(fit_proj$corner_coord$plot_ID, fit_proj$corner_coord$x_rel, fit_proj$corner_coord$y_rel), ]
  expect_lt(max(abs(ll$x_proj - pr$x_proj)), 10)
  expect_lt(max(abs(ll$y_proj - pr$y_proj)), 10)
})

test_that("plotPolygonFit checks corner counts per plot", {
  # Plots of different sizes are allowed with method 'exact'
  coords <- NouraguesCoords
  big <- coords$Plot == 213
  coords$Xfield[big] <- coords$Xfield[big] * 2
  expect_no_error(plotPolygonFit(
    point_data = coords, method = "exact",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    plot_col = "Plot", draw_plot = FALSE))

  # More than four corners in one plot with method 'exact'
  extra <- coords[coords$Plot == 201, ][1, ]
  extra$Xfield <- 50
  expect_error(plotPolygonFit(
    point_data = rbind(coords, extra), method = "exact",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    plot_col = "Plot", draw_plot = FALSE), "four corner points per plot: 201")

  # Too few corners with coordinates in one plot
  coords_na <- NouraguesCoords
  coords_na$Xutm[coords_na$Plot == 204][1:2] <- NA
  expect_error(plotPolygonFit(
    point_data = coords_na, method = "affine",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    plot_col = "Plot", draw_plot = FALSE), "'affine' method: 204")
})

test_that("plotPolygonFit warns about mirrored relative coordinates", {
  mirrored <- NouraguesCoords[NouraguesCoords$Plot == 201, ]
  mirrored$Yfield <- 100 - mirrored$Yfield
  expect_warning(plotPolygonFit(
    point_data = mirrored, method = "procrustes",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    draw_plot = FALSE), "reflection")
})

test_that("plotPolygonFit projects trees", {
  fit <- plotPolygonFit(
    point_data = NouraguesCoords, method = "procrustes",
    proj_col = c("Xutm", "Yutm"), rel_col = c("Xfield", "Yfield"),
    plot_col = "Plot", tree_data = NouraguesTrees,
    tree_rel_col = c("Xfield", "Yfield"), tree_plot_col = "Plot",
    draw_plot = FALSE)
  expect_equal(nrow(fit$tree_data), nrow(NouraguesTrees))
  expect_true(all(c("x_proj", "y_proj", "is_in_plot") %in% names(fit$tree_data)))
  expect_gt(mean(fit$tree_data$is_in_plot), 0.95)
})
