long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)


context("lat long to UTM")
test_that("lat long to UTM", {
  
  skip_if_not_installed("proj4")
  
  
  UTM = latlong2UTM(coord)
  expect_is(UTM, "data.frame")
  
  expect_equal(dim(UTM), c(3, 5))
  
  for (i in c(1, 2, 4, 5)) expect_is(UTM[,i], "numeric")
  expect_is(UTM[,3], "character")
  expect_equal(unique(UTM[,3]), "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
})