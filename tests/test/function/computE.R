coord1 <- cbind(KarnatakaForest$long, KarnatakaForest$lat)
require(data.table)
require(raster)
require(tictoc)
require(microbenchmark)

computeE <- function(coord) {
  ### Compute the Environmental Index (Chave et al. 2014)

  path <- folderControl("E")

  nam <- paste(path$path, "E.bil", sep = path$sep)
  RAST <- raster(nam)

  # Extract the raster value
  RASTval <- extract(RAST, coord, "bilinear")

  return(RASTval)
}



computeE1 <- function(coord) {
  path <- folderControl("E")

  # find the raster
  nam <- paste(path$path, "E.bil", sep = path$sep)
  RAST <- raster(nam)

  if (is.null(dim(coord))) {
    return(extract(RAST, matrix(coord, ncol = 2)))
  }

  # set the coord for
  coord <- as.data.table(coord)

  coord_unique <- unique(coord)
  coord_unique <- na.omit(coord_unique)

  # Extract the raster value
  coord_unique[, RASTval := extract(RAST, coord_unique, "bilinear")]

  r <- 0
  i <- 1
  while (anyNA(coord_unique$RASTval)) {
    r <- r + 5000
    coord_unique[is.na(RASTval), RASTval := sapply(extract(RAST, cbind(V1, V2), buffer = r), mean, na.rm = TRUE)]

    if (i > 8) {
      coord[coord_unique, on = c("V1", "V2"), RASTval := i.RASTval]
      stop("The coordinate n° ", paste(which(is.na(coord$RASTval)), collapse = " "), " are in a  ")
    }

    i <- i + 1
  }

  return(coord[coord_unique, on = c("V1", "V2"), RASTval])
}






res <- microbenchmark::microbenchmark("original" = computeE(coord1[1:10, ]), "modified" = computeE1(coord1[1:10, ]))
plot(res)

tic()
invisible(computeE1(coord1))
toc()

sequence <- seq(2, nrow(coord1), by = 100)
time <- matrix(0, nrow = length(sequence), ncol = 3, dimnames = list(NULL, c("size", "original", "modified")))
time[, 1] <- sequence
i <- 1
for (a in sequence) {
  tic()
  computeE(coord1[1:a, ])
  time1 <- toc()

  tic()
  computeE1(coord1[1:a, ])
  time2 <- toc()

  time[i, 2:3] <- c(time1$toc - time1$tic, time2$toc - time2$tic)
  i <- i + 1
}

plot(time[, 1], time[, 2], type = "l", col = "green")
lines(time[, 1], time[, 3], col = "red")


tic()
getValues(RAST)[which.min(replace(distanceFromPoints(RAST, coord_unique[is.na(RASTval), .(long, lat)]), is.na(RAST), NA))]
toc()


r <- 1
tic()
resultat <- NA
while (anyNA(resultat)) {
  resultat <- sapply(extract(RAST, coord_unique[is.na(RASTval), .(long, lat)], buffer = r), mean, na.rm = TRUE)

  test <- if (anyNA(resultat)) TRUE else FALSE

  cat(resultat, " ", r, "\n")

  r <- r + 1000
}
toc()



set.seed(2)
# set projected CRS
r <- raster(ncol = 10, nrow = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10, crs = "+proj=utm +zone=1")
r[] <- 1:10
r[sample(1:ncell(r), size = 25)] <- NA
plot(r)





xy <- data.frame(x = runif(10, 1, 10), y = runif(10, 1, 10))

# use normal extract function to show that NAs are extracted for some points
extracted <- raster::extract(x = r, y = xy)





res <- microbenchmark("original" = computeE(jitter(coord1)), "modified" = computeE1(jitter(coord1)))
plot(res)
