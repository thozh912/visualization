library("MASS")
cars <- read.csv2("cars.csv")

carsN <- scale(cars[,3:8])
d <- dist(carsN)
new <- cmdscale(d, 2)

plot(d, dist(new))


res <- isoMDS(d, k=2, p=2)
coords <- res$points
plot(coords)