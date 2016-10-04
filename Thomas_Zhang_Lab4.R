library("MASS")
cars <- read.csv2("cars.csv")

carsN <- scale(cars[,3:8])
d <- dist(carsN)
new <- cmdscale(d, 2)
write.csv(new, file = "carsNew.csv")
plot(d, dist(new),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for metric MDS"))
abline(a=0,b=1)
res <- isoMDS(d, k=2, p=2)
coords <- res$points
write.csv(coords, file = "carsNew2.csv")
plot(d,dist(coords),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for non-metric MDS"))
sh <- Shepard(d,coords)
lines(sh$x, sh$yf, type = "S", col = "blue")
res2 <- isoMDS(d, k=2, p=1)
coords2 <- res2$points
write.csv(coords2, file = "carsNew3.csv")
sh2 <- Shepard(d, coords2)
plot(d, dist(coords2),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for non-metric MDS","Minkowski distance p = 1"))
lines(sh2$x, sh2$yf, type = "S", col = "blue")
init <- data.frame(x1 = runif(38, -1,1), x2 = runif(38, -1,1))
res3 <- isoMDS(d, k=2, p=2, maxit = 500, y = as.matrix(init))
coords3 <- res3$points
write.csv(coords3, file = "carsNew4.csv")
sh3 <- Shepard(d, coords3)
plot(d, dist(coords3),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for non-metric MDS",
             "Minkowski distance p = 2, random starting points"))
lines(sh3$x, sh3$yf, type = "S", col = "blue")
## NA
