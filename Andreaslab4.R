library("MASS")

#2.1
cars <- read.csv2("cars.csv")

We should scale because we want to compare different variables, 
they measure different things in different units

#2.2
carsN <- scale(cars[,3:8])
d <- dist(carsN)
new <- cmdscale(d, 2)

write.csv(new, file = "carsNew.csv")

#2.3
plot(d, dist(new),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for metric MDS"))
abline(a=0,b=1)

#2.4
res <- isoMDS(d, k=2, p=2)
coords <- res$points
write.csv(coords, file = "carsNew2.csv")
plot(d,dist(coords),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for non-metric MDS"))
abline(a=0,b=1)
#2.5 
bo

#2.6
res2 <- isoMDS(d, k=2, p=1)
coords2 <- res2$points
sh2 <- Shepard(d, coords2)
plot(d, dist(coords2),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for non-metric MDS","Minkowski distance p = 1"))
lines(sh2$x, sh2$yf, type = "S", col = "blue")

#2.7
init <- data.frame(x1 = runif(38, -1,1), x2 = runif(38, -1,1))
res3 <- isoMDS(d, k=2, p=2, maxit = 500, y = as.matrix(init))
coords3 <- res3$points
sh3 <- Shepard(d, coords3)
plot(d, dist(coords3),pch ="+",col = "red",
     xlab=c("Old dist"),ylab = c("New dist"),
     main =c("Shepard plot for non-metric MDS",
             "Minkowski distance p = 2, random starting points"))
lines(sh3$x, sh3$yf, type = "S", col = "blue")

#2.8
comment