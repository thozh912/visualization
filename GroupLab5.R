library(rpart)
library(partykit)
#library(classifly)
#library(rggobi)
library(XLConnect)
library(googleVis)
library(fields)
library(animation)
ani.options(ffmpeg= "C:\\ffmpeg-20161007-c45ba26-win64-static\\bin\\ffmpeg.exe")

olive <-read.csv("olive.csv")
olive$Region <- as.factor(olive$Region)
tree <- rpart(Region ~ palmitic + palmitoleic + stearic +
                oleic + linoleic + linolenic + arachidic +
                eicosenoic, data = olive)
plot(as.party(tree))
#classifly(olive,Region ~ palmitic + palmitoleic + stearic +
#            oleic + linoleic + linolenic + arachidic +
#            eicosenoic,rpart)
olivescaled <- scale(olive[,4:11])
olive_dist <- dist(olivescaled)
olive_dend <- hclust(olive_dist, method = "complete")
plot(olive_dend)
threeclusters <- cutree(olive_dend, k = 3)
olive$three <- as.factor(threeclusters)
#write.csv(olive,file = "oliveout.csv")
#wb = loadWorkbook("Oilcoal.xls")
#oilcoaldata <- readWorksheet(wb, sheet = 1, header = TRUE)
#oilcoaldata$Year <- as.numeric(oilcoaldata$Year)
#hehe <- gvisMotionChart(oilcoaldata,
#                       idvar='Country', timevar='Year',
#                        sizevar = "Marker.size")
#plot(hehe)

#oilp <- 100 * (oilcoaldata$Oil / (oilcoaldata$Oil + #oilcoaldata$Coal))
#preds <- cbind(oilcoaldata$Year,as.factor(oilcoaldata$Country))
#thinplate <- Tps(preds,oilp)

#saveVideo({
#  for( i in 1:176 ){
#    oneloop <- predict.Krig(thinplate, x = matrix(nrow = 8, c(rep(1965 + .25 * i,8),
#                                                   1:8)))
#    rownames(oneloop) <- levels(as.factor(oilcoaldata$Country))
#    oneloop <- cbind(oneloop, 100-oneloop)
#    oneprop = prop.table(oneloop,margin=1)
#    barplot(t(oneprop), col=c("black","brown"))
#  }
#}, video.name = "C:\\Users\\Dator\\Documents\\R_HW\\visualization\\anime.mp4",
#interval = 0.05,ani.width = 800,ani.height = 600
#)
## NA
