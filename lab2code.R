library(sp)
library(XLConnect)
library(gclus)
library(aplpack)
library(portfolio)
library(lattice)
library(TSP)
jobs <- read.table("jobs.txt",sep="\t",header = TRUE)
map.market(id = jobs$Country, area = jobs$SI, group = jobs$Group,
           color = jobs$Fin, lab = c("group" = TRUE, "id" = TRUE),
           main = "Tree map: area = SI jobs, color = Fin jobs")
ones <- rep(1,26)
facesframe <- cbind(jobs[,2:7],ones,ones,jobs[,8:10],
                    ones,ones,ones,ones)
faces(facesframe, labels = jobs$Country,nrow.plot = 3)
#There are basically two clusters
countrydist <- dist(facesframe)
neworder <- order.single(countrydist)
segmentframe <- jobs[neworder,2:10]
par(mfrow = c(1,1))
stars(segmentframe,draw.segments = TRUE,labels = as.character(jobs$Country[neworder]),
      key.loc = c(-3,10))

swecounties <- readRDS("SWE_adm1.rds")
temp <- swecounties@data
temp$NAME_1[12] <- "Örebro"
names(temp)[6] <- "County"
wb <- loadWorkbook("BO0501C6.xlsx")
swehousedata = readWorksheet(wb,sheet = "BO0501C6")
newframe <- merge(temp, swehousedata,sort = FALSE)
swecounties@data$X2005=newframe$X2005
swecounties@data$X2010=newframe$X2010
par(mfrow = c(1,2))
spplot(swecounties, zcol="X2005")
spplot(swecounties, zcol="X2010")
