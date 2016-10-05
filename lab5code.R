library(rpart)
library(partykit)
library(classifly)
library(rggobi)
olive <-read.csv("olive.csv")
olive$Region <- as.factor(olive$Region)
tree <- rpart(Region ~ palmitic + palmitoleic + stearic +
                oleic + linoleic + linolenic + arachidic +
                eicosenoic, data = olive)
plot(as.party(tree))
classifly(olive,Region ~ palmitic + palmitoleic + stearic +
            oleic + linoleic + linolenic + arachidic +
            eicosenoic,rpart)