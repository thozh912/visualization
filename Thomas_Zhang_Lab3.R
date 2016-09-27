library(ggplot2)
library(seriation)
library(RColorBrewer)
library(slam)
library(tm)
library(wordcloud)
prodata <- read.delim("protein.txt")
pairs(prodata, panel = panel.smooth,pch = "+")
proscaled <- as.matrix(scale(prodata[,2:10]))
heatmap(proscaled, Rowv = NA, scale = "none",
        col = cm.colors(256))
heatmap(proscaled, Rowv = NULL, scale = "none",
        col = cm.colors(256))
rowdist<-dist(proscaled)
coldist<-dist(t(proscaled))

order1<-seriate(rowdist, "BBURCG")
order2<-seriate(coldist, "BBURCG")
ord1<-get_order(order1)
ord2<-get_order(order2)
reordmatr<-proscaled[rev(ord1),ord2]
heatmap(reordmatr, Colv=NA, Rowv=NA,
        labRow = prodata[rev(ord1),1],
        col=cm.colors(256))

order3<-seriate(proscaled, "PCA")
order4<-seriate(t(proscaled), "PCA")
ord3<-get_order(order3)
ord4<-get_order(order4)
reordmatr2<-proscaled[rev(ord3),ord4]
heatmap(reordmatr2, Colv=NA, Rowv=NA,
        labRow = prodata[rev(ord3),1],
        col=cm.colors(256))

data<-read.table("OneTwo.txt",header=F, sep='\n') #Read file
mycorpus <- Corpus(DataframeSource(data)) #Creating corpus (collection of text data)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
tdm <- TermDocumentMatrix(mycorpus) #Creating term-document matrix
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE) #Sum up the frequencies of each word
d <- data.frame(word = names(v),freq=v) #Create one column=names,second=frequences
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1:2)] #Create palette of colors
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=F,
          rot.per=.15, colors=pal, vfont=c("sans serif","plain"))

data2<-read.table("Five.txt",header=F, sep='\n') #Read file
mycorpus2 <- Corpus(DataframeSource(data2)) #Creating corpus (collection of text data)
mycorpus2 <- tm_map(mycorpus2, removePunctuation)
mycorpus2 <- tm_map(mycorpus2, function(x) removeWords(x, stopwords("english")))
tdm2 <- TermDocumentMatrix(mycorpus2) #Creating term-document matrix
m2 <- as.matrix(tdm2)
v2 <- sort(rowSums(m2),decreasing=TRUE) #Sum up the frequencies of each word
d2 <- data.frame(word = names(v2),freq=v2) #Create one column=names,second=frequences
wordcloud(d2$word,d2$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=F,
          rot.per=.15, colors=pal, vfont=c("sans serif","plain"))