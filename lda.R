library(topicmodels)
library(tm)
library(ggplot2)
library(sqldf)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
myCorpus <- corpus(textfile("technology201501.csv", textField = "title"))
myDfm <- dfm(myCorpus,ignoredFeatures=stopwords("english"), stem = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE)
myDfm <-removeFeatures(myDfm, c("reddit", "redditors","redditor","nsfw", "hey", "vs", "versus", "ur", "they'r", "u'll", "u.","u","r","can","anyone","will","amp","http","just"))
sparsityThreshold <- round(ndoc(myDfm) * (1 - 0.9999))
myDfm2 <- trim(myDfm, minDoc = sparsityThreshold)
myDtm <- quantedaformat2dtm(myDfm2)
save(myDfm2, file="dfm1.RData")
save(myDtm, file="dtm1.RData")
nfeature(myDfm2)
dim(myDfm2)
set.seed(42)
samp <- sample(ndoc(myDfm2),9000,replace=FALSE)
myDfm.train <- myDfm2[samp,]
myDfm.test <- myDfm2[-samp,]
save(myDfm.train, file="dfmtrain.RData")
save(myDfm.test, file="dfmtest.RData")

#Gibbs
best.model0 <- lapply(seq(5,55, by=10), function(k){LDA(quantedaformat2dtm(myDfm.train), k,method="Gibbs", list(iter=3000,seed = 123))})
save(best.model0, file = "LDAGibbs-by10.RData")

#best.model
best.model.perp0 <- as.data.frame(as.matrix(lapply(best.model0, perplexity,quantedaformat2dtm(myDfm.test))))
best.model.perp.df0 <- data.frame(topics=c(seq(5,55,by=10)), P=as.numeric(as.matrix(best.model.perp0)))
ggplot(best.model.perp.df0, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90)) + ggtitle("Perplexity of Gibbs models")

#VEM
best.model0.vem <- lapply(seq(5,55, by=10), function(k){LDA(quantedaformat2dtm(myDfm.train), k,method="VEM", list(seed = 123))})
save(best.model0.vem, file = "LDAVEM-by10.RData")

#best.model
best.model.perp0.vem <- as.data.frame(as.matrix(lapply(best.model0.vem, perplexity,quantedaformat2dtm(myDfm.test))))
best.model.perp.df0.vem <- data.frame(topics=c(seq(5,55,by=10)), P=as.numeric(as.matrix(best.model.perp0.vem)))
ggplot(best.model.perp.df0.vem, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90)) + ggtitle("Perplexity of VEM models")

png(filename="ldaperpGibbsVEM0.png")
#par(mfrow= c(2,2))
require(gridExtra)
plot1 <-ggplot(best.model.perp.df0, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90)) + ggtitle("Perplexity of Gibbs models")
plot2 <-ggplot(best.model.perp.df0.vem, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90))  + ggtitle("Perplexity of VEM models")
grid.arrange(plot1, plot2, ncol=2, heights=c(0.5, 0.5), widths=c(0.485,0.515))
dev.off()
#15-35

best.model <- lapply(seq(15,35, by=1), function(k){LDA(quantedaformat2dtm(myDfm.train), k,method="Gibbs", list(iter=3000,seed = 123))})
save(best.model, file = "LDAGibbs-by1.RData")
#best.model
best.model.perp <- as.data.frame(as.matrix(lapply(best.model, perplexity,quantedaformat2dtm(myDfm.test))))
best.model.perp.df <- data.frame(topics=c(seq(15,35,by=1)), P=as.numeric(as.matrix(best.model.perp)))
ggplot(best.model.perp.df, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity of the model") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90))
best.model.perp.df[which.min(best.model.perp.df$P),]
best.model.perp.df

load("LDAGibbs-by1.RData")
load("LDAVEM-by1.RData")

png(filename="ldaperpGibbsVEM1.png")
#par(mfrow= c(2,2))
require(gridExtra)
plot1 <-ggplot(best.model.perp.df, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90)) + ggtitle("Perplexity of Gibbs models")
plot2 <-ggplot(best.model.perp.df.vem, aes(x=topics, y=P)) + xlab("Number of topics") + ylab("Perplexity") + geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, size = 14)) +  theme(axis.title.y = element_text(size = 14, angle=90))  + ggtitle("Perplexity of VEM models")
grid.arrange(plot1, plot2, ncol=2, heights=c(0.5, 0.5), widths=c(0.485,0.515))
dev.off()


                                        #VEM
best.model0 <- lapply(seq(10,40, by=10), function(k){LDA(quantedaformat2dtm(myDfm.train), k,method="VEM")})


#best.model
best.model.perp0 <- as.data.frame(as.matrix(lapply(best.model0, perplexity,quantedaformat2dtm(myDfm.test))))
best.model.perp.df0 <- data.frame(topics=c(seq(10,40,by=10)), P=as.numeric(as.matrix(best.model.perp0)))

best.model.vem <- lapply(seq(15,35, by=1), function(k){LDA(quantedaformat2dtm(myDfm.train), k=k, control=list(seed = 123))})
save(best.model.vem, file = "LDAVEM-by1.RData")
#best.model
best.model.perp.vem <- as.data.frame(as.matrix(lapply(best.model.vem, perplexity,quantedaformat2dtm(myDfm.test))))
best.model.perp.df.vem <- data.frame(topics=c(seq(15,35,by=1)), P=as.numeric(as.matrix(best.model.perp.vem)))

#a <- best.model.perp.df[which.min(best.model.perp.df$P),1]
LDA2 <- LDA(quantedaformat2dtm(myDfm2), 13,method="Gibbs", list(iter=3000,seed = 123))
save(LDA2, file = "LDAGibbs1.RData")
terms(LDA2,10)
df=data.frame(table(topics(LDA2)))
df = df[order(-df$Freq),]
df

#wordcloud
myCorpus <- corpus(textfile("technology201502.csv", textField = "title"))
myDfm <- dfm(myCorpus,ignoredFeatures=stopwords("english"), stem = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE)
myDfm <-removeFeatures(myDfm, c("reddit", "redditors","redditor","nsfw", "hey", "vs", "versus", "ur", "they'r", "u'll", "u.","u","r","can","anyone","will","amp","http","just"))
sparsityThreshold <- round(ndoc(myDfm) * (1 - 0.99999))
myDfm2 <- trim(myDfm, minDoc = sparsityThreshold)
LDA2 <- LDA(quantedaformat2dtm(myDfm2), 25)
terms(LDA2,10)
df=data.frame(table(topics(LDA2)))
df = df[order(-df$Freq),]
df
terms <- as.data.frame(t(posterior(LDA2)$terms))
df <- data.frame(freq=terms[,15]*10000,word=rownames(terms))
pal <- brewer.pal(8,"Dark2")
png("wordcloud_topic15.png", width=800,height=800)
wordcloud(df$word,df$freq, scale=c(7,.5),min.freq=8,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


#LDA2@beta[,1]
#terms <- as.data.frame(t(posterior(LDA2)$terms))
#head(terms)


#prediction
newCorpus <- corpus(textfile("technology201503.csv", textField = "title"))
newDfm <- dfm(newCorpus,ignoredFeatures=stopwords("english"), stem = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE)
newDfm <- removeFeatures(newDfm, c("reddit", "redditors","redditor","nsfw", "hey", "vs", "versus", "ur", "they'r", "u'll", "u.","u","r","can","anyone","will","amp","http","just"))
sparsityThreshold <- round(ndoc(newDfm) * (1 - 0.99999))
newDfm2 <- trim(newDfm, minDoc = sparsityThreshold)
newDfm.pred <- newDfm2[50,]
posterior(LDA2,newDfm.pred)$topics
t201503 <- read.csv('technology201503.csv', stringsAsFactors = F)
t201503[50,1]
newDfm.pred <- newDfm2[41:50,]
posterior(LDA2,newDfm.pred)$topics
t201503 <- read.csv('technology201503.csv', stringsAsFactors = F)
t201503[41:50,1]

                                        #wordcloud
myCorpus <- corpus(textfile("technology201502.csv", textField = "title"))
myDfm <- dfm(myCorpus,ignoredFeatures=stopwords("english"), stem = TRUE, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE)
myDfm <-removeFeatures(myDfm, c("reddit", "redditors","redditor","nsfw", "hey", "vs", "versus", "ur", "they'r", "u'll", "u.","u","r","can","anyone","will","amp","http","just"))
sparsityThreshold <- round(ndoc(myDfm) * (1 - 0.99999))
myDfm2 <- trim(myDfm, minDoc = sparsityThreshold)
LDA2 <- LDA(quantedaformat2dtm(myDfm2), 25)
terms(LDA2,10)
df=data.frame(table(topics(LDA2)))
df = df[order(-df$Freq),]
df

topics.df <- data.frame(n=c(1:length(topics(LDA2))),topics(LDA2))
topic2 <- topics.df[topics.df$topics.LDA2.==2,1]
head(topic2)


myText <- read.csv("technology201502.csv", stringsAsFactors = FALSE)
myText <- myText[topic2,]
Corpus <- Corpus(VectorSource(myText$title))
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus,tolower)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus,PlainTextDocument)
corpus_clean <- tm_map(Corpus, removeWords, c(stopwords('english'),"reddit", "redditors","redditor","nsfw", "hey", "vs", "versus", "ur", "they'r", "u'll", "u.","u","r","can","anyone","will","amp","http","just","The"))
corpus_stem<- tm_map(corpus_clean, stemDocument)
wordcloud(corpus_stem, max.words = 100, random.order = FALSE)

Tdm <- TermDocumentMatrix(corpus_stem)
mat <- as.matrix(Tdm)
v <- sort(rowSums(mat),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#table(d$freq)
pal <- brewer.pal(8,"Dark2")
png("wordcloud_netneu2.png", width=800,height=800)
wordcloud(d$word,d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


                                        #other topics
topics.df <- data.frame(n=c(1:length(topics(LDA2))),topics(LDA2))
topic17 <- topics.df[topics.df$topics.LDA2.==17,1]
myText <- read.csv("technology201502.csv", stringsAsFactors = FALSE)
myText <- myText[topic17,]
Corpus <- Corpus(VectorSource(myText$title))
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus,tolower)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus,PlainTextDocument)
corpus_clean <- tm_map(Corpus, removeWords, c(stopwords('english'),"reddit", "redditors","redditor","nsfw", "hey", "vs", "versus", "ur", "they'r", "u'll", "u.","u","r","can","anyone","will","amp","http","just","The"))
corpus_stem<- tm_map(corpus_clean, stemDocument)
#wordcloud(corpus_stem, max.words = 100, random.order = FALSE)

Tdm <- TermDocumentMatrix(corpus_stem)
mat <- as.matrix(Tdm)
v <- sort(rowSums(mat),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#table(d$freq)
pal <- brewer.pal(8,"Dark2")
png("wordcloud_topic4.png", width=800,height=800)
wordcloud(d$word,d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()
