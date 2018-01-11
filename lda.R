library(topicmodels)
library(tm)
library(ggplot2)
library(quanteda)
library(wordcloud)
library(RColorBrewer)

# Pre-processing
myCorpus <- corpus(textfile("technology201501.csv", 
                            textField = "title"))
myDfm <- dfm(myCorpus, ignoredFeatures = stopwords("english"), 
             stem = TRUE, removeNumbers = TRUE, removePunct = TRUE, 
             removeSeparators = TRUE)
myDfm <- removeFeatures(myDfm, c("reddit", "redditors", 
                                 "redditor", "nsfw", "hey", "vs", "versus", 
                                 "ur", "they'r", "u'll", "u.", "u", "r", "can", 
                                 "anyone", "will", "amp", "http", "just"))
sparsityThreshold <- round(ndoc(myDfm) * (1 - 
                                            0.99999))
myDfm2 <- trim(myDfm, minDoc = sparsityThreshold)
myDtm <- quantedaformat2dtm(myDfm2)
save(myDfm2, file = "dfm1.RData")
save(myDtm, file = "dtm1.RData")
nfeature(myDfm2)
dim(myDfm2)

# Create training set and test set
set.seed(42)
samp <- sample(ndoc(myDfm2), 9000, replace = FALSE)
myDfm.train <- myDfm2[samp, ]
myDfm.test <- myDfm2[-samp, ]
save(myDfm.train, file = "dfmtrain.RData")
save(myDfm.test, file = "dfmtest.RData")

# Gibbs with different k's
best.model0.Gibbs <- lapply(seq(5, 55, by = 10), 
                            function(k) {
                              LDA(quantedaformat2dtm(myDfm.train), k, 
                                  method = "Gibbs", list(seed = 123))
                            })
save(best.model0.Gibbs, file = "LDAGibbs-by10.RData")
best.model.perp0.Gibbs <- as.data.frame(as.matrix(lapply(best.model0, 
                                                         perplexity, quantedaformat2dtm(myDfm.test))))
best.model.perp.df0.Gibbs <- data.frame(topics = c(seq(5, 
                                                       55, by = 10)), P = as.numeric(as.matrix(best.model.perp0.Gibbs)))
ggplot(best.model.perp.df0.Gibbs, aes(x = topics, 
                                      y = P)) + xlab("Number of topics") + ylab("Perplexity") + 
  geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, 
                                                               size = 14)) + theme(axis.title.y = element_text(size = 14, 
                                                                                                               angle = 90)) + ggtitle("Perplexity of Gibbs models")

# VEM with different k's
best.model0.vem <- lapply(seq(5, 55, by = 10), 
                          function(k) {
                            LDA(quantedaformat2dtm(myDfm.train), k, 
                                method = "VEM", list(seed = 123))
                          })
save(best.model0.vem, file = "LDAVEM-by10.RData")
best.model.perp0.vem <- as.data.frame(as.matrix(lapply(best.model0.vem, 
                                                       perplexity, quantedaformat2dtm(myDfm.test))))
best.model.perp.df0.vem <- data.frame(topics = c(seq(5, 
                                                     55, by = 10)), P = as.numeric(as.matrix(best.model.perp0.vem)))
ggplot(best.model.perp.df0.vem, aes(x = topics, 
                                    y = P)) + xlab("Number of topics") + ylab("Perplexity") + 
  geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, 
                                                               size = 14)) + theme(axis.title.y = element_text(size = 14, 
                                                                                                               angle = 90)) + ggtitle("Perplexity of VEM models")

# Model Selection Plots
png(filename = "ldaperpGibbsVEM0.png")
require(gridExtra)
plot1 <- ggplot(best.model.perp.df0.Gibbs, aes(x = topics, 
                                               y = P)) + xlab("Number of topics") + ylab("Perplexity") + 
  geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, 
                                                               size = 14)) + theme(axis.title.y = element_text(size = 14, 
                                                                                                               angle = 90)) + ggtitle("Perplexity of Gibbs models")
plot2 <- ggplot(best.model.perp.df0.vem, aes(x = topics, 
                                             y = P)) + xlab("Number of topics") + ylab("Perplexity") + 
  geom_line() + theme_bw() + theme(axis.title.x = element_text(vjust = -0.25, 
                                                               size = 14)) + theme(axis.title.y = element_text(size = 14, 
                                                                                                               angle = 90)) + ggtitle("Perplexity of VEM models")
grid.arrange(plot1, plot2, ncol = 2, heights = c(0.5, 
                                                 0.5), widths = c(0.485, 0.515))
dev.off()

# Model Fitting with Gibbs for January
LDAModel1 <- LDA(quantedaformat2dtm(myDfm2), 25, 
                 "Gibbs", list(iter = 4000, burnin = 2000, 
                               seed = 123))
save(LDAModel1, file = "LDAGibbs-01-25topics.RData")
terms(LDAModel1, 10)

# Topic Proportion
load("LDAGibbs-01-25topics.RData")
theta.df <- posterior(LDAModel1)$topics
theta <- as.data.frame(cbind(1:25, apply(theta.df, 
                                         2, mean)))
theta <- theta[order(-theta$V2), ]
df = data.frame(table(topics(LDAModel1)))
df = df[order(-df$Freq), ]
theta$V1 <- as.character(theta$V1)
theta$V1 <- factor(theta$V1, levels = theta$V1)
png(filename = "topicrank1bar.png")
ggplot(theta, aes(x = theta[, 1], y = theta[, 
                                            2])) + geom_bar(stat = "identity", fill = "lightgreen") + 
  coord_cartesian(ylim = c(0.0397, 0.0404)) + 
  xlab("Topic") + ylab("Topic Proportion") + 
  ggtitle("Popularity by Topic in January")
dev.off()

# Wordcloud
terms <- as.data.frame(t(posterior(LDAModel1)$terms))
df <- data.frame(freq = terms[, 21] * 10000, word = rownames(terms))
pal <- brewer.pal(8, "Dark2")
png("wordcloud_topic21.png", width = 800, height = 800)
wordcloud(df$word, df$freq, scale = c(7, 0.5), 
          min.freq = 8, max.words = Inf, random.order = FALSE, 
          rot.per = 0.15, colors = pal)
dev.off()

# Prediction
newCorpus <- corpus(textfile("technology201503.csv", 
                             textField = "title"))
newDfm <- dfm(newCorpus, ignoredFeatures = stopwords("english"), 
              stem = TRUE, removeNumbers = TRUE, removePunct = TRUE, 
              removeSeparators = TRUE)
newDfm <- removeFeatures(newDfm, c("reddit", "redditors", 
                                   "redditor", "nsfw", "hey", "vs", "versus", 
                                   "ur", "they'r", "u'll", "u.", "u", "r", "can", 
                                   "anyone", "will", "amp", "http", "just"))
sparsityThreshold <- round(ndoc(newDfm) * (1 - 
                                             0.99999))
newDfm2 <- trim(newDfm, minDoc = sparsityThreshold)
newDfm.pred <- newDfm2[50, ]
pred <- posterior(LDAModel1, newDfm.pred)$topics
pred.t <- as.data.frame(t(pred))
pred.t <- cbind(topic = factor(rownames(pred.t)), 
                pred.t)
pred.o <- pred.t[order(-pred.t[, 2]), ]
pred.o[, 1] <- as.character(pred.o[, 1])
pred.o[, 1] <- factor(pred.o[, 1], levels = pred.o[, 
                                                   1])
png(filename = "predict.png")
ggplot(pred.o, aes(x = pred.o[, 1], y = pred.o[, 
                                               2])) + geom_bar(stat = "identity", fill = "lightgreen") + 
  coord_cartesian(ylim = c(0.03, 0.11)) + xlab("Topic") + 
  ylab("Topic Probability") + ggtitle("Prediction of a Samsung Document")
dev.off()

t201503 <- read.csv("technology201503.csv", stringsAsFactors = F)
t201503[50, 1]

# Monitoring of topic Samsung
library(grid)
df <- data.frame(rank = c(3, 11, 7, 1, 3, 19, 
                          24, 11), month = c(1:8))
png("samsung.png")
ggplot(df, aes(x = month, y = rank)) + xlab("Month") + 
  ylab("Rank") + geom_line(colour = "blue", 
                           linetype = "solid", size = 1.5) + theme_bw() + 
  theme(axis.title.x = element_text(vjust = -0.25, 
                                    size = 14)) + theme(axis.title.y = element_text(size = 14, 
                                                                                    angle = 90)) + scale_y_continuous(trans = "reverse") + 
  geom_point(colour = "blue", size = 4, shape = 21, 
             fill = "white") + theme(panel.background = element_rect(fill = "aliceblue")) + 
  ggtitle("Ranking of Topic Samsung \n  01/2015 to 08/2015") + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  geom_text(aes(label = rank), hjust = 1.5, 
            vjust = -0.25)
dev.off()