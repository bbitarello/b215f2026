#make logo
library(wordcloud2)
words<-c("population", "frequency", "distribution", "probability", "precision", "accuracy", "histogram", "continuous", "discrete", "bar", "inference", "analysis", "sampling", "random", "categorical", "numerical", "hypothesis", "confidence interval", "standard error")
set.seed(10)
freqs<-sample((sort(unique(demoFreq$freq))[-c(1:7)]), size = 19, replace = T)
wordFreq<-data.frame(word=words, freq=freqs)
rownames(wordFreq)<-words
wordcloud2::letterCloud(data=wordFreq, word = "R")


# make schedule

library(tidyvese)
library(gt)