## SE and CI demo


library(infer)
library(tidyverse)
human_genes<-read_csv("~/Documents/GitHub/DataSetsb215/data-raw/human_genes.csv")
human_genes<-human_genes |> select(gene,size)

pop_mean<-mean(human_genes$size)
pop_sd<-sd(human_genes$size)

pop_SE_samp50<-pop_sd/sqrt(50)

#take a sample

mysamp50<-rep_sample_n(human_genes, size = 50, replace = F)
mysamp50<-mysamp50 |> ungroup() |> select(-replicate)

#sample stats
sampleStats<-tibble(mean=mean(mysamp50$size), sd=sd(mysamp50$size))
sampleStats<-sampleStats |> mutate(EstSE=sd/sqrt(50), RealSE=pop_SE_samp50)

#manual bootstrapping:

manualBoots<-rep_sample_n(mysamp50, size = 50, replace=T, reps=1000)
manualBootsSum<-manualBoots |> summarise(meanS=mean(size))
sampleStats<-sampleStats |> mutate(ManualBootsSE=sd(manualBootsSum$meanS))

# boots function
autBoots<-boot(human_genes,statistic = mymean, R=1000)


#using tidymodels
library(tidymodels)

boots <- bootstraps(human_genes, times = 100)
statistic <- function(splits) {
  x <- analysis(splits)
  meanL <- mean(x$size)
}

# iterate over each bootstrap sample and compute statistic
boots$meanL <- map_dbl(boots$splits, statistic)

#cool that works now using a sample

genesS<-human_genes |> sample_n(size = 100, replace = F)

mean(genesS$size)

bootsS <- bootstraps(genesS, times = 2000)
bootsS$meanL <- map_dbl(bootsS$splits, statistic)
summary(bootsS$meanL)
quantile(bootsS$meanL, probs=c(0.05, 0.5, 0.95))
