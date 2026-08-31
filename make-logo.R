#make logo
library(wordcloud2)
source("slides/bb_theme.R")
library(tidyverse)
library(gt)
library(plotly)

words<-c("population", "frequency", "distribution", "probability", "precision", "accuracy", "histogram", "continuous", "discrete", "bar", "inference", "analysis", "sampling", "random", "categorical", "numerical", "hypothesis", "confidence interval", "standard error")
set.seed(10)
freqs<-sample((sort(unique(demoFreq$freq))[-c(1:7)]), size = 19, replace = T)
wordFreq<-data.frame(word=words, freq=freqs)
rownames(wordFreq)<-words
wordcloud2::letterCloud(data=wordFreq, word = "R")


# make schedule





# make fig for logistics

df<-tibble(Module = "M1", Activity = c("DataCamp"),Prop = c(70,30), Group = c("outside of class", "in class"))

df<-bind_rows(df, tibble(Module = "M1", Activity = c("Theory"),Prop = c(95,5), Group = c("outside of class", "in class")))

df<-bind_rows(df, tibble(Module = "M1", Activity = c("Practice"),Prop = c(50,50), Group = c("outside of class", "in class")))

#plot grades over the years


grades<-clipr::read_clip_tbl()
grades<-grades|> 
  filter(course!="b398") 
grades<-grades |>
  mutate(pts = ifelse(grade_. > 100, 100, grade_.))
hist(grades$pts)

med<-median(grades$pts)
q1<-quantile(grades$pts)[["25%"]]
q3<-quantile(grades$pts)[["75%"]]
p1<-grades |>
  ggplot(aes( x = course, y=pts)) + geom_jitter(width = 0.1) + 
  geom_hline(yintercept = med, col = "orange") +
  bb_theme()
ggplotly(p1)
grades |> filter(course %in% c("b216", "b215")) |> mutate(group=cut(pts, breaks=11)) |> group_by(grade_bmc) |> tally(sort=T) |> mutate(prop = n/sum(n))

grades |> filter(course %in% c("b216", "b215")) |> mutate(group=cut(pts, breaks=11)) |> group_by(group) |> tally(sort=T) |> mutate(prop = n/sum(n))

p2<-grades |> filter(course %in% c("b216", "b215")) |>
  ggplot(aes(x = pts, group=course, color=course)) +
  stat_ecdf(geom = "step") +
  bb_theme()

allx<-seq(from=0, to=100)
x<-10 #proctored
y<-seq(from=1, to = 100, by=0.01)
#If we’re assuming that they get a perfect score on the unproctored assignments, then their final grade is a function of their proctored grades (y) and the weight of the proctored grades (x):

#https://acbart.github.io/2026/04/19/proctored-grades/
final_grade = (100 - x) + x * y / 100

df<-tibble(x = x, y = y, final = final_grade)

x<-50 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))


x<-60 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))


x<-30 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))


x<-20 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))

x<-10 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))


x<-70 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))


x<-80 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))

x<-90 #proctored
final_grade = (100 - x) + x * y / 100
df<-bind_rows(df, tibble(x = x, y = y, final = final_grade))

df<-df |> mutate(bmc = ifelse(final>=94, 4, ifelse(final>=90, 3.7, ifelse(final>=87, 3.3, ifelse(final>=83, 3.0, ifelse(final>=80, 2.7, ifelse(final>=76, 2.3, ifelse(final>=70, 2.0, ifelse(final>=67,1.7, ifelse(final>=64, 1.3, ifelse(final>=61, 1, 0)))))))))))
df<-df |> mutate(proctored_weight = x, proctored_score = y, final, bmc)
df$bmc<-factor(df$bmc, levels =  c("4", "3.7", "3.3","3", "2.7", "2.3","2", "Below merit", "Fail"))

#library(paletteer)

#mycols<-rev(c4a("isfahan1"))
old <- update_theme(palette.colour.discrete = scales::pal_viridis())

#here
df2<-expand.grid(proc_weight=seq(0,100, length.out=500), proc_score=seq(0,100, length.out=500))
df2<-df2 |> mutate(final= (100 - proc_weight) + proc_weight * proc_score / 100)
#df2<-df2 |> mutate(bmc = ifelse(final>=94, 4, ifelse(final>=90, 3.7, ifelse(final>=87, 3.3, ifelse(final>=83, 3.0, ifelse(final>=80, 2.7, ifelse(final>=76, 2.3, ifelse(final>=70, 2.0, ifelse(final>=60, "Below merit", "Fail")))))))))
df2<-df2 |> mutate(bmc = ifelse(final>=94, 4, ifelse(final>=90, 3.7, ifelse(final>=87, 3.3, ifelse(final>=83, 3.0, ifelse(final>=80, 2.7, ifelse(final>=76, 2.3, ifelse(final>=70, 2.0, ifelse(final>=67,1.7, ifelse(final>=64, 1.3, ifelse(final>=61, 1, 0)))))))))))
#df2$bmc<-factor(df2$bmc, levels =  c("4", "3.7", "3.3","3", "2.7", "2.3","2", "Below merit", "Fail"))
nrow(df2) # 250000
df2<-df2 |> distinct()
nrow(df2) # 250000
#p<-ggplot(df2, aes(x =proc_weight, y = proc_score, fill = bmc))+
#  scale_y_continuous(breaks = seq(0, 100, by=5))+
#  scale_x_continuous(breaks = seq(0, 100, by=5))+
#  bb_theme()

#p<-p+
#  geom_area(col="white")

#next

p1<-ggplot(df2, aes(x = proc_weight, y = proc_score, z = bmc))+
  bb_theme()
p1+  geom_contour_filled()
mycols<-c4a("viridis",n = 11)
p1<-ggplot(df2, aes(x = proc_weight, y = proc_score, z = final, group=bmc))+
  bb_theme()
p1<-p1+  geom_contour_filled(breaks = rev(c(0,61,64,67,70,76, 80, 83, 87, 90, 94,100))) #works
p1<-p1 +  scale_fill_manual(" ", labels = rev(c(0,1,1.3,1.7,2, 2.3, 2.7, 3, 3.3, 3.7, 4)), values = mycols)
p1<-p1 + xlab("Proctored weight") + ylab("Proctored score")
p1<-p1 +
  annotate("label", x = 13, y = 85, label = "4", size = 3, fontface = "bold", fill = "white", alpha = 0.7)
p1<-p1 +
  annotate("label", x = 27, y = 72, label = "3.7", size = 3, fontface = "bold", fill = "white", alpha = 0.7)

p1<-p1+
    annotate("label", x = 34, y = 66, label = "3.3", size = 3, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+annotate("label", x = 40, y = 62, label = "3", size = 3, fontface = "bold", fill = "white", alpha = 0.7) 
  p1<-p1+annotate("label", x = 45, y = 59, label = "2.7", size = 3, fontface = "bold", fill = "white", alpha = 0.7) 
  
  p1<-p1+annotate("label", x = 49, y = 55, label = "2.3", size = 2.5, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+annotate("label", x = 54, y = 50, label = "2", size = 3, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+annotate("label", x = 58, y = 46, label = "1.7", size = 2.5, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+annotate("label", x = 62, y = 44, label = "1.3", size = 1.8, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+annotate("label", x = 63, y = 40, label = "1", size = 1.8, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+annotate("label", x = 75, y = 25, label = "0", size = 3, fontface = "bold", fill = "white", alpha = 0.7)
  p1<-p1+ scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0))
  p1<-p1+scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0))
  p1<-p1 + geom_vline(xintercept = 60,  linetype = "dashed", color = "black", linewidth = 0.4)
  p1 + geom_vline(xintercept = 70,  linetype = "dashed", color = "black", linewidth = 0.4)
  
  ggsave("~/Documents/proctored_thresh.png")
  