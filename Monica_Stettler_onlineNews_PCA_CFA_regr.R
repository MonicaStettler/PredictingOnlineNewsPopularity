library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(psych)
library(ggplot2)
library(GGally)
library(stats)

### MONICA STETTLER, MAY 6, 2018
#### CSC424 - HOMEWORK 3 - PROBLEM 2
### FINAL PROJECT - PCA - ONLINE NEWS

#PCA PLOT FUNCTIONS

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}
############################################################

setwd("C:/Users/Monica/Documents/DEPAUL/424 - ADVANCED DATA ANALYSIS/FINAL PROJECT")
news = read.csv("OnlineNewsPopularity2.csv", header = TRUE)

##news = read.csv("C:/Users/Monica/Documents/DEPAUL/424 - ADVANCED DATA ANALYSIS/FINAL PROJECT/OnlineNewsPopularity.csv", header = TRUE)

str(news)
head(news)
dim(news)

#Get rid of the categorical data and id
newsNumeric = news[, c(3:13, 20:31,40:61 )]

#checking structure, check that items were removed
head(newsNumeric)
str(newsNumeric)
dim(newsNumeric)
names(newsNumeric)

#Scale entire dataset -optional, need to change PCA code below
#scaledNews = scale(newsNumeric, center = TRUE, scale = TRUE)
#scalednewsStats = summary(scaledNews)
#scalednewsStats

#write.csv(scalednewsStats, file = "scaledNews.csv")


##check NORMALITY and outliers
attach(newsNumeric)
detach(newsNumeric)
newsStats = summary(newsNumeric)
newsStats

hist(shares, col = "red")
qqnorm(shares)
describe(shares)


hist(n_tokens_title)
qqnorm(n_tokens_title)
plot(n_tokens_title, shares)

hist(n_tokens_content)
qqnorm(n_tokens_content)
plot(n_tokens_content, shares)

hist(num_hrefs)
qqnorm(num_hrefs)
plot(num_hrefs, shares)

hist(n_tokens_title)
qqnorm(n_tokens_title)
plot(n_tokens_title, shares)

hist(num_self_hrefs)
qqnorm(num_self_hrefs)
plot(num_self_hrefs, shares)    

hist(num_imgs)
qqnorm(num_imgs)
plot(num_imgs, shares)                   
             
hist(num_videos)
qqnorm(num_videos)
plot(num_videos, shares)           

hist(num_keywords)
qqnorm(num_keywords)
plot(num_keywords, shares)  

hist(average_token_length)
qqnorm(average_token_length)
plot(average_token_length, shares)  
       
hist(kw_min_min)
qqnorm(kw_min_min)
plot(kw_min_min, shares)    

hist(kw_max_min)
qqnorm(kw_max_min)
plot(kw_max_min, shares)    

hist(kw_avg_min)
qqnorm(kw_avg_min)
plot(kw_avg_min, shares)    

hist(kw_min_max)
qqnorm(kw_min_max)
plot(kw_min_max, shares)    

hist(kw_max_max)
qqnorm(kw_max_max)
plot(kw_max_max, shares)  

hist(kw_avg_max)
qqnorm(kw_avg_max)
plot(kw_avg_max, shares)    
 
hist(kw_min_avg)
qqnorm(kw_min_avg)
plot(kw_min_avg, shares)    
                              
hist(kw_max_avg)
qqnorm(kw_max_avg)
plot(kw_max_avg, shares)    

hist(kw_avg_avg)
qqnorm(kw_avg_avg)
plot(kw_avg_avg, shares)    

hist(num_self_hrefs)
qqnorm(num_self_hrefs)
plot(num_self_hrefs, shares)    

hist(self_reference_min_shares)
qqnorm(self_reference_min_shares)
plot(self_reference_min_shares, shares)    

hist(self_reference_max_shares)
qqnorm(self_reference_max_shares)
plot(self_reference_max_shares, shares)  

hist(self_reference_avg_sharess)
qqnorm(self_reference_avg_sharess)
plot(self_reference_avg_sharess, shares)    


#find outliers
mod <- lm(shares ~ ., data=newsNumeric)
car::outlierTest(mod)

#use Bartlett's test of Sphericity to check if correlations are significant and can run PCA
bartlett.test(newsNumeric)
bartlett.test(news)

#are any indep variables correlated with shares?

cor.newsNumeric = cor(newsNumeric[,1:44],newsNumeric[,45])
cor.newsNumeric
write.csv(cor.newsNumeric, file = "corr2Y.csv")

corrplot(cor.newsNumeric, method = "number")

# Run a correlation test to see how correlated the variables are.  Which correlations are significant

options("scipen"=100, "digits"=5)
MCorrTest = corr.test(newsNumeric, adjust="none")
MCorrTest

M = MCorrTest$p
M

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix

sigCorrXs = colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)
sigCorrXs

write.csv(sigCorrXs, file = "sigCorrXs.csv")

# Based on those results, I am going to pull out the variables that have no correlations 
#and too many correlations: n_unique_tokens, n_non_stop_words, n_non_stop_unique_tokens

dim(newsNumeric)
str(newsNumeric)
newsReduce <- newsNumeric[, -c(3,4,5)]
dim(newsReduce)


#PCA with prcomp
p = prcomp(newsReduce, center = TRUE, scale = TRUE)
p
summary(p)
plot(p)
abline(1,0)
print(p)
par(mar=c(2, 2, 2, 2))
plot(p)
PCA_Plot(p)
PCA_Plot_Secondary(p)
biplot(p)

rawLoadings = p$rotation %*% diag(p$sdev, nrow(p$rotation), nrow(p$rotation))
print(rawLoadings)
v = varimax(rawLoadings)
ls(v)
v
print(v, cutoff=.4, sort = T)

#Re-running with psych 
p2 = psych::principal(newsReduce, rotate="varimax", nfactors=6, normalize = TRUE, covar = FALSE, scores=TRUE)
p2
newsPCApsych = print(p2$loadings, cutoff=.4, sort = T)
write.csv(newsPCApsych, file = "newsPCApsych.csv")



################################### RUN PLOTS
plot(p2)
abline(1, 0)
summary(p2)
print(p2)
par(mar=c(2, 2, 2, 2))
plot(p2)
PCA_Plot(p2)
PCA_Plot_Secondary(p2)
biplot(p2)


###################################### USING PCA COMPONANTS IN REGRESSION

newdat<-p2$scores[,1:2]

str(news)

newsRegr <- lm(news$shares ~ p2$scores[,1] + p2$scores[,2] + news$LDA_00 + news$LDA_01 + news$LDA_02 
               + news$LDA_03 + news$LDA_04 + news$min_negative_polarity + news$min_positive_polarity
               +news$title_sentiment_polarity + news$n_tokens_title + news$num_videos + news$n_unique_tokens
              + news$n_non_stop_words + news$n_non_stop_unique_tokens + news$data_channel_is_lifestyle
               + news$data_channel_is_entertainment + news$data_channel_is_bus, news$data_channel_is_socmed
               + news$data_channel_is_tech + news$data_channel_is_world + news$weekday_is_monday + news$weekday_is_tuesday
               + news$weekday_is_wednesday + news$weekday_is_thursday + news$weekday_is_friday + news$weekday_is_saturday
               + news$weekday_is_sunday + news$is_weekend)
summary(newsRegr)

newsRegrDT <- lm(news$shares ~ p2$scores[,1] + p2$scores[,2] + news$LDA_00 + news$LDA_01 + news$LDA_02 
               + news$LDA_03 + news$LDA_04 + news$n_tokens_content + news$average_token_length
               + news$data_channel_is_entertainment + 
               + news$kw_min_max + news$kw_avg_avg + news$kw_max_avg + news$kw_avg_max + news$kw_avg_min + news$kw_min_avg)  
summary(newsRegrDT)

newsRegrDT2 <- lm(news$shares ~ p2$scores[,1] + p2$scores[,2]  
                 + news$n_tokens_content + news$average_token_length
                 + news$data_channel_is_entertainment)  
summary(newsRegrDT2)

str(news)
newsRegr2 <- lm(news$shares ~ p2$scores[,1] + p2$scores[,2] + news$LDA_00 + news$LDA_01 + news$LDA_02)

summary(newsRegr2)

newsRegr2PC <- lm(news$shares ~ p2$scores[,1] + p2$scores[,2], data = newsReduce)
summary(newsRegr2PC)

lmPC <- lm(news$shares ~ p2$scores[,1] + p2$scores[,3] + 
        p2$scores[,4] + p2$scores[,6], data=newsReduce)
summary(lmPC)

lmPCs <- lm(news$shares ~p2$scores[,1])

#FACTOR ANALYSIS
fit = factanal(newsReduce, 6)
print(fit$loadings, cutoff=.4, sort=T)

newsNumeric = news[, c(3:13, 20:31,40:61 )]

library(rpart)
newsdt <- news[,c(3:61)]
dtfit <- rpart(shares ~., data = newsdt, method = "anova")

printcp(dtfit)
plotcp(dtfit)
summary(dtfit)
print(dtfit)
plot(dtfit)
text(dtfit)

#############
attr(hbat,'variable.labels')
fullFit <- lm(shares ~ ., data=newsNumeric)
summary(fullFit)

vif(fullFit)




