Baboon =read.csv(file="/Users/zhuyihao/Desktop/Data_Analytics_Project/Dataset_Chacma_Baboon_Vocal_Repertoire.csv", header = TRUE)

#Making Sex and Age Category numerical
Baboon$Sex <- as.numeric(Baboon$Sex)
Baboon$Age.Category <- as.numeric(Baboon$Age.Category)

#Extract Female and Man dataframe
Male = Baboon[which(Baboon$Sex == 2),]
Male <- as.data.frame(Male)
Female = Baboon[which(Baboon$Sex == 1),]
Female <- as.data.frame(Female)

#Checking the dataset
dim(Baboon)
str(Baboon)
summary(Baboon)

#normalization
normalization <- function(x){
  return ((x-min(x))/(max(x) - min(x)))
}
Male[4:122] = normalization(Male[4:122])
Female[4:122] = normalization(Female[4:122])

#Data correlation test
Male_update = data.frame(Male[,'Duration'],Male[,'DFA1.st'],Male[,'DFA2.st'],
                         Male[,'DFB3.mean'],Male[,'DFB4.pr'],Male[,'Diff.mean'],
                         Male[,'Ampratio.1'],Male[,'Ampratio.2'],Male[,'Ampratio.2'],
                         Male[,'F3.pr'],Male[,'PF..st'],Male[,'CS.mean'])

Female_update = data.frame(Female[,'Duration'],Female[,'DFA1.st'],Female[,'DFA2.st'],
                         Female[,'DFB3.mean'],Female[,'DFB4.pr'],Female[,'Diff.mean'],
                         Female[,'Ampratio.1'],Female[,'Ampratio.2'],Female[,'Ampratio.2'],
                         Female[,'F3.pr'],Female[,'PF..st'],Female[,'CS.mean'])
data_cor_male <- cor(Male_update)
data_cor_female <- cor(Female_update)
library(corrplot)
corrplot(corr = data_cor_male,method = 'color',addCoef.col = "grey")
corrplot(corr = data_cor_female,method = 'color',addCoef.col = "grey")


#Kmeans Analysis
#Use different number of features (118,38,19,9)
Male_119 = Male[4:122]
Male_119 = scale(Male_119[,-1])
Female_119 = Female[4:122]
Female_119 = scale(Female_119[,-1])

Male_38 = Male[4:42]
Male_38 = scale(Male_38[,-1])
Female_38 = Female[4:42]
Female_38 = scale(Female_38[,-1])

Male_19 = Male[4:23]
Male_19 = scale(Male_19[,-1])
Female_19 = Female[4:23]
Female_19 = scale(Female_19[,-1])

Male_9 = Male[4:13]
Male_9 = scale(Male_9[,-1])
Female_9 = Female[4:13]
Female_9 = scale(Female_9[,-1])
#Get the summation of within class distance for each k
K = 2:10
set.seed(22)

Within_dis_Male_119 = sapply(K, function(k){
  kmeans(Male_119,centers = k)$tot.withinss
})
plot(K,Within_dis_Male_119,col = 2, type = "l", xlab = "number of K", ylab = "Within sum of squares")

Within_dis_Female_119 = sapply(K, function(k){
  kmeans(Female_119,centers = k)$tot.withinss
})
plot(K,Within_dis_Female_119,type = "l", xlab = "number of K", ylab = "Within sum of squares")

#Use Silhouette Coefficient to get the best K
library(fpc)
sw_male_119 = sapply(K, function(k){
  cluster.stats(dist(Male_119),kmeans(Male_119,centers = k)$cluster)$avg.silwidth
})
sw_male_19 = sapply(K, function(k){
  cluster.stats(dist(Male_19),kmeans(Male_19,centers = k)$cluster)$avg.silwidth
})
sw_male_38 = sapply(K, function(k){
  cluster.stats(dist(Male_38),kmeans(Male_38,centers = k)$cluster)$avg.silwidth
})
sw_male_9 = sapply(K, function(k){
  cluster.stats(dist(Male_9),kmeans(Male_9,centers = k)$cluster)$avg.silwidth
})
plot(K,sw_male_119,col=2,type = "b",xlab = "number of clusters",ylab = "average silhouette width",xlim = c(2,10),ylim = c(0,1))
lines(K,sw_male_38,col=3,type="b")
lines(K,sw_male_19,col=4,type="b")
lines(K,sw_male_9,col=5,type="b")
legend("topright",pch =c(15,15,15,15),legend=c("M119","M38","M19","M9"),col=c(2,3,4,5),bty="n")


sw_female_119 = sapply(K, function(k){
  cluster.stats(dist(Female_119),kmeans(Female_119,centers = k)$cluster)$avg.silwidth
})
sw_female_19 = sapply(K, function(k){
  cluster.stats(dist(Female_19),kmeans(Female_19,centers = k)$cluster)$avg.silwidth
})
sw_female_38 = sapply(K, function(k){
  cluster.stats(dist(Female_38),kmeans(Female_38,centers = k)$cluster)$avg.silwidth
})
sw_female_9 = sapply(K, function(k){
  cluster.stats(dist(Female_9),kmeans(Female_9,centers = k)$cluster)$avg.silwidth
})
plot(K,sw_female_119,col=2,type = "b",xlab = "number of clusters",ylab = "average silhouette width",xlim = c(2,10),ylim = c(0,1))
lines(K,sw_female_38,col=3,type="b")
lines(K,sw_female_19,col=4,type="b")
lines(K,sw_female_9,col=5,type="b")
legend("topright",pch =c(15,15,15,15),legend=c("F119","F38","F19","F9"),col=c(2,3,4,5),bty="n")

#Hierachical clustering
library(NbClust)
library(stats)
hc_m <- hclust(dist(Male[4:122],method = "euclidean"), method = "ward.D2")
plot(hc_m_2)
plot(hc_m,hang = -0.01, cex = 0.7)

hc_f <- hclust(dist(Female[4:122],method = "euclidean"), method = "ward.D2")
hc_f
plot(hc_f,hang = -0.01, cex = 0.7)

#Based on clustering result we get from K-means, we find that K=2 is best fitting the model
clusters_m = cutree(hc_m,k=2)
table(clusters_m)
plot(hc_m,cex = 0.7, hang = -0.01)
rect.hclust(hc_m,k=2,border = "red")
clusters_f = cutree(hc_f,k=2)
table(clusters_f)
plot(hc_f,cex = 0.7, hang = -0.01)
rect.hclust(hc_f,k=3,border = "red")
#Kmeans Visualization
fit_1 = kmeans(Male_119,2)
fit_2 = kmeans(Female_119,4)
plot(Male_119,col=fit_1$cluster)
plot(Female_119,col=fit_2$cluster)
#ggplot
library(ggfortify)
library(ggplot2)
autoplot(kmeans(Male_119,2),data=Male_119,label=TRUE,label.size=3,frame=TRUE)
autoplot(kmeans(Female_119,3),data=Female_119,label=TRUE,label.size=3,frame=TRUE)


#KNN
Baboon[5:122] = normalization(Baboon[5:122])
# Classify the Sex of Baboon based on vocal reportoire
ind <- sample(2, nrow(Baboon), replace=TRUE, prob=c(0.75, 0.25))
KNNtrain <- Baboon[ind==1,]
KNNtest <- Baboon[ind==2,]
sqrt(118)

library(class)
KNNpred <- knn(train = KNNtrain[4:122], test = KNNtest[4:122], cl = KNNtrain$Sex, k = 10)
table(KNNtest$Sex,KNNpred)

# Classify the Age of Baboon based on vocal reportoir
ind_age = sample(2, nrow(Baboon), replace = TRUE, prob = c(0.75, 0.25))
train_age = Baboon[ind_age==1,]
test_age = Baboon[ind_age==2,]

KNNpred_age = knn(train = train_age[4:122], test = test_age[4:122], cl = train_age$Age.Category, k = 10)
table(test_age$Age.Category,KNNpred_age)









