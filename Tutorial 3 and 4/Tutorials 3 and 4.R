#Tutorials 3 & 4
#Karen Hwang
#09/10/2020


#Exercise 1.1 - Regression: 
install.packages("readxl")

library(readxl)

data_reg3 <- read_excel("example_linear-reg_dataset1.xls", sheet = "Hoja3")
lmHeight3 = lm(height~age + playtime, data = data_reg3)

summary(lmHeight3)

plot(lmHeight3$residuals, ylab='Residuals')
#Residuals appear to be random




#Exercise 2.1 - Clustering: 
set.seed(786)
data_seeds <- read.csv("seeds_dataset.txt" ,sep = '\t',header = FALSE)
feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(data_seeds) <- feature_name

#check if everything is numerical and for any missing/NA values 
str(data_seeds)
any(is.na(data_seeds))

#Exclude the type.of.seed column from the dataset in order to do clustering
seeds_label <- data_seeds$type.of.seed
data_seeds$type.of.seed <- NULL

#Scale all the columns
data_seeds_norm <- as.data.frame(scale(data_seeds))
summary(data_seeds_norm)

#Build distance matrix
dist_mat <- dist(data_seeds_norm, method = 'euclidean')

#Hierarchical clustering - complete linkage
hclust_comp <- hclust(dist_mat, method = 'complete')
plot(hclust_comp)

cut_comp <- cutree(hclust_comp, k = 3)
plot(hclust_comp)
rect.hclust(hclust_comp , k = 3, border = 2:6)

abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dplyr))
data_seeds_comp_cl <- mutate(data_seeds, cluster = cut_comp)
count(data_seeds_comp_cl,cluster)

suppressPackageStartupMessages(library(ggplot2))
ggplot(data_seeds_comp_cl, aes(x=area, y = perimeter, color = factor(cluster))) + geom_point()
table(data_seeds_comp_cl$cluster,seeds_label)

#Hierarchal clustering - average linkage
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)

abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dplyr))
data_seeds_avg_cl <- mutate(data_seeds, cluster = cut_avg)
count(data_seeds_avg_cl,cluster)

suppressPackageStartupMessages(library(ggplot2))
ggplot(data_seeds_avg_cl, aes(x=area, y = perimeter, color = factor(cluster))) + geom_point()
table(data_seeds_avg_cl$cluster,seeds_label)





#Exercise 2.2 - PCA
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

library(ggbiplot)

#Categorize these cars into US, Japanese and European:
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(mtcars), groups=mtcars.country) + ggtitle("PCA of mtcars dataset")






#Exercise 3.1 - Classification:
require(ISLR)
head(Smarket)

#Boxplot
par(mfrow=c(1,8))
par(mar=c(1,1,1,1))
for(i in 2:4) {
  boxplot(Smarket[,i], main=names(Smarket)[i])
}

#Correlation plot
library(corrplot)
correlations <- cor(Smarket[,2:4])
corrplot(correlations, method="circle")

#Density distribution plot
library(caret)
x <- Smarket[,2:4]
y <- Smarket[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#Logistic regression model & summary
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3, data = Smarket, family = binomial)
summary(glm.fit)

#Predictions and fitter probabilities
glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]

#Classify up or down
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
attach(Smarket)
table(glm.pred,Direction)

#Classification Rate
mean(glm.pred == Direction)
