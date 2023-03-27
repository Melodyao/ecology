rm(list = ls())
#Section I: Read data and data pre-Process – preliminary. 
#1
#read env and fish data,summarize fish abundance data by sites
#combine env and total fish to a new data frame named as "env_fish". 
library("ade4")
library("tidyverse")
data(doubs)
env <- rownames_to_column(doubs$env, var = "site")
total_fish <- rowSums(doubs$fish)                     
env_fish <- cbind(env,total_fish)                       

#2
#visualize the features of the new env_fish set
env_fish %>% 
  gather(-total_fish, key = "value", value = "env") %>%
  ggplot(aes(x = env, y = total_fish)) +
  geom_point()+
  geom_smooth(se = FALSE) +
  facet_wrap(~value, scales = "free") +
  theme_bw()                                          

#find  linear relationships between environmental variables and the total fish abundance
plot(env_fish$dfs~env_fish$total_fish)
c <- lm(env_fish$dfs~env_fish$total_fish)
abline(c)                                                #plot the correlation of two specific values

#3
#delete no fish sites
#remove all rows where any column contains an outlier.
env_fish <- filter(env_fish,total_fish!=0)              
a <- boxplot.stats(env_fish$slo)$out 
x_0 <- env_fish [-which(env_fish$slo %in% a ),]  
b <- boxplot.stats(env_fish$flo)$out
x_1 <- x_0 [-which(env_fish$flo %in% b ),]
c <- boxplot(env_fish$pH)$out
x_2 <- x_1[-which(env_fish$pH  %in% c ),] 
d <- boxplot(env_fish$har)$out
x_3 <- x_2[-which(env_fish$har %in% d ),] 
e <-boxplot(env_fish$pho)$out
x_4 <- x_3[-which(env_fish$pho %in% e ),]
f <- boxplot(env_fish$nit)$out
x_5 <- x_4[-which(env_fish$nit %in% f ),] 
g <- boxplot(env_fish$amm)$out
x_6 <- x_5[-which(env_fish$amm %in% g ),] 
h <- boxplot(env_fish$bdo)$out
env_fish <- x_6[-which(env_fish$bdo %in% h ),]           

#4
# exclude near zero-variance for analysis
nzv<- nearZeroVar(env_fish, saveMetrics=TRUE)

#detecte the collinearity among env variables or removing highly correlated features 
#with an absolute correlation of 0.75 or higher
#don't know how to do 

#ection II: Building a regression model.  
#5
# split data into training and test sets
#visualize the features and targets of the training set
library("caret")
set.seed(721)
index <- sample(1:nrow(env_fish),size=0.8*nrow(env_fish))
train <- env_fish[index,] 
test <- env_fish[-index,]                               

x = as.matrix(env_fish[,1:12])
y = as.factor(env_fish$total_fish)
featurePlot(x, y, plot = "density",
            strip = strip.custom(par.strip.text=list(cex=.7),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))   
            
#6
#Createand evaluate a baseline model between the environmental variables and the total fish abundance 
library("randomForest")
RF_model <- randomForest(total_fish ~., data = train, ntree = 500, mtry = 5, importance = TRUE)
print(RF_model)
pred_train <- predict(RF_model, train)
pred_test <- predict(RF_model, test)

RMSE(pred_train, train$total_fish)  #7.894573
RMSE(pred_test, test$total_fish)    #19.80662

library("rpart")
DT_model <- rpart(total_fish~., data = train, method = "anova" ) 
print(DT_model)
pred_train <- predict(DT_model, train)
pred_test <- predict(DT_model, test)

RMSE(pred_train, train$total_fish)  #21.99072
RMSE(pred_test, test$total_fish)    #19.80662

#data set is too small to construct BRT_model
#library("gbm")
#BRT_model <- gbm(total_fish~., data = train, shrinkage = 0.01, distribution = "bernoulli", n.tree = 3000)
#print(BRT_model)