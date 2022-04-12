#trees are useful way of handling data visually and allow first look
#building the classification tree
#install if necessary
#example with famous iris dataset (built-in)
#good for species classification!
library(rpart)
iris_tree_initial <- rpart(Species ~ ., data = iris, method = "class", 
                           minsplit = 2, minbucket = 1)
plot(iris_tree_initial)
text(iris_tree_initial)
#or for a prettier graph
require(rattle)
fancyRpartPlot(iris_tree_initial, main="Iris")

#what if you want fewer splits (less complex model)
#can use defaults for buckets 
iris_tree_initial_auto <- rpart(Species ~ ., data = iris)
fancyRpartPlot(iris_tree_initial_auto, main="Iris")

#or minimize complexity parameter (good for larger models)
iris_tree_model_2<- prune(iris_tree_initial, 
                          cp =   iris_tree_initial$cptable[which.min(iris_tree_initial$cptable[,"xerror"]),"CP"])
#is using this to make decisions
iris_tree_initial$cptable
fancyRpartPlot(iris_tree_model_2)

#validation techniques
#UNDER CONSTRUCTION
#need 0/1 column for for prediction
iris$virginica <- iris$Species
levels(iris$virginica)[levels(iris$virginica) == "virginica"]  <- "1"
levels(iris$virginica)[levels(iris$virginica) %in% c("setosa", "versicolor")] <- "0"
iris$virginica <- as.numeric(as.character(iris$virginica))

#compare glm and gam 
iris_glm <- glm(virginica ~ . - Species, iris, family = binomial)
summary(iris_glm)
iris_glm_final <- stepAIC(iris_glm)

iris_gam <- gam(virginica ~ s(Sepal.Length) + s(Sepal.Width) + 
                  s(Petal.Length) + s(Petal.Width), data = iris)
summary(iris_gam)
iris_gam_a <-update(iris_gam, . ~ . - s(Petal.Width))
summary(iris_gam_a)
iris_gam_b <-update(iris_gam_a, . ~ . - s(Sepal.Width))
summary(iris_gam_b)

AICc(iris_gam_b,  iris_glm_final)

#compare visually using AUC
#calculate AUROC (AUC)
require(ROCR)
iris_glm_final_predict<-prediction(fitted.values(iris_glm_final), iris$virginica)
iris_glm_final_performance<-performance(iris_glm_predict,"tpr","fpr")
#to see auc
plot(iris_glm_performance, main = "glm AUC")

#compare to gam
iris_gam_b_predict<-prediction(fitted.values(iris_gam_b), iris$virginica)
iris_gam_b_performance<-performance(iris_gam_b_predict,"tpr","fpr")
#to see auc
plot(iris_gam_b_performance, main = "gam AUC")

#cross validation
require(boot)
#K is the number of groups to put data into. default is "leave-one"out" design
iris_glm_final_cv<-cv.glm(iris,  iris_glm_final)
str(iris_glm_final_cv)
#delta is the prediction error and the adjusted rate - use adjusted to minimize
#impact of sampling or outliers

