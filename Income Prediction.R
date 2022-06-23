###########################
# data cleaning method ###
###########################
dat <-read.csv("US_Census.csv")
str(dat)
dat <- dat[-c(2)]
library(dplyr)
library(corrplot)
library(Metrics)
# removing any datapoints with people younger than 18
dat.no18 <- subset(dat, AGE>=18)

# since we're trying to predict income of those who do make money
# we'll be removing those who reported $0 income
dattemp<- subset(dat.no18, INCOME!=0)
plot(dattemp$INCOME)

# getting range of income
table(dattemp$INCOME)
max(dattemp$INCOME)
median(dattemp$INCOME)
mean(dat$INCOME)


# we will be using log since it makes the data normally distributed
hist(log(dattemp$INCOME,base=exp(10)))
hist(log(dattemp$INCOME,base=exp(25)))

# checking to see significant variables
allin <- lm(INCOME~.,dattemp )
summary(allin)
names(dattemp)

# removing because of linear dependencies
# since parent is based on 18 or below, we'll be removing this variable 
# given that we removed all people under 18
drop <- c("PARENT", "CLSWKR", "Retirement_Source", "Poverty_Level")
dat1 = dattemp[,!(names(dattemp) %in% drop)]
#dat1 <- dattemp[,-c(9,11,18,22:24)]
str(dat1)
names(dat1)

# income of those who didn't finish HS are still valuable, so we'll keep 
min(dat1$HGA)
hist(dat1$HGA)
below39 <- subset(dat1, HGA<39)
hist(below39$INCOME)
table(below39$INCOME)
summary(below39$INCOME)
summary(below39$AGE)

newallin <-lm(INCOME~., dat1)
summary(newallin)
names(dat1)
dat1$disability <- if_else(dat1$PEDISOUT == 1 | dat1$PEDISREM == 1, 1, 0)
# making indicator variables
disability <- as.factor(dat1$disability)
marital <- as.factor(dat1$MARITAL)
sex <- as.factor(dat1$SEX) #1 male, 2 female
#sex <- as.factor(dat1$SEX -1) #0 male, 1 female
edu_attmt<- as.factor(dat1$HGA -  30)
prim_rel <- as.factor(dat1$PF)
insurance <- as.factor(dat1$NOW_DIR) #0 not in universe,1 yes, 2 no
asian_type <- as.factor(dat1$ASIAN)
maj_occ <- as.factor(dat1$MJ_OCC_SEC)
prof_cert <- as.factor(dat1$Professional_Certification)
first_job <- as.factor(dat1$First_Job)
maj_ind <- as.factor(dat1$Major_Industry)
clswkr <- as.factor(dat1$CLSWKR)
otr_inc <- as.factor(dat1$Other_Income_Sources)
region <- as.factor(dat1$Region)
unmem <- as.factor(dat1$Union_Member)
own_land <-as.factor(dat1$RNT_YN)

#replacing not in universe values of -1 with 0
dat1$Professional_Certification[dat1$Professional_Certification == -1] <- 0   
dat1$ASIAN[dat1$ASIAN == -1] <- 0
dat1$Union_Member[dat1$Union_Member == -1] <- 0
colSums(dat1 < 1)

head(dat1)
names(dat1)
tmp_sex <-data.frame(model.matrix(~sex -1))
#tmp_pedisout <-data.frame(model.matrix(~pedisout -1))
#tmp_pedisrem <-data.frame(model.matrix(~pedisrem -1))
tmp_marital <-data.frame(model.matrix(~marital -1))
tmp_edu_attmt <-data.frame(model.matrix(~edu_attmt -1))
tmp_prim_rel <-data.frame(model.matrix(~prim_rel -1))
tmp_insurance <-data.frame(model.matrix(~insurance -1))
tmp_asian_type <-data.frame(model.matrix(~asian_type -1))
tmp_maj_occ <-data.frame(model.matrix(~maj_occ -1))
tmp_prof_cert <-data.frame(model.matrix(~prof_cert -1))
tmp_first_job <-data.frame(model.matrix(~first_job -1))
tmp_maj_ind <-data.frame(model.matrix(~maj_ind -1))
#tmp_clswkr <-data.frame(model.matrix(~clswkr -1))
tmp_otr_inc <-data.frame(model.matrix(~otr_inc -1))
tmp_region <-data.frame(model.matrix(~region -1))
tmp_unmem <- data.frame(model.matrix(~unmem-1))
tmp_own_land <- data.frame(model.matrix(~own_land-1))

names(dat1)
dat2 <- cbind(dat1[, c(1,5)], dat1$disability,
              tmp_marital[,2:7], tmp_sex[,2], tmp_edu_attmt[,2:15], 
              tmp_prim_rel[,2:6], tmp_insurance[,2], tmp_asian_type[,2:7], 
              tmp_maj_occ[,2:11],tmp_prof_cert[,2:3], tmp_first_job[,2:9], 
              tmp_maj_ind[,2:14],tmp_unmem[,2:3], tmp_otr_inc[,2:12], tmp_region[,2:10])
head(dat2)
newallin <- lm(INCOME~.,dat2)
summary(newallin)

#looking at correlation of indicator variables

corrdf <- as.data.frame(as.table(cor(dat2)))
corrdf
str(cor(dat2))
corrdf %>% arrange(desc(Freq)) %>% filter(Freq< -0.6)
corrdf %>% arrange(desc(Freq)) %>% filter(Freq> 0.6) %>% filter(Freq != 1)
# Not in Universe and No are telling the same information, so we can combine NIU and No
# Same with prof_cert
#dat2$disability <- paste(dat2$pedisout1, dat2$pedisout2, dat2$pedisrem1, dat2$pedisrem2)
#head(dat2$disability)
#Major occupation is highly correlated, so we will drop all of those dummies.
drop2 <- c("maj_occ1", "maj_occ2", "maj_occ3", "maj_occ4",
           "maj_occ5", "maj_occ6", "maj_occ7", "maj_occ8",
           "maj_occ9", "maj_occ10")
dat3 = dat2[,!(names(dat2) %in% drop2)]
newallin2 <- lm(log(INCOME)~.,dat3)
summary(newallin2)
cor <- cor(dat1)
corrplot(cor, method='color')

library(leaps)
set.seed(36963508)
train <- sample(nrow(dat3),(nrow(dat3)/2)-1)
dat3.train <- dat3[train,]
dat3.test <- dat3[-train,]

regfit.full <- regsubsets(INCOME~., data = dat3.train, nvmax = 81, really.big = T,
                          method = "seqrep")
summary(regfit.full)
summary(regfit.full)$rsq
plot(summary(regfit.full)$rsq)

dat.test.mat <- model.matrix(INCOME~.,data = dat3.test)
coef35 <- coef(regfit.full,35)

coef20 <- coef(regfit.full,20)
yhat20 <- dat.test.mat[,names(coef20)] %*% coef20
MSE.bs20 <- mean((dat3.test$INCOME - yhat20)^2)
RMSE.bs20 <- MSE.bs20^0.5
RMSE.bs20

reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
plot(reg.summary$rsq)

which.max(regfit.full$adjr2)
maxar2 <- which.max(reg.summary$adjr2)
points(maxar2,reg.summary$adjr2[maxar2], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(reg.summary$cp)
mincp <- which.min(reg.summary$cp)
points(mincp,reg.summary$cp[mincp], col = "blue", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab ="Bayesian Info Crit")
which.min(reg.summary$bic)
minbic <- which.min(reg.summary$bic)
points(minbic,reg.summary$bic[minbic], col = "green", cex = 2, pch = 20)
par(mfrow=c(1,1))

reg.summary$bic
str(reg.summary)

## Ridge/Lasso Model
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(glmnet)


#setwd()
dat <- read.csv('Final_Cleaned_Census_Data_MSBA.csv')
str(dat)

set.seed(6942069) 

index = sample(1:nrow(dat), 0.7*nrow(dat)) 

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

dim(train)
dim(test)



#####################
##      RIDGE     ###
#####################
names(dat)

#define response variable
y.train <- train$INCOME
y.test <- test$INCOME

#define matrix of predictor variables
x.train <- data.matrix(train[,c(2:81)])
x.test <- data.matrix(test[,c(2:81)])

#fit ridge regression model
ridge_reg <- glmnet(x.train, y.train, alpha = 0)

#view summary of model
summary(ridge_reg)


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x.train, y.train, alpha = 0)

#find optimal lambda value that minimizes test MSE
cv_model$lambda.min
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model)

best_ridge <- glmnet(x.train, y.train, alpha = 0, lambda = best_lambda)
coef(best_ridge)

plot(ridge_reg, xvar = "lambda")

#use fitted best model to make predictions
y_predicted_tr <- predict(ridge_reg, s = best_lambda, newx = x.train)
y_predicted_tst <- predict(ridge_reg, s = best_lambda, newx = x.test)


#find SST and SSE
sst_tr <- sum((y.train - mean(y.train))^2)
sse_tr <- sum((y_predicted_tr - y.train)^2)

sst_tst <- sum((y.test - mean(y.test))^2)
sse_tst <- sum((y_predicted_tst - y.test)^2)

#find R-Squared
rsq_tr <- 1 - sse_tr/sst_tr
rsq_tr
rsq_tst <- 1 - sse_tst/sst_tst
rsq_tst

#find RMSE
RMSE_train <- sqrt(sst_tr/nrow(dat))
RMSE_train

RMSE_test <- sqrt(sst_tst/nrow(dat))
RMSE_test

#####################
##      LASSO     ###
#####################

#define response variable
y.train <- train$INCOME
y.test <- test$INCOME

#define matrix of predictor variables
x.train <- data.matrix(train[,c(2:81)])
x.test <- data.matrix(test[,c(2:81)])

#perform k-fold cross-validation to find optimal lambda value
cv_lasso <- cv.glmnet(x.train, y.train, alpha = 1)

#find optimal lambda value that minimizes test MSE
optimal_lambda <- cv_lasso$lambda.min
optimal_lambda


#produce plot of test MSE by lambda value
plot(cv_lasso) 

#find coefficients of best model
best_lasso <- glmnet(x.train, y.train, alpha = 1, lambda = optimal_lambda)
coef(best_lasso)


#use fitted best model to make predictions
lasso_predicted_tr <- predict(best_lasso, s = best_lambda, newx = x.train)
lasso_predicted_tst <- predict(best_lasso, s = best_lambda, newx = x.test)

#find SST and SSE
sst_lassotr <- sum((y.train - mean(y.train))^2)
sse_lassotr <- sum((lasso_predicted_tr - y.train)^2)

sst_lassotst <- sum((y.test - mean(y.test))^2)
sse_lassotst <- sum((lasso_predicted_tst - y.test)^2)

#find R-Squared
rsq_lassotr <- 1 - sse_lassotr/sst_lassotr
rsq_lassotr
rsq_lassotst <- 1 - sse_lassotst/sst_lassotst
rsq_lassotst

#find RMSE
RMSE_lassotrain <- sqrt(sst_lassotr/nrow(dat))
RMSE_lassotrain

RMSE_lassotest <- sqrt(sst_lassotst/nrow(dat))
RMSE_lassotest
########################
####XGboost model#######
########################
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
install.packages("xgboost")
install.packages("DiagrammeR")

library("xgboost")

cor(dat)[1,]
#setwd("~/Documents/predictive analytics/Group Project")
dat1 <- read.csv("Final_Cleaned_Census_Data_MSBA.csv")
##dat1 <- subset(dat,INCOME<=500000 & INCOME >=24000)
set.seed(6942069)
index <- sample(1:nrow(dat1), 0.7*nrow(dat1)) 

train <- dat1[index,] # Create the training data 
test <- dat1[-index,] # Create the test data

label <- as.numeric(train$INCOME)
trainmatrix <- as.matrix(train[,2:72])
bstSparse <- xgboost(data = trainmatrix, label = label, max.depth = 2, booster = "gblinear", nthread = 2,nrounds = 500, objective = "reg:squarederror")
testmatrix <- as.matrix(test[,2:72])
pred <-predict(bstSparse,testmatrix)
RSS.test <- sum((test$INCOME-pred)^2)
MSE.test <- RSS.test/nrow(test)
MSE.test
RMSE.test <- MSE.test^0.5
RMSE.test
residuals <- sum((test$INCOME-pred)^2)
test_mean<-mean(pred)
tss <- sum((pred-test_mean)^2)
rss<-sum((test$INCOME-pred)^2)
rsq <- 1- (tss/rss)
rsq 
names(bstSparse)
diffe <- test$INCOME-pred


compdata <- data.frame(test$INCOME,pred,diffe)


x = 1:length(test$INCOME)
plot(x, test$INCOME, col = "red", type = "l")
lines(x, pred, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

#higher income people are harder to predict. 
plot(compdata,col = "Purple")

plot(bstSparse$evaluation_log$iter,bstSparse$evaluation_log$train_rmse)

##############
#####SVM######
##############
install.packages("Metrics")
library(Metrics)
library(e1071)
library(lattice)
library(ggplot2)
library(caret)
library(Metrics)
library('dplyr')


dat <-read.csv("Final_Cleaned_Census_Data_MSBA.csv")
str(dat)

# Select 10,000 rows for train data

set.seed(13579)
train.attr <- sample(41999,10000)
train <- dat[train.attr,]
summary(train)
sum(is.na(train))

# Select 5,000 rows for train data

dat.nota.notsel <- dat[-train.attr,]
dat.nota.notsel
test.nota <- sample(31999,5000)
test <- dat.nota.notsel[test.nota,]
summary(test)
sum(is.na(test))

# SVM linear

svmfit <- svm(INCOME~., data = train, kernel = "linear", scale = FALSE)
svmfit$nSV
svmfit$tot.nSV
svmfit$index
summary(svmfit)
plot(svmfit, train)

# Predicting testing with SVM linear

predYsvm = predict(svmfit, test)
RMSEsvm=rmse(predYsvm,test$INCOME)
mse = mse(test$INCOME, predYsvm)
mae = MAE(test$INCOME, predYsvm)
rmse = RMSE(test$INCOME, predYsvm)

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n")


# SVM radial

svmfit1 <- svm(INCOME~., data = train, kernel = "radial", scale = TRUE)
print(svmfit1)

# RMSE for train model

yhat1.1 <- svmfit1$fitted
table(truth = train$INCOME, predict = yhat1.1)
MSE1 <- mean((train$INCOME - yhat1.1)^2)
RMSE1 <- MSE1^0.5
RMSE1

#Predicting test with SVM radial

predYsvm1 = predict(svmfit1, test)
RMSEsvm1=rmse(predYsvm1,test$INCOME)
mse1 = mse(test$INCOME, predYsvm1)
mae1 = MAE(test$INCOME, predYsvm1)
rmse1 = RMSE(test$INCOME, predYsvm1)

cat(" MAE:", mae1, "\n", "MSE:", mse1, "\n", 
    "RMSE:", rmse1, "\n")

# Calculating R-squared

rss <- sum((predYsvm1 - test$INCOME) ^ 2)
tss <- sum((test$INCOME - mean(test$INCOME)) ^ 2)
rsq <- 1 - rss/tss
rsq

###########
### GBM ###
###########
library(gbm)
library(h2o)
dat <- read.csv('Final_Cleaned_Census_Data_MSBA.csv')
head(dat)
set.seed(6942069)
index = sample(1:nrow(dat), 0.7*nrow(dat)) 

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

dim(train)
dim(test)
X_train = as.matrix(train)
y_train = train$INCOME

X_test = as.matrix(test)
y_test = test$INCOME

set.seed(6942069)
gbm.fit <- gbm(
  formula = INCOME ~ .,
  distribution = "gaussian",
  data = test,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# print results
print(gbm.fit)
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")

CV_RSq <- (cor(gbm.fit$cv.fitted, test$INCOME))^2
Train_RSq <- (cor(gbm.fit$fit, test$INCOME))^2


par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

############################
##### Linear Regresssion####
############################

#libraries
setwd("C:/Users/chaff/Desktop/UC Irvine/Winter Quarter/Predictive Analytics/Project")
library(plyr)
library(dplyr)
library(Metrics)
library(ggplot2)
library(NbClust)

#-------------------------------------------------------------------------------------
set.seed(6942069) 

dat <- read.csv("Final_Cleaned_Census_Data_MSBA.csv")
head(dat)
ncol(dat)
colnames(dat)
index = sample(1:nrow(dat), 0.7*nrow(dat)) 

nrow(dat)
dat[57920,]

our_predictions <- subset(dat, dat$INCOME  == "brian"|"garrett"|"han"|"andrew"|"sally")
our_predictions

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

dim(train)
dim(test)


train_mod1 <- lm(log(INCOME) ~                
                   AGE + marital7 + sex + edu_attmt2 + edu_attmt3 + edu_attmt4 + edu_attmt5 + 
                   edu_attmt6  + edu_attmt7 +  edu_attmt8 +  edu_attmt9 + edu_attmt10 +  edu_attmt11 
                 + edu_attmt12 + edu_attmt13 + edu_attmt14 + edu_attmt15 + prim_rel1 +  prim_rel2 + 
                   insurance + asian_type1 + prof_cert2  + first_job1 + first_job4 + 
                   first_job5  +  first_job6 + maj_ind3 + maj_ind4 + maj_ind7 + maj_ind8 + maj_ind9 
                 + maj_ind13 + region3 + region4 + region10, data = train)


test_prediction <- predict(train_mod1, data = test)
test_prediction

head(test_prediction)

test_rmse <- rmse(test$INCOME,test_prediction)
test_rmse

plot1 <- plot(train_mod1)
plot1


#-----------------------------------------------------------------------
#checking for number of lines which meet certain income requirements ie how many people
#make more than x, what is the RMSE/model stats of people within the group defined

count <- 0
for (i in dat$INCOME) {
  
  if(i > 12000 & i < 500000)  {count = count + 1
  }}
print(count)



dat$INCOME %>% unique() %>% length() #checking for the number of unique values

dat1 <- subset(dat, dat$INCOME  < 500000 & dat$INCOME > 12000)


index1 = sample(1:nrow(dat1), 0.7*nrow(dat1)) 

train1 = dat1[index1,] # Create the training data 
test1 = dat1[-index1,] # Create the test data

#taking the log of income and squaring Age
train_mod2 <- lm(log(INCOME) ~                
                   AGE^2 + marital7 + sex + edu_attmt2 + edu_attmt3 + edu_attmt4 + edu_attmt5 + 
                   edu_attmt6  + edu_attmt7 +  edu_attmt8 +  edu_attmt9 + edu_attmt10 +  edu_attmt11 
                 + edu_attmt12 + edu_attmt13 + edu_attmt14 + edu_attmt15 + prim_rel1 +  prim_rel2 + 
                   insurance + asian_type1 + prof_cert2  + first_job1 + first_job4 + 
                   first_job5  +  first_job6 + maj_ind3 + maj_ind4 + maj_ind7 + maj_ind8 + maj_ind9 
                 + maj_ind13 + region3 + region4 + region10, data = train1)

summary(train_mod2)
test_prediction2 <- predict(train_mod2, data = test1)

head(test_prediction2)


test_rmse2 <- rmse(test1$INCOME,test_prediction2)
test_rmse2

#dat$Income dat$INCOME  < 500000 & dat$INCOME > 12000: RMSE = 107246.2
#dat$INCOME > 1000000) : rmse = 1264303
#dat$INCOME < 1000000) : rmse = 109582.9
#dat$INCOME < 100000) rmse = 45905.07
#dat$INCOME > 100000) rmase = 287702.8
#dat$INCOME < 80000) rmse = 38786.03


############################################################
# Predictions #
############################################################
jc.dat <-read.csv("US_Census_with_us.csv") #with us
nrow(jc.dat)
dim(jc.dat)
set.seed(420)
############################################################

############################################################
str(jc.dat)


jc.dat.no18 <- subset(jc.dat, AGE >= 18)
jc.dat.pos.income <- subset(jc.dat.no18, INCOME != 0)
jc.dat_postHS <- subset(jc.dat.pos.income, jc.dat.pos.income$HGA >= 39)
jc.new_dat <- jc.dat_postHS
jc.new_dat$disability <- if_else(jc.new_dat$PEDISOUT == 1 | jc.new_dat$PEDISREM == 1, 1, 0)
jc.new_dat$ASIAN[jc.new_dat$ASIAN == -1] <- 0
jc.new_dat$Union_Member[jc.new_dat$Union_Member == -1] <- 0

jc.drop <- c("PARENT", "CLSWKR", "Retirement_Source", "Poverty_Level","MJ_OCC_SEC", 
             "PEDISOUT", "PEDISREM", "PEDISPHY", "PHF_SEQ","Income_Bracket")

jc.dat_temp <- jc.new_dat[,!(names(jc.new_dat) %in% jc.drop)]

head(jc.dat_temp)
ncol(jc.dat_temp)
colnames(jc.dat_temp)

jc.disability <- as.factor(jc.dat_temp$disability)
jc.marital <- as.factor(jc.dat_temp$MARITAL)
jc.sex <- as.factor(jc.dat_temp$SEX) #1 male, 2 female
jc.edu_attmt<- as.factor(jc.dat_temp$HGA - 30)
jc.prim_rel <- as.factor(jc.dat_temp$PFREL)
jc.insurance <- as.factor(jc.dat_temp$NOW_DIR) #0 not in universe,1 yes, 2 no
jc.asian_type <- as.factor(jc.dat_temp$ASIAN)
jc.prof_cert <- as.factor(jc.dat_temp$Professional_Certification)
jc.first_job <- as.factor(jc.dat_temp$First_Job)
jc.maj_ind <- as.factor(jc.dat_temp$Major_Industry)
jc.clswkr <- as.factor(jc.dat_temp$CLSWKR)
jc.otr_inc <- as.factor(jc.dat_temp$Other_Income_Sources)
jc.region <- as.factor(jc.dat_temp$Region)
jc.unmem <- as.factor(jc.dat_temp$Union_Member)
jc.own_land <-as.factor(jc.dat_temp$RNT_YN)


jc.tmp_sex <- data.frame(model.matrix(~jc.sex -1))
jc.tmp_marital <-data.frame(model.matrix(~jc.marital -1))
jc.tmp_edu_attmt <-data.frame(model.matrix(~jc.edu_attmt -1))
jc.tmp_prim_rel <-data.frame(model.matrix(~jc.prim_rel -1))
jc.tmp_insurance <-data.frame(model.matrix(~jc.insurance -1))
jc.tmp_asian_type <-data.frame(model.matrix(~jc.asian_type -1))
jc.tmp_prof_cert <-data.frame(model.matrix(~jc.prof_cert -1))
jc.tmp_first_job <-data.frame(model.matrix(~jc.first_job -1))
jc.tmp_maj_ind <-data.frame(model.matrix(~jc.maj_ind -1))
jc.tmp_otr_inc <-data.frame(model.matrix(~jc.otr_inc -1))
jc.tmp_region <-data.frame(model.matrix(~jc.region -1))
jc.tmp_unmem <- data.frame(model.matrix(~jc.unmem-1))
jc.tmp_own_land <- data.frame(model.matrix(~jc.own_land-1))


jc.dat_temp2 <- cbind(jc.dat_temp[, c(1,3,17)],
                      jc.tmp_marital[,2:7], jc.tmp_sex[,2], jc.tmp_edu_attmt[,2:ncol(jc.tmp_edu_attmt)], # look at temp edu attmt 
                      jc.tmp_prim_rel[,2:6], jc.tmp_asian_type[,2:7],  #
                      jc.tmp_prof_cert[,2:3], jc.tmp_first_job[,2:9], 
                      jc.tmp_maj_ind[,2:14],jc.tmp_unmem[,2:3], jc.tmp_otr_inc[,2:12], jc.tmp_region[,2:10], jc.tmp_insurance[,2], jc.tmp_own_land[,1]) #tmp_own_land[,1] removed that as landown yes already has

ncol(jc.dat_temp)
colnames(jc.dat_temp)

ncol(jc.dat_temp2)
colnames(jc.dat_temp2)
#ncol(tmp_region)
names(jc.dat_temp2)[3] <- "disability"
names(jc.dat_temp2)[10] <- "FemaleYes"
names(jc.dat_temp2)[74] <- "InsuranceCovered"
names(jc.dat_temp2)[75] <- "LandOwnerYes"

colnames(jc.dat_temp2)
head(jc.dat_temp2)
ncol(jc.dat_temp2)

jc.our_info <- jc.dat_temp2[54593:nrow(jc.dat_temp2),]
jc.our_info

jc.dat2 <- jc.dat_temp2[1:54592,]


jc.count_test <- 0

for (i in jc.dat2$INCOME) {
  
  if(i < 100000)  {jc.count_test = jc.count_test + 1
  }}

print(jc.count_test)


jc.model_dat <- subset(jc.dat2, jc.dat2$INCOME > 5000)

jc.index1 = sample(1:nrow(jc.model_dat), 0.7*nrow(jc.model_dat)) 

jc.train1 = jc.model_dat[jc.index1,] # Create the training data 
jc.test1 = jc.model_dat[-jc.index1,] # Create the test data

#taking the log of income and squaring Age

jc.train_mod2 <- lm(log(INCOME) ~ AGE^2 + disability + jc.marital2 + jc.marital3 + jc.marital4 + jc.marital5 + 
                      jc.marital6 + jc.marital7 + FemaleYes  + jc.edu_attmt10 + jc.edu_attmt11 + jc.edu_attmt12 + 
                      jc.edu_attmt13 + jc.edu_attmt14 + jc.edu_attmt15 + jc.edu_attmt16 + jc.prim_rel1 + jc.prim_rel2 + jc.prim_rel3 + jc.prim_rel4 + jc.prim_rel5 + 
                      InsuranceCovered + jc.asian_type1 + jc.asian_type2 + jc.asian_type3 + jc.asian_type4 
                    + jc.asian_type5 + jc.asian_type6 + jc.prof_cert1 + jc.prof_cert2 + jc.first_job1 + 
                      jc.first_job2 + jc.first_job3 + jc.first_job4 + jc.first_job5 + jc.first_job6 + 
                      jc.first_job7 + jc.first_job8 + jc.maj_ind1 + jc.maj_ind2 + jc.maj_ind3 + jc.maj_ind4 + 
                      jc.maj_ind5 + jc.maj_ind6 + jc.maj_ind7 + jc.maj_ind8 + jc.maj_ind9 + jc.maj_ind10 + 
                      jc.maj_ind11 + jc.maj_ind12 + jc.maj_ind13 + jc.unmem1 + jc.unmem2 + jc.otr_inc1 + 
                      jc.otr_inc2 + jc.otr_inc3 + jc.otr_inc4 + jc.otr_inc7  + jc.otr_inc9  + jc.otr_inc13 + jc.region2 + jc.region3 + 
                      jc.region4 + jc.region5 + jc.region6 + jc.region7 + jc.region8 + jc.region9 + jc.region10 + 
                      LandOwnerYes, data = jc.train1 )
summary(jc.train_mod2)


jc.test_prediction2 <- predict(jc.train_mod2, data = jc.test1)
jc.test_rmse2 <- rmse(jc.test1$INCOME,jc.test_prediction2)
jc.test_rmse2

jc.our_prediction <- predict(jc.train_mod2, jc.our_info) #gets logged values of predicted incomes
jc.actual_pred <- exp(jc.our_prediction)
jc.actual_pred
summary(jc.actual_pred)


#predictions @ income < 70000
# 163544   163545   163546   163547   163548   163549   163550   163551   163552   163553 
#30466.67 23486.45 16697.70 23486.45 16697.70 24155.63 19947.78 23675.83 18124.06 15352.51 
#163554 
#43867.97 

#predictions @ income > 5000
#163544    163545    163546    163547    163548    163549    163550    163551    163552 
#70027.04  50128.59  39330.79  50128.59  39330.79  47235.57  47723.20  35546.07  33399.75 
#163553    163554 
#37084.94 166511.14 
