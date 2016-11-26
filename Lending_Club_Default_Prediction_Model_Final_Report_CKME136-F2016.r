
#install.packages("caret", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("ggplot2", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("psych", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("gmodels", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("party", repos='http://cran.us.r-project.org',dependencies = TRUE)
#install.packages("e1071", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("unbalanced", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("hmeasure", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("pROC", repos='http://cran.us.r-project.org',dependencies = TRUE)
install.packages("adabag", repos='http://cran.us.r-project.org',dependencies = TRUE)
library(caret)
library(psych)
library(lattice)
library(party)
library(unbalanced)
#library(hmeasure)
library(pROC)

lending_club_2012_2013 <- read.csv("lending_club_rfe/LoanStats3b_securev1.csv", header=TRUE, skip=1)
lending_club_2007_2011 <- read.csv("lending_club_rfe/LoanStats3a_securev1.csv", header=TRUE, skip=1)

lending_club_jan_2013 <- subset(lending_club_2012_2013,issue_d == "Jan-2013")
lending_club_feb_2013 <- subset(lending_club_2012_2013,issue_d == "Feb-2013")
lending_club_2012 <- subset(lending_club_2012_2013,grepl("2012",issue_d))
lending_club_2012_2013_36 <- subset(rbind(lending_club_2012,lending_club_feb_2013,lending_club_jan_2013),grepl("36",term))
lending_club_2011 <- subset(lending_club_2007_2011, grepl("2011",issue_d))
lending_club_2007_2010 <- lending_club_2007_2011[-NROW(lending_club_2011),]
lending_club_final <- rbind(lending_club_2007_2010,lending_club_2011,lending_club_2012_2013_36)

nrow(lending_club_final)

ncol(lending_club_final)

# add label (default_status) to dataset
status <- c("Current","Fully Paid","Late (16-30 days)","Does not meet the credit policy. Status:Charged Off","Charged Off","Default","In Grace Period","Late (31-120 days)","Does not meet the credit policy. Status:Fully Paid")
default_status <- c(0,0,1,1,1,1,1,1,0)
lending_club_default <- data.frame(default = default_status[match(lending_club_final$loan_status, status)])
lending_club_final <- cbind(lending_club_final,lending_club_default)

# remove all records which have default as NA
lending_club_final <- lending_club_final[-which(is.na(lending_club_final$default)==TRUE),]

lending_club_final <- lending_club_final[, !(colnames(lending_club_final) %in% c("total_rec_int","total_pymnt_inv","total_pymnt"
                                                                                 ,"total_rec_prncp","collection_recovery_fee",
                                                                                 "recoveries","last_pymnt_amnt",
                                                                                 "total_rec_late_fee","last_pymnt_d",
                                                                                 "last_pymnt_amnt","next_pymnt_d","out_prncp",
                                                                                 "out_prncp_inv","issue_d", 
                                                                                 "initial_list_status","funded_amnt",
                                                                                 "funded_amnt_inv","id","pymnt_plan"))]

length(colnames(lending_club_final))

# find which varaibles/features are all NA
na_pct <- sapply(lending_club_final, function(y) sum(is.na(y))/length(y))
na_pct <- data.frame(na_pct)
all_na <- na_pct == 1

# remove variables/features which are all NA
lending_club_final <- lending_club_final[,-which(all_na==TRUE)]

# Find which variables/features have some NA values
na_pct <- sapply(lending_club_final, function(y) sum(is.na(y))/length(y))
na_pct <- data.frame(na_pct)
sig_na <- na_pct > 0
sig_na.df <- as.data.frame(sig_na)
sig_na.df <- subset(sig_na.df, sig_na.df$na_pct==TRUE)
sig_na_col <- row.names(sig_na.df)

# create a data frame of variables/features with some NA values
lending_club_final_na_sig <- lending_club_final[,which(names(lending_club_final) %in% sig_na_col)]

# remove variables/features which have some NA values
lending_club_final <- lending_club_final[,-which(sig_na==TRUE)]

# change the datatypes of the dataframe above to integer
lending_club_final_na_sig_int <- as.data.frame(lapply(lending_club_final_na_sig,as.integer))

# apply column averages to NA values
for(i in 1:ncol(lending_club_final_na_sig_int)){
  lending_club_final_na_sig_int[is.na(lending_club_final_na_sig_int[,i]), i] <- mean(lending_club_final_na_sig_int[,i], 
                                                                                        na.rm = TRUE)
}

# bring it all together
lending_club_final <- cbind(lending_club_final_na_sig_int,lending_club_final)

length(colnames(lending_club_final))

# columns/features in dataset
colnames(lending_club_final)

library(ggplot2)
ggplot(lending_club_final, aes(as.factor(default), as.double(int_rate))) + geom_boxplot() +
labs(title="Loan Amount vs. Interest Rate", x="default status", y="interest rate")

# find correlation coefficient
cor(as.numeric(lending_club_final$int_rate), lending_club_final$default, method="pearson")

#install.packages("gmodels", repos='http://cran.us.r-project.org',dependencies = TRUE)
library(gmodels)

emp_length.factor <- lending_club_final$emp_length
default.factor <- as.factor(lending_club_final$default)

emp_length.levels <- levels(emp_length.factor)
emp_length.levels <- emp_length.levels[2:nlevels(lending_club_final$emp_length)]
emp_length.len <- length(emp_length.levels)

joint <- CrossTable(emp_length.factor, default.factor, prop.chisq=FALSE)
joint_counts <- joint$prop.col
barplot(joint_counts, beside=TRUE, ylab="proportion", xlab="default status", col=rainbow(emp_length.len))
legend("topright", emp_length.levels, pch=15, col=rainbow(emp_length.len))

# find correlation coefficient
cor(as.numeric(lending_club_final$emp_length), lending_club_final$default, method="pearson")

ggplot(lending_club_final, aes(as.factor(default), loan_amnt)) + geom_boxplot() +
labs(title="Loan Amount vs. Default Status", x="default status", y="loan amount")

# find correlation coefficient
cor(as.numeric(lending_club_final$loan_amnt), lending_club_final$default, method="pearson")

ggplot(lending_club_final, aes(as.factor(default), annual_inc)) + geom_boxplot()

summary(lending_club_final$annual_inc)

boxplot(annual_inc~as.factor(default),outline=FALSE, data=lending_club_final, ylab="annual income", xlab="default status")
title("Annual Income vs. Default Status")

# find correlation coefficient
cor(as.numeric(lending_club_final$annual_inc), lending_club_final$default, method="pearson")

summary(lending_club_final$delinq_2yrs)
delinq_2.table <- table(delinq_2yrs=lending_club_final$delinq_2yrs,default=as.factor(lending_club_final$default))
prop.table(delinq_2.table,2)

#install.packages("psych", repos='http://cran.us.r-project.org',dependencies = TRUE)
#library(psych)
describeBy(lending_club_final$delinq_2yrs, as.factor(lending_club_final$default))

#library(lattice)
stripplot(lending_club_final$delinq_2yrs~as.factor(lending_club_final$default), jitter=TRUE, 
          main="Delinq. 2 Years vs. Default Status", xlab="default status", ylab="delinq_2yrs")

# find correlation coefficient
cor(lending_club_final$delinq_2yrs, lending_club_final$default, method="pearson")

describeBy(lending_club_final$fico_range_low, as.factor(lending_club_final$default))
ggplot(lending_club_final, aes(as.factor(default), fico_range_low)) + geom_boxplot() +
    labs(title="FICO Range Low vs. Default Status", x="default status", y="FICO range low")

# find correlation coefficient
cor(lending_club_final$fico_range_low, lending_club_final$default, method="pearson")

describeBy(lending_club_final$fico_range_high, as.factor(lending_club_final$default))
ggplot(lending_club_final, aes(as.factor(default), fico_range_high)) + geom_boxplot() +
    labs(title="FICO Range High vs. Default Status", x="default status", y="FICO range high")

# find correlation coefficient
cor(lending_club_final$fico_range_high, lending_club_final$default, method="pearson")

describeBy(lending_club_final$dti, as.factor(lending_club_final$default))

ggplot(lending_club_final, aes(as.factor(default), dti)) + geom_boxplot() +
    labs(title="DTI Ratio vs. Default Status", x="default status", y="dti")

# find correlation coefficient
cor(lending_club_final$dti, lending_club_final$default, method="pearson")

describeBy(lending_club_final$last_fico_range_high, as.factor(lending_club_final$default))

ggplot(lending_club_final, aes(as.factor(default), last_fico_range_high)) + geom_boxplot() +
    labs(title="Last FICO Range High vs. Default Status", x="default status", y="last_fico_range_high")

# find correlation coefficient
cor(lending_club_final$last_fico_range_high, lending_club_final$default, method="pearson")

describeBy(lending_club_final$last_fico_range_low, as.factor(lending_club_final$default))

ggplot(lending_club_final, aes(as.factor(default), last_fico_range_low)) + geom_boxplot() +
    labs(title="Last FICO Range Low vs. Default Status", x="default status", y="last_fico_range_low")

# find correlation coefficient
cor(lending_club_final$last_fico_range_low, lending_club_final$default, method="pearson")

# convert int_rate to numeric
lending_club_final$int_rate <- as.character(lending_club_final$int_rate)
lending_club_final$int_rate <- as.numeric(substr(lending_club_final$int_rate,1,nchar(lending_club_final$int_rate)-1))
 
# convert earliest_cr_line to POSIX
lending_club_final$earliest_cr_line <- as.vector(sapply(lending_club_final$earliest_cr_line, function(x) paste0(x,"-01")))
lending_club_final$earliest_cr_line <- as.Date(lending_club_final$earliest_cr_line,"%b-%Y-%d")
lending_club_final$earliest_cr_line <- as.numeric(as.POSIXct(lending_club_final$earliest_cr_line, format="%Y-b%-%d"))    

# convert revol_util to numeric
lending_club_final$revol_util <- as.character(lending_club_final$revol_util)
lending_club_final$revol_util <- as.numeric(substr(lending_club_final$revol_util,1,nchar(lending_club_final$revol_util)-1))

# convert last_credit_pull_d to POSIX because as factors they have too many levels!
lending_club_final$last_credit_pull_d <- as.vector(sapply(lending_club_final$last_credit_pull_d, function(x) paste0(x,"-01")))
lending_club_final$last_credit_pull_d <- as.Date(lending_club_final$last_credit_pull_d,"%b-%Y-%d")
lending_club_final$last_credit_pull_d <- as.numeric(as.POSIXct(lending_club_final$last_credit_pull_d, format="%Y-b%-%d"))
        
# remove useless variables
to_remove <- c("url","desc","title","emp_title","id","loan_status","zip_code")
lending_club_final <- lending_club_final[ , !(names(lending_club_final) %in% to_remove)]

nrows <- nrow(lending_club_final)
ncomplete <- sum(complete.cases(lending_club_final))
print(1-(ncomplete/nrows))

lending_club_final <- lending_club_final[complete.cases(lending_club_final),]

# how many records in data set so far
lcf_before_na_rm <- nrow(lending_club_final)
lcf_before_na_rm

lending_club_final_fact_as_num <- lending_club_final

# change factor columns/ features to numeric
indx <- sapply(lending_club_final_fact_as_num, is.factor)
lending_club_final_fact_as_num[indx] <- lapply(lending_club_final_fact_as_num[indx], function(x) seq_along(levels(x))[x])

# create subset of dataset with only numeric features    
num_feat <- sapply(lending_club_final_fact_as_num, is.numeric)
lending_club_final_num_only <- lending_club_final_fact_as_num[,num_feat] 

# eliminate columns/features which are all the same value
unilength <- sapply(lending_club_final_num_only,function(x) length(unique(x)))
lending_club_final_num_only <- subset(lending_club_final_num_only, select=unilength>1)
    
# calculate correlation matrix
correlationMatrix <- cor(lending_club_final_num_only)

# summarize the correlation matrix
#print(correlationMatrix)

# find features that are highly correlated
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.6)
highCorrelationMatrix <- correlationMatrix[highlyCorrelated,highlyCorrelated]

# plot correlation of highly correlated features
library(corrplot)
corrplot(highCorrelationMatrix, method="circle")

table(lending_club_final$default)

prop.table(table(lending_club_final$default))

train_rows <- sample(nrow(lending_club_final),(nrow(lending_club_final)*0.6))
lending_club.train <- lending_club_final[train_rows,]
lending_club.test <- lending_club_final[-train_rows,]

# How many records per class?
table(lending_club.train$default)

# Plot histogram of class distribution
hist(lending_club.train$default)

# Percentage of total dataset per class (0= non-default, 1= default)
prop.table(table(lending_club.train$default))

hist(lending_club.train$default)

#install.packages("unbalanced", repos='http://cran.us.r-project.org',dependencies = TRUE)
#library(unbalanced)

n <- ncol(lending_club.train)
y <- as.factor(lending_club.train$default)
x <- lending_club.train[ ,-n]
lending_club.train.smote <- ubSMOTE(X=x, Y=y)
lending_club.train.smote$default <- as.numeric(as.character(lending_club.train.smote$Y))

lending_club.train <- cbind(lending_club.train.smote$X, default=lending_club.train.smote$default)

# How many records per class?
table(lending_club.train$default)

# Plot histogram of class distribution
hist(lending_club.train$default)

# Percentage of total dataset per class (0= non-default, 1= default)
prop.table(table(lending_club.train$default))

hist(lending_club.train$default)

library(caret)
#install.packages("e1071", repos='http://cran.us.r-project.org',dependencies = TRUE)
library(e1071)
library(randomForest)
set.seed(9)

ptm <- proc.time()
#########################
rf.fit <- randomForest(as.factor(default)~., data=lending_club.train, importance=TRUE, ntree=400)

# Generate predictions based on model
lending_club.test$default.pred <- predict(rf.fit,lending_club.test)
########################
rf.pre.time <- proc.time() - ptm

# Create Confusion Matrix
cm.pre_rfe <- confusionMatrix(lending_club.test$default.pred,lending_club.test$default)
cm.pre_rfe

# area under a ROC curve
#auc(lending_club.test$default,as.numeric(lending_club.test$default.pred))

# Let's use RFE to see if we can prune some variables/features and hopefully get a better result
set.seed(77)

# 10-fold cross-validation
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
lending_club.train.rfe <- lending_club.train[sample(nrow(lending_club.train),2000),]
rfe.train <- rfe(lending_club.train.rfe[,1:74], as.factor(lending_club.train.rfe[,75]), sizes=1:74, rfeControl=control)

# how big is the optimal variable subset?
print(rfe.train$bestSubset)

plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:74)

predictors(rfe.train)

predictors <- predictors(rfe.train)
formula <- paste("as.factor(default)",paste(predictors, collapse=" + "), sep="~")
formula <- as.formula(formula)
formula

# Redo the Random Forest model with the optimal subset according to RFE
set.seed(44)

ptm <- proc.time()
#########################
rf.fit.opt <- randomForest(formula, data=lending_club.train, ntree=400, type='classification')

lending_club.test$default.pred.opt <- predict(rf.fit.opt,lending_club.test)
########################
rf.post.time <- proc.time() - ptm

# Create Confusion Matrix
cm.post_rfe <- confusionMatrix(lending_club.test$default.pred.opt,lending_club.test$default)
cm.post_rfe

set.seed(3)
# BAGGED DECISION TREES
library(rpart)
#install.packages("adabag", repos='http://cran.us.r-project.org',dependencies = TRUE)
library(adabag)
lending_club.train$default.factor <- as.factor(lending_club.train$default)
lending_club.test$default.factor <- as.factor(lending_club.test$default)
formula_bdt <- paste("default.factor",paste(predictors,collapse=" + "),sep="~")

ptm <- proc.time()
#########################
# mfinal indicates total number of trees grown 
# and minsplit is the minimum number of observations that must exist in a node in order for a split to be attempted
bdt.bagging <- bagging(formula_bdt, mfinal=500, control=rpart.control(minsplit = 50), data=lending_club.train)

# make predictions
bdt.bagging.pred <- predict.bagging(bdt.bagging, newdata=lending_club.test)
########################
bdt.time <- proc.time() - ptm

# Create Confusion Matrix
cm.bdt <- confusionMatrix(bdt.bagging.pred$class,lending_club.test$default)
cm.bdt

# find Area Under a ROC Curve (AUC)
#auc(lending_club.test$default,as.numeric(bdt.bagging.pred$class))

set.seed(10)

ptm <- proc.time()
#########################
# create SVM model
lc.svm.fit <- svm(formula, data=lending_club.train)

# make predictons
lending_club.test$default.pred.svm <- predict(lc.svm.fit,lending_club.test)
########################
svm.time <- proc.time() - ptm

# Create Confusion Matrix
cm.svm <- confusionMatrix(lending_club.test$default.pred.svm,lending_club.test$default)
cm.svm

lending_club.init_sol <- lending_club_final

# train and test datasets
train_rows <- sample(nrow(lending_club.init_sol),(nrow(lending_club.init_sol)*0.7))
lending_club.init_sol.train <- lending_club.init_sol[train_rows,]
lending_club.init_sol.test <- lending_club.init_sol[-train_rows,]

lending_club.init_sol.train$default <- factor(lending_club.init_sol.train$default, levels=c(0,1), labels=c('PAID', 'DEFAULT'))
lending_club.init_sol.test$default <- factor(lending_club.init_sol.test$default, levels=c(0,1), labels=c('PAID', 'DEFAULT'))

# create model
default_pred.ctree <- ctree(formula, data=lending_club.init_sol.train, 
                               controls=ctree_control(testtype="Bonferroni", minsplit=4000, minbucket=4000))

# save model of tree to disk
png("ctree.png", res=80, height=900, width=2000) 
   plot(default_pred.ctree) 
dev.off()

lending_club.init_sol.test$default.pred <- predict(default_pred.ctree, newdata=lending_club.init_sol.test, type='response')
cm.ctree <- confusionMatrix(lending_club.init_sol.test$default.pred, lending_club.init_sol.test$default, positive="PAID")

# poplulate Accuracy (PCC)
acc <- cbind(as.numeric(cm.post_rfe$overall)[1],as.numeric(cm.bdt$overall)[1],as.numeric(cm.svm$overall)[1])

# populate Kappa
kap <- cbind(as.numeric(cm.post_rfe$overall)[2],as.numeric(cm.bdt$overall)[2],as.numeric(cm.svm$overall)[2])

# populate Sensitivity
sen <- cbind(as.numeric(cm.post_rfe$byClass)[1],as.numeric(cm.bdt$byClass)[1],as.numeric(cm.svm$byClass)[1])

# populate Specificity
spec <- cbind(as.numeric(cm.post_rfe$byClass)[2],as.numeric(cm.bdt$byClass)[2],as.numeric(cm.svm$byClass)[2])

# populate AUC
auc <- cbind(as.numeric(cm.post_rfe$byClass)[11],as.numeric(cm.bdt$byClass)[11],as.numeric(cm.svm$byClass)[11])

perf.mat <- rbind(acc,kap,sen,spec,auc)
rownames(perf.mat) <- c("PCC (1)","Kappa (2)","Sensitivity (3)","Specificity (4)","AUC (5)")
colnames(perf.mat) <- c("Random Forest - Post RFE","Bagged Decision Trees","SVM")

rf.mat <- as.matrix(rf.post.time)
svm.mat <- as.matrix(svm.time)
bdt.mat <- as.matrix(bdt.time)

et <- cbind(rf.mat[3], svm.mat[3], bdt.mat[3])
colnames(et) <- c("Random Forests","SVM","Bagged DT")

matplot(perf.mat, type = c("b"),pch=1,col = 2:4, ylab="rate", main="Predictive Models Performance Results")
legend("bottomright", legend = c("Random Forest - Post RFE","Bagged Decision Trees","SVM"), col=2:4, pch=1) 
perf.mat

et
barplot(et, main="Classification Model Time Performance", ylab="elapsed time in seconds")

dataDict <- read.table("LCDataDictionary.csv", header = TRUE, sep=",")
dataDict[which(dataDict$Feature %in% predictors(rfe.train)),]

cm.ctree

lc_tiny_test <- lending_club.test[sample(nrow(lending_club.test),15),1:75]

lc_tiny_test$default.pred <- predict(rf.fit.opt,lc_tiny_test)

lc_tiny_test <- cbind(lc_tiny_test[,which(colnames(lc_tiny_test) %in% predictors(rfe.train))],
                      default=lc_tiny_test[,75], default.pred=lc_tiny_test[,76])
lc_tiny_test

dataDict


