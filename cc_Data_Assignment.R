## Last amended: 1st June, 2018
# Otto : Kaggle Project
# Data is anonymised
# There are 93 features for a product.
# Refer: https://www.kaggle.com/c/otto-group-product-classification-challenge/data
# Feature Engineering:
#        https://www.slideshare.net/eugeneyan/kaggle-otto-challenge-how-we-achieved-85th-out-of-3845-and-what-we
#        https://kaggle2.blob.core.windows.net/forum-message-attachments/79598/2514/FINAL_ARCHITECTURE.png
# Feature Extraction with KNN
#        http://davpinto.com/fastknn/articles/knn-extraction.html
#
#
#  NOTE: Code here is same as in file: otto_featureEngineering.R
#        except that PCA portion is added here

# Objectives:
#            a. Data preprocessing: Center, Scale
#            b. Feature Engineering
#            c. Data modeling (decision tree)
#            d. PCA
#            d. Model evaluation
#            e. Feature importance (NOT POSSIBLE)
#
# 1
## install and load the decison tree package-----

library(fastknn)
library(C50)			  # For decision tree induction classification
library(dplyr)			# For data manipulation
library(ggplot2)		# For plotting 
library(caret)			# for data-preprocessing & for data-partitioning
library(fastknn)
# library("devtools")
# install_github("davpinto/fastknn")
library(dplyr)
library(magrittr)
library(forcats) 

# 2
### Read and understand data----------

# 2.1
# Set working directory and read the data
#   Note: File otto.r is in fdp/fdp/decisiontree folder
#setwd("C:/Users/ashokharnal/OneDrive/Documents/xgboost/otto")
setwd("C:\\Users\\vikandul\\Desktop\\BIG DATA - FORE SCHOOL COURSE\\Class Notes and Supporting documentation\\Week06092018\\decisiontree")

# 2.2 Read data
train<-read.csv("train.csv",header=T)
dim(train)           # 61878 X 95
head(train)          # Last column (95th) is target
str(train)

# 2.3
# Decison tree C50 requires that 'target' be a factor
train$target<-as.factor(train$target)

# 2.4 
# How many class-categories? Nine.
levels(train$target)

## 3.0 Transform data/Feature Engineering ---------------

# 3.1
# Check NAs
sum(is.na(train))		# There is none

# 3.2 Drop the id column
train$id <- NULL

################################ Feature Engineering #########################

"""
# 3.3 Feature 1: Row sums of features 1:93
train$rowsum<-rowSums(train[,1:93])


# 3.4 Feature 2: Row variances of features 1:93
train$rowvar<-  rowSums((train[,1:93] - rowMeans(train[,1:93]))^2)/(dim(train)[2] - 1)


# 3.5 Count of number of zeros in rows
train$rowzeros<-rowSums(train[,1:93] == 0)
head(train)

"""
View(cc_data)

sum(cc_data$MARRIAGE =='0')

# AVERAGE CREDIT BILL AMOUNT
for(i in 1:nrow(cc_data)){
  cc_data$avg_crd_bill[i] <- round((cc_data$BILL_AMT1[i] +cc_data$BILL_AMT2[i]+cc_data$BILL_AMT3[i]+cc_data$BILL_AMT4[i]+cc_data$BILL_AMT5[i]+cc_data$BILL_AMT6[i])/6,2)
}

# AVERAGE Payment made
for(i in 1:nrow(cc_data)){
  cc_data$avg_pay_amt[i] <- round((cc_data$PAY_AMT1[i] +cc_data$PAY_AMT2[i]+cc_data$PAY_AMT3[i]+cc_data$PAY_AMT4[i]+cc_data$PAY_AMT5[i]+cc_data$PAY_AMT6[i])/6,2)
}

str(cc_data)
dim(cc_data)

View(cc_data)
dim(cc_data)

cc_data$ID<- NULL

cc_data$age_class<-cut(cc_data$AGE, 3,labels = c("young", "middle","senior"))

#leave out response variable and age attribute which doesn't 

model_data<- cc_data[,c(1,6:23, 26,24)]

head(model_data)

model_data$default.payment.next.month<- as.factor(model_data$default.payment.next.month)


dim(model_data)

transformed_data = preProcess(model_data[,c(-21)],  method=c("center", "scale"))

#Get transfromed data using predict() from 'trans' model
PC = predict(transformed_data, model_data[,c(-21)])		# Returns scaled, centered data 
dim(PC)      
class(PC) 


model_train <- cbind(PC,target=model_data[,21])
dim(model_train)      # 61878 X 97
head(model_train)

# Partition data in train/validation sets.
#   Partition in a stratified manner
trainindex<-createDataPartition(model_train$target,p=0.8,list=FALSE)
training_data<-model_train[trainindex,]  ;     dim(training_data)
validation_data<-model_train[-trainindex,]  ; dim(validation_data)
head(training_data)



system.time(
  newfe   <- knnExtract(                                      # It is FastKNN function
    xtr = data.matrix(training_data[,-21]),          # 97 is target
    ytr = training_data[,21],
    xte = data.matrix(validation_data[,-21]),
    k = 3
  )
)

# 5.2
View(newfe$new.tr)    ;dim(newfe)      # New training features
View(newfe$new.te)          # New test features

# 5.3
dim(newfe$new.tr)           # 49507 X  27
dim(newfe$new.te)           # 12371 X  27

# 5.4
class(newfe$new.tr)
class(newfe$new.te)



# 5.5 Scale and center these new features using caret (preProcess())
new<-rbind(newfe$new.tr, newfe$new.te)
trans = preProcess(new,  method=c("center", "scale"))
PC = predict(trans, new)		# Returns scaled, centered data 
head(PC)

dim(training_data)

# 6.1 Column-wise stack complete set of predictors
#     Original + New features (exclude target)
fx<-cbind(training_data[,-21], PC[1:nrow(newfe$new.tr), ])     

dim(validation_data)

# 6.2
dim(fx)            # 49507 X 123
class(fx)          # data.frame


# 7.1  Column-wise stack complete set of validation features
#      (exclude target)
vx<-cbind(validation_data[,-21], PC[(nrow(newfe$new.tr)+1) : nrow(PC), ])

dim(training_data)
head(training_data)

# 7.2
dim(vx)            # 12371 X 123
validation_data[,21]         # class values

# 8.0 Model now
system.time(model <-                       # Note 's' is small in system.time()
              C5.0(
                x = fx,                     # Predictors (only)
                y = training_data[, 21],               # Target values
                trials = 10,                # Boosting helps in generalization
                # No of boosting steps
                control = C5.0Control       # parameter control list. May specify separately
                (
                  noGlobalPruning = FALSE,  # Should global pruning be done? FALSE implies: Yes, do it
                  # FALSE => More generlization
                  CF = 0.15,                # Larger CF (0.75)=>Less tree-pruning.
                  # Smaller values (0.1) =>More tree pruning & more general
                  minCases = 4,             # Min cases per leaf-node.
                  # More cases-> More generalization
                  #sample = 0.80,           # Take 80% sample for training
                  winnow = TRUE,            # TRUE may make it more general, FALSE less general
                  earlyStopping = TRUE      # Should boosting be stopped early?
                )
              )
)

# 8.1 Make predictions now of type class
out <- predict(model, vx , type = "class")
out
# 8.2 Create a dataframe of actual and predicted classes
#     for quick comparisons
df_comp <- data.frame(predicted = out, actual = validation_data[,21])
View(df_comp)

# 8.3
#  Check accuracy of predictions
#  Create confusion matrix. 'dnn' stands for dimension names

table(
  df_comp$actual,
  df_comp$predicted,
  dnn = c("Actual", "Predicted")    # Table headings
)

# 8.4 Calculate accuracy   
accuracy <- sum(as.character(df_comp$actual) == as.character(df_comp$predicted)) / nrow(df_comp)
accuracy

################################ Feature Importance #########################


# 8.5 Which variables were important in predictions
impt<-C5imp(model)

# 8.6
row.names(impt)

# 9.
df<-data.frame(x = row.names(impt), y = impt$Overall)

# 9.1 Filter out variables whose oevrall contribution is zero
df %<>% filter (df$y > 0 )

# 9.2 Remaining number
nrow(df)         # 83





#############################################################################################################

