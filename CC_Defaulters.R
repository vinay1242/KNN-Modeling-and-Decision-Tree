

library(ggplot2)   # For plotting functions
library(ggthemes)	 # For a variety of plot-themes
library(dplyr)     # verbs: select, filter, mutate, group_by, summarise
library(gridExtra) 


setwd('C:\\Users\\vikandul\\Desktop\\BIG DATA - FORE SCHOOL COURSE\\Class Notes and Supporting documentation\\Week_06162018\\Excercise\\UCI_Credit_Card.csv')
cc_data <- read.csv("UCI_Credit_Card.csv", header =TRUE)
head(cc_data)

# ID field is not required
cc_data$ID<- NULL

# Target field should be categorical
cc_data$default.payment.next.month<- as.factor(cc_data$default.payment.next.month)

#sex , education, marriage should be categorical
cc_data$SEX<- as.factor(cc_data$SEX)
cc_data$EDUCATION<- as.factor(cc_data$EDUCATION)
cc_data$MARRIAGE<- as.factor(cc_data$MARRIAGE)

# convert Age in multiple categories like young, middle , old etc..
cc_data$age_class<-cut(cc_data$AGE, 3,labels = c("young", "middle","senior"))


cc_data$age_class<-cut(cc_data$AGE, 3,labels = c("young", "middle","senior"))


str(cc_data)


# education have values 0 which is undefined and 5 and 6 are unknown


dim(credit_card_data    )

sum(is.na(cc_data))


cc_trans = preProcess(cc_data[,c(-24)],  method=c("center", "scale"))

PC = predict(cc_trans, cc_data[,c(-24)])	

View(PC)

education<-ggplot(data=cc_data, aes(x=EDUCATION, fill = as.factor(default.payment.next.month)))    +geom_bar(position="stack")  +geom_text(stat='count', aes(label=..count..), size = 3, position = position_stack(vjust=0))
age_class<-ggplot(data=cc_data, aes(x=age_class, fill = as.factor(default.payment.next.month)))    +geom_bar(position="stack")  +geom_text(stat='count', aes(label=..count..), size = 3, position = position_stack(vjust=0))
sex_class<-ggplot(data=cc_data, aes(x=SEX, fill = as.factor(default.payment.next.month)))    +geom_bar(position="stack")  +geom_text(stat='count', aes(label=..count..), size = 3, position = position_stack(vjust=0))
marriage_class<-ggplot(data=cc_data, aes(x=MARRIAGE, fill = as.factor(default.payment.next.month)))    +geom_bar(position="stack")  +geom_text(stat='count', aes(label=..count..), size = 3, position = position_stack(vjust=0))

grid.arrange(education, age_class, sex_class, marriage_class)


ggplot(cc_data, aes(age_class))+geom_bar()

ggplot(cc_data, aes(x=PAY_AMT1, y= BILL_AMT1))+geom_area( )

pay0<- ggplot(cc_data, aes(PAY_0, fill = default.payment.next.month))+geom_area()
pay2<- ggplot(cc_data, aes(PAY_2))+geom_bar()
pay3<- ggplot(cc_data, aes(PAY_3))+geom_bar()
pay4<- ggplot(cc_data, aes(PAY_4))+geom_bar()
pay5<- ggplot(cc_data, aes(PAY_5))+geom_bar()
pay6<- ggplot(cc_data, aes(PAY_6))+geom_bar()







grid.arrange(pay0,pay2,pay3,pay4         )
grid.arrange(payamt1,payamt2,payamt3,payamt4,payamt5,payamt6 )


geom_text(size = 3, position = position_stack(vjust = 0.5))

ggplot(data=credit_card_data, aes(x=SEX)) +            # Opens window
  geom_bar(width=0.5,color="red",color="yellow") +  # Parameters specific to bar-layer  
  theme_solarized()   


  
View(credit_card_data)

len(cc_data)

credit_card_data$SEX<- as.factor(credit_card_data$SEX)
levels(credit_card_data$SEX)<- c("male", "female")
levels(credit_card_data$SEX)
i=1
#--- classify pay_0 to 6 into 2 groups 0 and 1


for(i in 1:nrow(cc_data)){
   if(( cc_data$PAY_6[i]<= 0)){
      cc_data$pp6[i] = 0
  }
    else{
  cc_data$pp6[i] = 1
    }
}

# AVERAGE CREDIT BILL AMOUNT
for(i in 1:nrow(cc_data)){
  cc_data$avg_crd_bill[i] <- round((cc_data$BILL_AMT1[i] +cc_data$BILL_AMT2[i]+cc_data$BILL_AMT3[i]+cc_data$BILL_AMT4[i]+cc_data$BILL_AMT5[i]+cc_data$BILL_AMT6[i])/6,2)
    }

# AVERAGE Payment made
for(i in 1:nrow(cc_data)){
  cc_data$avg_pay_amt[i] <- round((cc_data$PAY_AMT1[i] +cc_data$PAY_AMT2[i]+cc_data$PAY_AMT3[i]+cc_data$PAY_AMT4[i]+cc_data$PAY_AMT5[i]+cc_data$PAY_AMT6[i])/6,2)
}






View(cc_data)

cc_data$PAY_0[2]

class(nrow(cc_data))

dim(cc_data)

model_data<- cc_data[,c(2,4,5,25,26,27,28,29,30,31,32,33) ]

View(model_data)

model_data$default.payment.next.month<- as.factor(model_data$default.payment.next.month)


model_trans = preProcess(model_data[,c(-4)],  method=c("center", "scale"))


# 3.7
# Get transfromed data using predict() from 'trans' model
PC = predict(model_trans, model_data[,c(-4)])		# Returns scaled, centered data 
dim(PC)      # 77 columns
class(PC)    # it is a data frame
View(PC)



#------------------------------------------------


# 3.8
# Reconstruct complete data by including
#  target column but not id column
cc_data_train <- cbind(PC,target=model_data[,4])
dim(cc_data_train)      # 61878 X 97
head(cc_data_train)    # Target is 97th column



## 4.0 Partition data ----------

# 4.1
# Partition data in train/validation sets.
#   Partition in a stratified manner
trainindex<-createDataPartition(cc_data_train$target,p=0.8,list=FALSE) # this will return index values and get stored to trainindex.
tr<-cc_data_train[trainindex,]  ;     dim(tr)
valid<-cc_data_train[-trainindex,]  ; dim(valid)


# 4.2 Just have a look
head(tr)
dim(tr)
View(tr)


################################ More Feature Engineering #########################

# 5.0 Feature engineering using fastknn
#      fastknn generates k * c new features, where c is the number of class labels.
#      The new features are computed from the distances between the observations
#      and their k nearest neighbors inside each class, as follows:
#  
#       First test feature contains the distances between each test instance
#        and its nearest neighbor inside the first class.
#       Second test feature contains the sums of distances between each test
#        instance and its 2 nearest neighbors inside the first class.
#       Third test feature contains the sums of distances between each test
#        instance and its 3 nearest neighbors inside the first class.
#         And so on.

# 5.1  Takes time... wait...May take 10 minutes
system.time(
  newfe   <- knnExtract(                                      # It is FastKNN function
    xtr = data.matrix(tr[,-12]),          # 97 is target xtr, ytr and xte are function attributes.
    ytr = tr[,12],                          # xtr - stores training predictor fields , ytr stores training target field, xte -stores validation data   
    xte = data.matrix(valid[,-12]),
    k = 3  # number of nearest neighbours to consider for KNN 
  )
)

# 5.2
View(newfe$new.tr)          # New training features
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


# 6.1 Column-wise stack complete set of predictors
#     Original + New features (exclude target)
fx<-cbind(tr[,-12], PC[1:nrow(newfe$new.tr), ])     


# 6.2
dim(fx)            # 49507 X 123
class(fx)          # data.frame


# 7.1  Column-wise stack complete set of validation features
#      (exclude target)
vx<-cbind(valid[,-12], PC[(nrow(newfe$new.tr)+1) : nrow(PC), ])


# 7.2
dim(vx)            # 12371 X 123
valid[,12]         # class values

################################ Modeling #########################


### 8. Model building and testing------

# 8.0 Model now
system.time(model <-                       # Note 's' is small in system.time()
              C5.0(
                x = fx,                     # Predictors (only)
                y = tr[, 12],               # Target values
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
df_comp <- data.frame(predicted = out, actual = valid[,12])
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

# 9.3 Plot these to get an idea of variable importance

ggplot(df, aes(x = fct_reorder(x,y), y =y)) +
  geom_bar(stat = "identity") + 
  coord_flip()



#######################################################
set.seed(1234)
pd<- sample(2, nrow(model_data), replace = TRUE, prob = c(0.8,0.2))
train<- model_data[pd==1,]
validate<- model_data[pd==2,]

View(train)
library(rpart)
library(party)
tree<- ctree(default.payment.next.month ~ LIMIT_BAL+EDUCATION+MARRIAGE+avg_crd_bill+avg_pay_amt, data =model_data, controls = ctree_control(mincriterion = .9,minsplit = 200))
tree
plot(tree)
predict(tree,validate)


tree1<- rpart(default.payment.next.month ~ LIMIT_BAL+EDUCATION+MARRIAGE+avg_crd_bill+avg_pay_amt, data =train)
library(rpart.plot)
predict(tree1, validate)

tab<- table(predict(tree1), train$default.payment.next.month)
