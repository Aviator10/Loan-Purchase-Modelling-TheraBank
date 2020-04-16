getwd()
mydata<-read_excel('Thera Bank-Data Set.xlsx',sheet = 2)
str(mydata)
head(mydata)
summary(mydata)

sum(is.na(mydata))
sum(is.na(mydata[6]))
sum(is.na(mydata[2]))
sum(is.na(mydata[3]))
sum(is.na(mydata[4]))
sum(is.na(mydata[5]))
sum(is.na(mydata[7]))
sum(is.na(mydata[8]))
sum(is.na(mydata[9]))
sum(is.na(mydata[10]))
sum(is.na(mydata[11]))
sum(is.na(mydata[12]))
sum(is.na(mydata[13]))
sum(is.na(mydata[14]))


median(mydata$`Family members`,na.rm = TRUE)
mydata$`Family members`[which(is.na(mydata$`Family members`))]<-median(mydata$`Family members`,na.rm = TRUE)
sum(is.na(mydata[6]))


mydata[rowSums(mydata < 0) == 0,]
which(mydata$`Experience (in years)`<0)
mydata$`Experience (in years)`[which(mydata$`Experience (in years)`<0)]<-0

boxplot(mydata$`Age (in years)`)
boxplot(mydata$`Experience (in years)`)
boxplot(mydata$`Income (in K/month)`)

for(i in 2:14){
  outlier_check<-boxplot(mydata[i],plot = FALSE)$Out
  print(outlier_check)
}


mydata1<-mydata[,-c(1,5)]
multi.hist(mydata1)
boxplot(mydata1,col='BLUE',horizontal = TRUE,las=2,cex.axis=0.4)

plot(mydata1$CCAvg,mydata1$Mortgage)
plot(mydata1$CCAvg,mydata1$`Income (in K/month)`)
plot(mydata1$Education,mydata1$`Income (in K/month)`)
plot(mydata1$Mortgage,mydata1$`Income (in K/month)`)
plot(mydata1$`Family members`,mydata1$`Personal Loan`)

cor.plot(mydata1,numbers = TRUE,xlas=2)


mydata2<-mydata[,-c(1,5)]
table(mydata2$`Personal Loan`)
mydata2$`Age (in years)`<-scale(mydata2$`Age (in years)`)
mydata2$`Experience (in years)`<-scale(mydata2$`Experience (in years)`)
mydata2$`Income (in K/month)`<-scale(mydata2$`Income (in K/month)`)
mydata2$`Family members`<-scale(mydata2$`Family members`)
mydata2$CCAvg<-scale(mydata2$CCAvg)
mydata2$Education<-scale(mydata2$Education)
mydata2$Mortgage<-scale(mydata2$Mortgage)

km.res<-kmeans(mydata2,5,nstart = 25)
print(km.res)

fviz_nbclust(mydata2,kmeans,method = 'wss',print.summary = TRUE,k.max = 40)
km.res<-kmeans(mydata2,13,nstart = 25)
print(km.res)
fviz_cluster(km.res,mydata2,ellipse.type='norm')

set.seed(1234)
sample<-sample.split(mydata2,SplitRatio=0.6)
p_train<-subset(mydata2,sample==TRUE)
p_test<-subset(mydata2,sample==FALSE)
table(p_train$`Personal Loan`)
table(p_test$`Personal Loan`)

#define control parameters
r.ctrl <- rpart.control(minsplit = 515, minbucket = 172,cp=0.33,xval = 10)#972

attach(mydata2)
#To make the model
m1 <- rpart(formula = `Personal Loan`~., data = p_train, method = "class", control = r.ctrl)

#To draw decision tree
fancyRpartPlot(m1)
plot(m1)


#To find the accuracy of the train data, we'll need the predicted class
#for each case and the predicted probability
p_train$predict.class <- predict (m1, p_train, type = "class")
head(p_train)

p_train$predict.score<-predict(m1, p_train)
head(p_train)

#Draw confusion matrix
with(p_train, table(`Personal Loan`, predict.class))
#accuracy = (2623+195)/2917 = 97%

#Applying on test data
p_test$predict.class <- predict (m1, p_test, type = "class")
head(p_test)

p_test$predict.score<-predict(m1, p_test)
head(p_test)

#Draw confusion matrix
with(p_test, table(`Personal Loan`, predict.class))
#accuracy = (1897+102)/2083 = 96%




# Get KS , AUC and Gini

pred_test <- prediction(p_test$predict.score[,2], p_test$`Personal Loan`)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test)
KS_test <- max(attr(perf_test, 'y.values')[[1]]-attr(perf_test, 'x.values')[[1]])
KS_test
auc_test <- performance(pred_test,"auc")
auc_test <- as.numeric(auc_test@y.values)
auc_test
gini_test = ineq(p_test$predict.score[,2], type="Gini")
gini_test

#Pruning
bestcp <- m1$cptable[which.min(m1$cptable[,"xerror"]), "CP"]
bestcp
m1_pruned<- prune(m1, cp= bestcp ,"CP")


#RandomForest

getwd()
mydata<-read_excel('Thera Bank-Data Set.xlsx',sheet = 2)

sum(is.na(mydata[6]))
mydata$`Family members`[which(is.na(mydata$`Family members`))]<-median(mydata$`Family members`,na.rm = TRUE)

which(mydata$`Experience (in years)`<0)
mydata$`Experience (in years)`[which(mydata$`Experience (in years)`<0)]<-0

mydataRF<-mydata[,-c(1,5)]
table(mydataRF$`Personal Loan`)

#Changing to categorical variables
mydataRF$`Family members`<-as.factor(mydataRF$`Family members`)
mydataRF$Education<-as.factor(mydataRF$Education)
mydataRF$`Securities Account`<-as.factor(mydataRF$`Securities Account`)
mydataRF$`CD Account`<-as.factor(mydataRF$`CD Account`)
mydataRF$Online<-as.factor(mydataRF$Online)
mydataRF$CreditCard<-as.factor(mydataRF$CreditCard)
mydataRF$`Personal Loan`<-as.factor(mydataRF$`Personal Loan`)

#Scaling of variables.
mydataRF$`Age (in years)`<-scale(mydataRF$`Age (in years)`)
mydataRF$`Experience (in years)`<-scale(mydataRF$`Experience (in years)`)
mydataRF$`Income (in K/month)`<-scale(mydataRF$`Income (in K/month)`)
mydataRF$CCAvg<-scale(mydataRF$CCAvg)
mydataRF$Mortgage<-scale(mydataRF$Mortgage)

#Changing variable names
names(mydataRF)[1]<-"Age"
names(mydataRF)[2]<-"Experience"
names(mydataRF)[3]<-"Income"
names(mydataRF)[4]<-"FamilyMembers"
names(mydataRF)[8]<-"PersonalLoan"
names(mydataRF)[9]<-"SecuritiesAccount"
names(mydataRF)[10]<-"CDAccount"
class(mydataRF$PersonalLoan)

#Splitting train and test
set.seed(1234)
sample1<-sample.split(mydataRF,SplitRatio=0.6)
p_trainRF<-subset(mydataRF,sample1==TRUE)
p_testRF<-subset(mydataRF,sample1==FALSE)
table(p_trainRF$PersonalLoan)
table(p_testRF$PersonalLoan)
class(p_trainRF)

#Running random forest
mtry<-tuneRF(p_trainRF[-8],p_trainRF$PersonalLoan,ntreeTry = 500,stepFactor = 1.5,improve = 0.01,
             trace = TRUE,plot = TRUE)

rf<-randomForest(formula= PersonalLoan~.,data = p_trainRF, mtry=6, ntree=500,importance=TRUE)
varImpPlot(rf)
print(rf)

#Accuracy 98.7%

#Predicting on Validation set
predTest <- predict(rf, p_testRF, type = "class")
# Checking classification accuracy
mean(predTest == p_testRF$PersonalLoan)                    
table(predTest,p_testRF$PersonalLoan) 
# Accuracy 98.7


# Get KS , AUC and Gini

p_testRF$predict.class <- predict(rf, p_testRF, type="class")
p_testRF$predict.score <- predict(rf, p_testRF, type="prob")


pred_test_rf <- prediction(p_testRF$predict.score[,2], p_testRF$PersonalLoan)
perf_test_rf <- performance(pred_test_rf, "tpr", "fpr")
plot(perf_test_rf)
KS_test_rf <- max(attr(perf_test_rf, 'y.values')[[1]]-attr(perf_test_rf, 'x.values')[[1]])
KS_test_rf
auc_test_rf <- performance(pred_test_rf,"auc"); 
auc_test_rf <- as.numeric(auc_test_rf@y.values)
auc_test_rf

gini_test_rf = ineq(p_testRF$predict.score[,2], type="Gini")
gini_test_rf





