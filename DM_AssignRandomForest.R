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














