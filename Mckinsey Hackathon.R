setwd("C:/Users/Kamal/Desktop/Naukari leni hai")
a<-read.csv("Train.csv",header=T)
#write.csv(a,"newa.csv",row.names=F)
#an<-read.csv("newa.csv",header=T)
#a$DOB<-as.Date(a$DOB,"%d/%m/%y")
install.packages("eeptools")
library(eeptools)
library(randomForest)
install.packages("AUC")
library(AUC)
install.packages("gbm")
library(gbm)
x <- as.Date(c("2018-01-19"))
temp<-age_calc(a$DOB,x,units = "years")
colSums(is.na(c))
max(a$DOB)
a$Approved<-as.factor(a$Approved)
a$Lead_Month<-as.factor(a$Lead_Month)
a$Lead_WD<-as.factor(a$Lead_WD)
a$Lead_WOM<-as.factor(a$Lead_WOM)
#a[a$Source=="S150",][1,]
a$Employer_Category2<-ifelse(is.na(a$Employer_Category2),4,a$Employer_Category2)
c$Employer_Category2<-ifelse(is.na(c$Employer_Category2),4,c$Employer_Category2)
c$Emp_Code<-ifelse(is.na(c$Emp_Code),-9,c$Emp_Code)
a$Customer_Existing_Primary_Bank_Code<-as.character(a$Customer_Existing_Primary_Bank_Code)
a$Customer_Existing_Primary_Bank_Code<-ifelse(a$Customer_Existing_Primary_Bank_Code %in% c('B050',	'B026',	'B041',	'B039',	'B043',	'B005',	'B011',	'B047',	'B002',	'B001',	'B046',	'B006',	'B019',	'B016',	'B004',	'B033',	'B014',	'B023',	'B036',	'B008'),a$Customer_Existing_Primary_Bank_Code,'s1')
a$Customer_Existing_Primary_Bank_Code<-as.factor(a$Customer_Existing_Primary_Bank_Code)
a$Employer_Category2<-as.factor(a$Employer_Category2)
a$bc<-as.numeric(substr(a$Customer_Existing_Primary_Bank_Code,2,4))
c$bc<-as.numeric(substr(c$Customer_Existing_Primary_Bank_Code,2,4))
a$bc<-ifelse(is.na(a$bc),-1,a$bc)
c$bc<-ifelse(is.na(c$bc),-1,c$bc)

b<-a[1:20000,]
b<-rbind(b,a[a$Source=="S150",][1,])
set.seed(57)
m2<-randomForest(Approved~Var1
                      +Source_Category
                      +Lead_WOM
                      +Lead_WD
                      #+Lead_Month
                      +Age
                      +Source
                      #+Source_Category
                      #+Contacted
                      +Primary_Bank_Type
                      #+Customer_Existing_Primary_Bank_Code
                      +Monthly_Income
                      +Emp_Code
                      +Employer_Category1
                      +Employer_Category2
                      +City_Category
                      #+bc
                      +Gender
                      +Existing_EMI
                      +Loan_Amount
                      +Loan_Period
                      +Interest_Rate
                      +EMI
                      +Same.Company.prior
                      +Actual_Income
                      ,data=a)

plot(roc(as.vector(predict(m,type = "prob")[,2]),a$Approved))
auc(roc(as.vector(predict(m1,type = "prob")[,2]),a$Approved))
ff<-as.factor(a$Approved)
auc(roc(as.vector(predict(m,n.trees=100,type="response")),ff))
c<-read.csv("test.csv")
c<-rbind(a[1,-27],c)
c<-c[-1,]
levels(c$Source) <- levels(a$Source)
sub<-as.data.frame(cbind(as.character(c$Id),predict(m2,c,type="prob")[,2]))
colnames(sub)<-c("ID","Approved")
write.csv(sub,"final.csv",row.names = F)

for(i in 1:22)
{
  print(sum(a[,9]==an[,9]))
}
unique(c$Source)

m<-gbm(Approved~Var1
       +Source_Category
       +Lead_WOM
       +Lead_WD
       #+Lead_Month
       +Age
       +Source
       #+Source_Category
       #+Contacted
       +Primary_Bank_Type
       #+Customer_Existing_Primary_Bank_Code
       +Monthly_Income
       #+Employer_Code
       +Employer_Category1
       +Employer_Category2
       +City_Category
       #+City_Code
       +Gender
       +Existing_EMI
       +Loan_Amount
       +Loan_Period
       +Interest_Rate
       +EMI
       +Same.Company.prior
                ,data=a,n.trees=300,distribution = "poisson")

ff<-as.factor(a$Approved)
auc(roc(as.vector(predict(m,n.trees=300,type="response")),ff))
sub<-as.data.frame(cbind(as.character(c$ID),predict(m,c,n.trees=500,type="response")))
colnames(sub)<-c("ID","Approved")
write.csv(sub,"2ndnodrama.csv",row.names = F)

new_a<-cbind(sample(1:5,nrow(a),replace = T),a)
colnames(new_a)[1]<-"fold"

for(r in )
cv_preds<-NULL
for (i in 1:5)
{
train<-new_a[new_a$fold!=i,]
test<-new_a[new_a$fold==i,]
s_mat<-sparse.model.matrix(Approved~.-1, data = train[,-1])
m<-xgboost(data = s_mat, label = as.vector(train$Approved), max_depth = 4,
        eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic",eval_metric="auc")
cv_preds<-rbind(cv_preds,cbind(predict(m,sparse.model.matrix(Approved~.-1, data = test)),as.integer(as.character(test$Approved))))
}    
cv_preds<-as.data.frame(cv_preds)
colnames(cv_preds)<-c("preds","labels")
auc(roc(as.vector(cv_preds$preds),factor(cv_preds$labels)))



s_mat<-sparse.model.matrix(Approved~.-1, data = a)
c$Approved<-1
m<-xgb.cv(data = s_mat, label = as.vector(a$Approved), max_depth = 4,
           eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic",eval_metric="auc",nfold=5)
subxg<-as.data.frame(cbind(as.character(c$Id),predict(m,sparse.model.matrix(Approved~.-1, data = c))))
colnames(subxg)<-c("ID","Approved")
write.csv(subxg,"vgb1.csv",row.names = F)
