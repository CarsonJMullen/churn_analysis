################################################################################
#Telco Customer Churn Data - Boosting Validation
################################################################################

#read-in data
ca <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE)

#----------Encoding-----------
ca$gender_encode <- as.numeric(factor(ca$gender))
ca$Partner_encode <- as.numeric(factor(ca$Partner))
ca$PhoneService_encode <- as.numeric(factor(ca$PhoneService))
ca$MultipleLines_encode <- as.numeric(factor(ca$MultipleLines))
ca$InternetService_encode <- as.numeric(factor(ca$InternetService))
ca$OnlineBackup_encode <- as.numeric(factor(ca$OnlineBackup))
ca$DeviceProtection_encode <- as.numeric(factor(ca$DeviceProtection))
ca$TechSupport_encode <- as.numeric(factor(ca$TechSupport))
ca$StreamingTV_encode <- as.numeric(factor(ca$StreamingTV))
ca$StreamingMovies_encode <- as.numeric(factor(ca$StreamingMovies))
ca$Contract_encode <- as.numeric(factor(ca$Contract))
ca$PaperlessBilling_encode <- as.numeric(factor(ca$PaperlessBilling))
ca$PaymentMethod_encode <- as.numeric(factor(ca$PaymentMethod))

ca_x <- subset(ca, select = c("gender_encode","Partner_encode","PhoneService_encode",
                              "MultipleLines_encode","InternetService_encode","OnlineBackup_encode",
                              "DeviceProtection_encode","TechSupport_encode","StreamingTV_encode",
                              "StreamingMovies_encode","Contract_encode","PaperlessBilling_encode",
                              "PaymentMethod_encode","MonthlyCharges","TotalCharges","Churn","SeniorCitizen"))


#--------------------------------------------------
ca <- ca_x
logMedVal <- ca$Churn
ca$logMedVal = logMedVal
#--------------------------------------------------

#train, val, test
set.seed(99)
n=nrow(ca)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
catrain=ca[ii[1:n1],]
caval = ca[ii[n1+1:n2],]
catest = ca[ii[n1+n2+1:n3],]

#-----------------------------------

set.seed(1)
idv = c(4,10) #depth of trees
ntv = c(1000,5000) #No. of tress
lamv=c(0.05, 0.1) 
parmb = expand.grid(idv,ntv,lamv)
colnames(parmb) = c('tdepth','ntree','lam')
print(parmb)
nset = nrow(parmb)
print(nset)
olb = rep(0,nset)
ilb = rep(0,nset)
bfitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing boost ',i,' out of ',nset,'\n')
  tempboost = gbm(logMedVal~.,data=catrain,distribution='gaussian',
                  interaction.depth=parmb[i,1],n.trees=parmb[i,2],shrinkage=parmb[i,3])
  ifit = predict(tempboost,n.trees=parmb[i,2])
  ofit=predict(tempboost,newdata=caval,n.trees=parmb[i,2])
  olb[i] = sum((caval$logMedVal-ofit)^2)
  ilb[i] = sum((catrain$logMedVal-ifit)^2)
  bfitv[[i]]=tempboost
}
ilb = round(sqrt(ilb/nrow(catrain)),3); olb = round(sqrt(olb/nrow(caval)),3)
#--------------------------------------------------
#print losses
print(cbind(parmb,olb,ilb))

#--------------------------------------------------
#write val preds
iib=which.min(olb)
theb = bfitv[[iib]] 
thebpred = predict(theb,newdata=caval,n.trees=parmb[iib,2])
write(thebpred,file='thebpred-2.txt',ncol=1)

################################################################################
#Telco Customer Churn Data - Boosting Test
################################################################################

#--------------------------------------------------
#fit on train+val
set.seed(1)
catrainval = rbind(catrain,caval)
ntrees=5000
finb = gbm(logMedVal~.,data=catrainval,distribution='gaussian',
           interaction.depth=4,n.trees=ntrees,shrinkage=.2)
finbpred=predict(finb,newdata=catest,n.trees=ntrees)
#--------------------------------------------------
#plot y vs yhat for test data and compute rmse on test.


finbrmse = sqrt(sum((catest$logMedVal-finbpred)^2)/nrow(catest))
cat('finbrmse: ',finbrmse,'\n')
plot(catest$logMedVal,finbpred,xlab='test logMedVal',ylab='boost pred')
abline(0,1,col='red',lwd=2)

#--------------------------------------------------
#plot variable importance
p=ncol(catrain)-1 #want number of variables for later
vsum=summary(finb) #this will have the variable importance info
row.names(vsum)=NULL #drop varable names from rows.



#write variable importance table
cat('\\begin{verbatim}\n')
print(vsum)
cat('\\end{verbatim}\n')

#plot variable importance
#the package does this automatically, but I did not like the plot
plot(vsum$rel.inf,axes=F,pch=16,col='red')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')

#--------------------------------------------------
#partial dependence plots

par(mfrow=c(3,3))
nms = names(catrain)[1:9]
for(i in 1:9) plot(finb,i=nms[i])

#--------------------------------------------------
rm(list=ls())

