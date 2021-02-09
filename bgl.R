rm(list=ls())
bg=read.csv("bgl.csv",na.strings = c("",NA))
str(bg)
sapply(bg, function(x) sum(is.na(x)))
summary(bg)
dim(bg)
library(dplyr)
library(ggplot2)
bg%>%ggplot(aes(area_type))+geom_bar(aes(fill=area_type))+guides(fill=F)+ggtitle("Area Type")
table(bg$size)
table(bg$society)
bg%>%ggplot(aes(bath))+geom_histogram(bins=50,fill="blue",col="black")+ggtitle("Bath")

bg%>%ggplot(aes(price))+geom_histogram(bins=50,fill="blue",col="black")+ggtitle("Price")

bg%>%filter(is.na(size))

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

bg=delete.na(bg,2)

sapply(bg, function(x) sum(is.na(x)))

bg%>%filter(is.na(location))
bg%>%filter(society=="Grare S")

bg$location[is.na(bg$location)]="Anantapura"

unique(bg$size)

bg%>%filter(is.na(bath))

bg$size=gsub("[^[:digit:].]", "",bg$size)


i1=with(bg,grepl("Acres",total_sqft))
ac=bg[i1,]
i2=with(bg,grepl("Cents",total_sqft))
ce=bg[i2,]
i3=with(bg,grepl("Grounds",total_sqft))
gr=bg[i3,]
i4=with(bg,grepl("Guntha",total_sqft))
gn=bg[i4,]
i5=with(bg,grepl("Perch",total_sqft))
pe=bg[i5,]
i6=with(bg,grepl("Sq. Meter",total_sqft))
sm=bg[i6,]
i7=with(bg,grepl("Sq. Yards",total_sqft))
sq=bg[i7,]


ac$total_sqft=gsub("[^[:digit:].]", " ",ac$total_sqft)
ac$total_sqft=as.numeric(ac$total_sqft)
ac$total_sqft=ac$total_sqft*43560

ce$total_sqft=gsub("[^[:digit:].]", " ",ce$total_sqft)
ce$total_sqft=as.numeric(ce$total_sqft)
ce$total_sqft=ce$total_sqft*435.6

gr$total_sqft=gsub("[^[:digit:].]", "",gr$total_sqft)
gr$total_sqft=as.numeric(gr$total_sqft)
gr$total_sqft=gr$total_sqft*2400

gn$total_sqft=gsub("[^[:digit:].]", " ",gn$total_sqft)
gn$total_sqft=as.numeric(gn$total_sqft)
gn$total_sqft=gn$total_sqft*1088.98


pe$total_sqft=gsub("[^[:digit:].]", " ",pe$total_sqft)
pe$total_sqft=as.numeric(pe$total_sqft)
pe$total_sqft=pe$total_sqft*272.25


sm$total_sqft=gsub("[^[:digit:]]","",sm$total_sqft)
sm$total_sqft=as.numeric(sm$total_sqft)
sm$total_sqft=sm$total_sqft*10.7639


sq$total_sqft=gsub("[^[:digit:]]", "",sq$total_sqft)
sq$total_sqft=as.numeric(sq$total_sqft)
sq$total_sqft=sq$total_sqft*9

dim(bg)
cl=rbind(ac,ce,gn,gr,pe,sm,sq)


cl$total_sqft=as.factor(cl$total_sqft)
bg$total_sqft=sapply(strsplit(as.character(bg$total_sqft), " - ", fixed=T), function(x) mean(as.numeric(x)))
bg%>%filter(!is.na(bg$total_sqft))->bg
bg=rbind(bg,cl)
dim(bg)

unique(bg$total_sqft)

summary(bg$total_sqft)

bg%>%group_by(society)%>%summarize(count=n())%>%arrange(desc(count))
bg%>%filter(!is.na(society))%>%nrow()
bg=bg[,-5]

colSums(is.na(bg))
bb=bg[,3]
bg=bg[,-3]

library(mice)
md.pattern(bg)
mymice=mice(bg,m=5,method="pmm")
mymiceComplete=complete(mymice,2)
summary(mymiceComplete)
bg=mymiceComplete
summary(bg)
bg=cbind(bg,bb)
colnames(bg)[8]="location"

unique(bg$availability)

bg$availability=ifelse(bg$availability=="Ready To Move",1,0)
bg$location=as.character(bg$location)
bg%>%group_by(location)%>%summarize(count=n())%>%filter(count<=10)->ll

bg$other=ifelse(bg$location %in% (ll$location),"Other",0)
bg$location=ifelse(bg$other=="Other","Other",bg$location)
length(unique(bg$location))
bg=bg[,-9]


bg$price_per_sqft=(bg$price*100000)/as.numeric(bg$total_sqft)
str(bg)
bg$price_per_bhk=as.numeric(bg$total_sqft)/as.numeric(bg$size)


bg%>%ggplot(aes(price_per_bhk))+geom_histogram(bins=50,fill="blue",col="black")+scale_x_continuous(limits = c(0,3000))+ggtitle("Price per BHK")
bg%>%ggplot(aes(price_per_sqft))+geom_histogram(bins=50,fill="blue",col="black")+scale_x_continuous(limits = c(0,50000))+ggtitle("Price per Sqft")
bg%>%ggplot(aes(price))+geom_histogram(bins=50,fill="blue",col="black")+scale_x_continuous(limits = c(0,1000))+ggtitle("Price")
bg%>%ggplot(aes(as.numeric(total_sqft)))+geom_histogram(bins=50,fill="blue",col="black")+scale_x_continuous(limits = c(0,10000))+ggtitle("Total Sq.feet")
bg%>%ggplot(aes(as.numeric(price_per_bhk)))+geom_boxplot()+scale_x_continuous(limits = c(0,2000))+coord_flip()
bg%>%ggplot(aes(as.numeric(total_sqft)))+geom_boxplot()+scale_x_continuous(limits = c(0,4000))+coord_flip()
bg%>%ggplot(aes(price_per_sqft))+geom_boxplot()+scale_x_continuous(limits = c(0,40000))+coord_flip()


bg$total_sqft=as.numeric(bg$total_sqft)
summary(bg$total_sqft)

bg%>%filter(price_per_bhk > 300 & price_per_bhk < 1500 & total_sqft < 5000 & price_per_sqft <22000 )->bg

summary(bg)

bg1=bg[,c(3:7,9,10)]
bg1=lapply(bg1, function(x) as.numeric(x))
bg1=as.data.frame(bg1)
library(corrplot)
corrplot(cor(bg1),type="upper",method = "number")
bg=bg[,-c(3,6,10)]

library(caret)
dd=dummyVars(" ~ area_type", data = bg,fullRank = F)
d1=data.frame(predict(dd, newdata = bg))
bg=cbind(bg,d1)

dd=dummyVars(" ~ location", data = bg,fullRank = F)
d1=data.frame(predict(dd, newdata = bg))
bg=cbind(bg,d1)
bg=bg[,-c(1,6)]

bg1=bg[,c(2,3,5)]
bg1=lapply(bg1, function(x) as.numeric(x))
bg1=as.data.frame(bg1)
bg1=scale(bg1)
bg1=as.data.frame(bg1)
bg=bg[,-c(2,3,5)]
bg=cbind(bg,bg1)

library(caTools)
set.seed(500)
spl=sample.split(bg,SplitRatio = 0.8)
tr=subset(bg,spl==T)
ts=subset(bg,spl==F)

set.seed(152)
lm=lm(price~.,data=tr)
summary(lm)
par(mfrow=c(2,2))
plot(lm)

cd=cooks.distance(lm)
i=which(cd>4*mean(cd))
length(i)
tr1=tr[-i,]

set.seed(58)
lm=lm(price~.,data=tr1)
summary(lm)
par(mfrow=c(1,1))

library(lmtest)
bptest(lm)

bc=MASS::boxcox(lm)
best_lam=bc$x[which(bc$y==max(bc$y))]
best_lam

set.seed(211)
lm=lm((price)^0.46~.,data=tr1)
summary(lm)

ts$pred=predict(lm,newdata=ts)
library(MLmetrics)
RMSE(ts$price,ts$pred)

#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare

######Ridge
set.seed(200)
r1=train(price~.,method="glmnet",data=tr,trControl=trainControl(method="repeatedcv",number=10,repeats=5,verboseIter = T),tuneGrid=expand.grid(alpha=0,lambda=seq(-10,250,length=10)))
summary(r1)
plot(r1)
ts$pred=predict(r1,ts,type="raw")
RMSE(ts$pred,ts$price)


#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare

######Lasso
set.seed(258)
la=train(price~.,method="glmnet",data=tr,trControl=trainControl(method="repeatedcv",number=10,repeats=5,verboseIter = T),tuneGrid=expand.grid(alpha=1,lambda=seq(-10,250,length=10)))
plot(la)
ts$pred=predict(la,ts,type="raw")
RMSE(ts$pred,ts$price)


#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare

#######DT
library(rpart)
library(rpart.plot)
library(rattle)

set.seed(897)
r.ctrl=rpart.control(minisplit=10,minbucket=5,cp=0,xval=10)
dt=rpart(formula=tr$price~.,data=tr,control = r.ctrl)
plotcp(dt)
dt$cptable


r.ctrl=rpart.control(minisplit=50,minbucket=5,cp=0.00012,xval=10)
dt1=rpart(formula=tr$price~.,data=tr,control = r.ctrl)
#fancyRpartPlot(dt1)

ts$pred=predict(dt1,ts,type="vector")
RMSE(ts$pred,ts$price)

#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare

#####Random Forest
library(randomForest)
#set seed again for randomness
set.seed(1000)
#build first RF model
rf=randomForest(price~.,data=tr,ntree=400,mtry=49,nodesize=30,importance=T)
print(rf)


#tune rf to identify the best mtry
set.seed(109)
trrf=tuneRF(tr[,-c(1)],y=tr$price,mtryStart = 10,stepFactor = 1.5,ntree=400,improve = 0.0001,nodesize=10,
            trace=T,plot=T,doBest = T,importance=T)
summary(trrf)
trrf
rf1=randomForest(price~.,data=tr,ntree=500,mtry=244,nodesize=10,importance=T)
plot(rf1)
print(rf1)

ts$pred=predict(rf1,ts,type="response")
RMSE(ts$pred,ts$price)


#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare

##############Bagging
library(ipred)
library(rpart)
set.seed(127)
bg=bagging(price ~.,data=tr,control=rpart.control(maxdepth=5, minsplit=10))

ts$pred=predict(bg,ts,type="response")
RMSE(ts$pred,ts$price)


#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare
#####xgboost

library(caTools)
set.seed(5001)
spl=sample.split(bg,SplitRatio = 0.8)
tr=subset(bg,spl==T)
ts=subset(bg,spl==F)
set.seed(1233)
library(xgboost)
param_list = list(objective = "reg:linear", eta=0.03,gamma = 2, max_depth=10,subsample=0.8,colsample_bytree=0.5)
xg = xgb.DMatrix(data = as.matrix(tr[,-c(1)]), label= tr$price) 
te = xgb.DMatrix(data = as.matrix(ts[,-c(1)]))
set.seed(112) 
xgbcv = xgb.cv(params = param_list,data = xg,nrounds = 500,nfold = 10,print_every_n = 10,early_stopping_rounds = 10,maximize = F)
xgb_m = xgb.train(data = xg, params = param_list, nrounds = 265)
ts$pred=predict(xgb_m,te,type="vector")
RMSE(ts$pred,ts$price)

#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$price,ts$pred)
rsquare

######
library(caTools)
set.seed(5001)
spl=sample.split(bg,SplitRatio = 0.8)
tr=subset(bg,spl==T)
ts=subset(bg,spl==F)
set.seed(1233)
library(xgboost)

gd_features_train<-as.matrix(tr[,-1])
gd_label_train<-as.matrix(tr[,1])
gd_features_test<-as.matrix(ts[,-1])
tp_xgb<-vector()
lr=c(0.01,0.1,0.3,0.5,0.7,1)
md=c(1,3,5,9,7,15)
nr=c(50,100,500,1000)
mc=c(1,3,7,9,10,15)
gm=c(1,2,3,4,6,7,8,9,10)

for (i in mc){
xgb.fit <- xgboost(
  data = gd_features_train,
  label = gd_label_train,
  eta = 0.01,
  max_depth =7,
  min_child_weight = 1,
  nrounds = 500,
  nfold = 10,
  objective = "reg:linear", 
  verbose = 0,               
  early_stopping_rounds = 10,
  gamma=3
)
ts$pred=predict(xgb.fit, gd_features_test)
tp_xgb=cbind(tp_xgb,(rsq(ts$price,ts$pred)))
}
tp_xgb

q()
