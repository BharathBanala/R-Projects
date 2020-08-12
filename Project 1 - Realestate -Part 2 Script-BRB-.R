setwd("C://Users//Sharath Reddy Banala//Documents//r")


getwd()

housing_train=read.csv("housing_train.csv",stringsAsFactors = F)
boxplot(housing_train$Bathroom)
library(tidyverse)

housing_test= read.csv("housing_test.csv",stringsAsFactors = F)
housing_test$Price=NA
housing_train$data='train'
housing_test$data='test'
housing=rbind(housing_train,housing_test)
housing$Address=NULL
glimpse(housing)

ggplot(housing,aes(YearBuilt,Price))+geom_point()


housing=housing %>% 
  select(-BuildingArea,-CouncilArea,-YearBuilt)
sum(is.na(housing$Landsize))
colnames(housing)


#NA values
lapply(housing,function(x) sum(is.na(x)))


housing$Bedroom2= mean(housing$Bedroom2,na.rm = T)
LandSizeMean = round(mean(housing$Landsize, na.rm = T))
BathroomMean = round(mean(housing$Bathroom, na.rm = T))
CarMean = round(mean(housing$Car, na.rm = T))
housing$Landsize <- ifelse(is.na(housing$Landsize),LandSizeMean,housing$Landsize)
housing$Bathroom <- ifelse(is.na(housing$Bathroom),BathroomMean,housing$Bathroom)
housing$Car <- ifelse(is.na(housing$Car),CarMean,housing$Car)



#NA values
lapply(housing,function(x) sum(is.na(x)))

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}




housing=CreateDummies(housing ,"Type",100)
housing=CreateDummies(housing,"Method",100)
glimpse(housing)

mean(sort(table(housing$SellerG)))
mean(sort(table(housing$Suburb)))

summary(housing)



housing=CreateDummies(housing,"Suburb",70)
housing=CreateDummies(housing,"SellerG",50)

housing=housing %>% 
  select(-Postcode)
#NA values
lapply(housing,function(x) sum(is.na(x)))



#Feature scaling
housing$Landsize = scale(housing$Landsize)
housing$Rooms = scale(housing$Rooms)
housing$Bedroom2 = scale(housing$Bedroom2)
housing$Bathroom = scale(housing$Bathroom)
housing$Car = scale(housing$Car)
housing$Distance = scale(housing$Distance)

housing_train=housing %>% 
  filter(data=='train') %>% 
  select(-data)
housing_test=housing %>% 
  filter(data=='test') %>% 
  select(-data,-Price)

set.seed(5)
s=sample(1:nrow(housing_train),0.7*nrow(housing_train))
housing_train1=housing_train[s,]
housing_train2=housing_train[-s,]

glimpse(housing_train1)
#Model
fit=lm(Price~.,data=housing_train1)
summary(fit)
sort(vif(fit),decreasing = T)

fit=lm(Price~. ,data=housing_train1)

sort(vif(fit),decreasing = T)
summary(fit)
# P values cuttoff
fit=step(fit)

summary(fit)
formula(fit)

fit=lm(Price ~ Rooms + Distance + Bathroom + Car + Landsize + Type_u + 
         Type_h + Method_SP + Method_S + Suburb_Armadale + Suburb_Williamstown + 
         Suburb_Melbourne + Suburb_SunshineWest  + 
         Suburb_BrunswickWest + Suburb_KeilorEast + Suburb_HawthornEast + 
         Suburb_SurreyHills + Suburb_Kensington + Suburb_Toorak + 
         Suburb_Elwood + Suburb_Maribyrnong + Suburb_Newport + Suburb_Doncaster + 
         Suburb_AscotVale + Suburb_Footscray + Suburb_Hampton + 
         Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
         Suburb_Carnegie + Suburb_Bentleigh + Suburb_PascoeVale + 
         Suburb_BrightonEast + Suburb_BalwynNorth + Suburb_Coburg + 
         Suburb_Kew + Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + 
         Suburb_Brunswick + Suburb_SouthYarra + 
         Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
         Suburb_Reservoir + SellerG_Douglas+ 
         SellerG_Kay + SellerG_McGrath  + SellerG_Gary + 
         SellerG_Jas + SellerG_Sweeney + SellerG_RT + SellerG_Fletchers + 
           SellerG_Buxton + SellerG_Marshall + 
          + SellerG_Jellis ,data=housing_train1)

summary(fit)

### Predict
val.pred=predict(fit,newdata=housing_train2)
val.pred
errors=housing_train2$Price-val.pred
RMSE=errors**2 %>% mean() %>% sqrt()
RMSE
Score=212467/RMSE
Score
## on entire data

## Decision tree regression method

housing.tree=tree(Price~.,data=housing_train1)

## Tree in text format

housing.tree

## Visual Format

plot(housing.tree)
text(housing.tree)

## Performance on validation set

val.IR=predict(housing.tree,newdata = housing_train2)

rmse_val=((val.IR)-(housing_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val
Score=212467/rmse_val
Score
val.IR1=predict(housing.tree,newdata = housing_train1)
rmse_val1=((val.IR1)-(housing_train1$Price))^2 %>% mean() %>% sqrt()
rmse_val1
Score=212467/rmse_val1
Score


## Regression Random Forest with Parameter Tuning

## function for getting all possible combinations : expand.grid
## random forest
param=list(mtry=c(5,15,60,100,115),
           ntree=c(50,100,500,700,800),
           maxnodes=c(5,20,50,100,200),
           nodesize=c(1,27,42,50,100))
5*5*5*5


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=15){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=15
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =housing_train1,
             tuning =params,
             folds = cvFolds(nrow(housing_train1), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}


myerror=384048.2
scor=212467/myerror
scor
best_params=data.frame(mtry=60,
                       ntree=700,
                       maxnodes=100,
                       nodesize=50)

ho.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=housing_train)

test.pred=predict(ho.rf.final,newdata = housing_test)
write.csv(test.pred,"Bharath_Banala_P1_Part2.csv",row.names = F)
