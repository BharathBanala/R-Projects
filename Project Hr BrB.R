setwd("C:\\Users\\Sharath Reddy Banala\\Documents\\R")

#Read Files 
hr_train=read.csv("hr_train.csv",stringsAsFactors = F)
hr_test=read.csv("hr_test.csv",stringsAsFactors = F)

unique(hr_test$time_spend_company)


s_train=filter(hr_train,salary!='high')
t.test(promotion_last_5years~left,data=s_train)
t.test(time_spend_company~promotion_last_5years,s_train)
sum(hr_train$left==0)
sum(hr_train$left==1)
hr_train$average_montly_hours

ggplot(hr_train,aes(satisfaction_level,average_montly_hours,color=left))+geom_point()

t.test(Work_accident~left,data=s_train)
t.test(Work_accident~promotion_last_5years,data=s_train)
t.test(number_project~promotion_last_5years,data=s_train)
t.test(average_montly_hours~left,data=s_train)



hr_test=read.csv("hr_test.csv",stringsAsFactors = F)
hr_test$left = NA
hr_train$data='train'
hr_test$data='test'
hr_all = rbind(hr_train,hr_test)
glimpse(hr_all)
lapply(hr_all,function(x) sum(is.na(x)))

hr_all$left = as.factor(hr_all$left)
hr_all$average_montly_hours = scale(hr_all$average_montly_hours)



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
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
hr_all=CreateDummies(hr_all,'sales',50)
hr_all=CreateDummies(hr_all,"time_spend_company",50)
hr_all=CreateDummies(hr_all,'salary',50)
hr_train = hr_all %>% filter(data =='train') %>% select(-data)
hr_test = hr_all %>% filter(data =='test') %>% select(-data,-left)

set.seed(2)
s=sample(1:nrow(hr_train),0.80*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

log.fit=glm(left~.-time_spend_company_3,data=hr_train1,family = "binomial")
sort(vif(log.fit),decreasing = T)

log.fit=step(log.fit)

formula(log.fit)
log.fit=glm(left ~ satisfaction_level + number_project + 
              average_montly_hours + Work_accident + sales_RandD + sales_IT + 
              time_spend_company_7 + time_spend_company_10 + time_spend_company_6 + 
              time_spend_company_5 + time_spend_company_4 + time_spend_company_2 + 
              salary_medium + salary_low,
            data=hr_train1,family='binomial')


summary(log.fit)
train.score=predict(log.fit,newdata = hr_train1,type='response')
train.auc=auc(roc(hr_train1$left,train.score))
train.auc
val.score=predict(log.fit,newdata = hr_train2,type='response')
val.auc=auc(roc(hr_train2$left,val.score))
val.auc

##Randomforest

param=list(mtry=c(5,10,15,20,25),
           ntree=c(100,300,500,700,800),
           maxnodes=c(5,10,20,30,40,50,100),
           nodesize=c(1,7,15,25,35,40))
expand.grid(param)

## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=30
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~., 
             data =hr_train1,
             tuning =params,
             folds = cvFolds(nrow(hr_train1), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}


myauc= 0.8400817

best_params=data.frame(mtry=10,
                       ntree=100,
                       maxnodes=100,
                       nodesize=7)

hr.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=hr_train
)

test.score=predict(hr.rf.final,newdata = hr_test,type='prob')[,2]

write.csv(test.score,'Bharath_Banala_P4_part2.csv',row.names = F)   


## Variable IMportance

d=importance(hr.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))