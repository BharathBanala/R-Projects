setwd("C:\\Users\\Sharath Reddy Banala\\Documents\\R")

#Read Files 
bank_train=read.csv("bank_full_train.csv",stringsAsFactors = F)
bank_test=read.csv("bank_full_test.csv",stringsAsFactors = F)

glimpse(bank_train)

#combining Test and Train files

bank_test$y= NA
bank_train$data= 'train'
bank_test$data= 'test'
bank=rbind(bank_train,bank_test)
glimpse(bank)


# For Referance Column names
colnames(bank)
#"age"       "job"       "marital"   "education" "default"  
#[6] "balance"   "housing"   "loan"      "contact"   "day"      
#[11] "month"     "duration"  "campaign"  "pdays"     "previous" 
#[16] "poutcome"  "ID"        "y"         "data"

table(bank$poutcome)
bank <- bank[,- c(10,11,14,16)]


aa=c("job","marital","education","default" ,"housing", "loan","contact")


for (i in aa){
  
  a=table(bank[,i])
  print(a)
}


bank$education[bank$education=='unknown'] <- 'primary'
bank$job[bank$job=='unknown'] <- 'unemployed'
bank$contact[bank$contact=='unknown'] <- 'telephone'
       
bank <- bank[,-9] # Droping ID

Avg_Duration <- mean(bank$duration)

bank <- bank %>% 
  mutate(duration = ifelse (duration > Avg_Duration ,'Above_Avg','Below_Avg') )



# Converting all Yes and No to binary Elements

#ss=c("default" ,"housing", "loan","y")

#bank <- bank %>%
#  mutate(default= ifelse(default=='no',0,1)) %>% 
#  mutate(housing= ifelse(housing=='no',0,1)) %>% 
#  mutate(loan= ifelse(loan=='no',0,1)) %>% 
#  mutate(y= ifelse(y=='no',0,1))



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

bank=CreateDummies(bank,'job',1500)
bank=CreateDummies(bank,'marital',1500)
bank=CreateDummies(bank,'education',1500)
bank=CreateDummies(bank,'contact',1500)
bank=CreateDummies(bank,'duration',1500)
bank_train = bank %>% filter(data =='train') %>% select(-data)
bank_test = bank %>% filter(data =='test') %>% select(-data,-y)

set.seed(5)
s=sample(1:nrow(bank_train),0.80*nrow(bank_train))
bank_train1=bank_train[s,]
bank_train2=bank_train[-s,]


log.fit=glm(y~.,data=bank_train1,family = "binomial")
sort(vif(log.fit),decreasing = T)

log.fit=step(log.fit)

formula(log.fit)
log.fit=glm(y ~ balance + housing + loan + campaign + ID + job_retired + 
              job_services + job_admin. + job_blue_collar + marital_married + 
              education_tertiary + duration_Below_Avg,
            data=bank_train1,family='binomial')
summary(log.fit)
train.score=predict(log.fit,newdata = bank_train1,type='response')
train.auc=auc(roc(bank_train1$y,train.score))
train.auc
val.score=predict(log.fit,newdata = bank_train2,type='response')
val.auc=auc(roc(bank_train2$y,val.score))
val.auc

test.score=predict(log.fit,newdata = bank_test,type='response')

write.csv(test.score,'Bharath_Banala_P5_part2.csv',row.names = F)

