library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

rd_train=read.csv("C:/Users/user/Downloads/store_train.csv",stringsAsFactors = F)

rd_test= read.csv("C:/Users/user/Downloads/store_test.csv",stringsAsFactors = F)


rd_test$store=NA
str(rd_train)

rd_train$data='train'
rd_test$data='test'
rd_all=rbind(rd_train,rd_test)


lapply(rd_all,function(x) sum(is.na(x)))

rd_all$countyname=NULL
rd_all$countytownname=NULL

rd_all=rd_all[!(is.na(rd_all$CouSub)),]

for(col in names(rd_all)){
  
  if(sum(is.na(rd_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    rd_all[is.na(rd_all[,col]),col]=mean(rd_all[,col],na.rm=T)
  }
  
}

View(rd_all)

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

glimpse(rd_all)


rd_all=CreateDummies(rd_all,"Areaname",500)
rd_all=CreateDummies(rd_all ,"storecode",500)
rd_all=CreateDummies(rd_all ,"state_alpha",500)
rd_all=CreateDummies(rd_all ,"store_Type",500)



rd_all$store=as.levels(rd_all$store)

rd_train=rd_all %>% filter(data=='train') %>% select(-data)
rd_test=rd_all %>% filter(data=='test') %>% select(-data,-store)

##

set.seed(2)
s=sample(1:nrow(ld_train),0.8*nrow(rd_train))
rd_train1=rd_train[s,]
rd_train2=rd_train[-s,]

rd_all.tree=tree(store~.,data=rd_train1)

val.score=predict(rd_all.tree,newdata = rd_train2,type='vector')[,2]
pROC::roc(rd_train2$store,val.score)$auc

## Tree in text format

rd_all.tree

## Visual Format

plot(rd_all.tree)
text(rd_all.tree)

set.seed(2)
s=sample(1:nrow(rd_train),0.8*nrow(rd_train))
rd_train1=rd_train[s,]
rd_train2=rd_train[-s,]



params=list(mtry=c(5,10),ntree=c(100,500),
            maxnodes=c(15,20),nodesize=(c(2,5)))

expand.grid(params)

## paramter values that we want to try out

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=30
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
  
  k=cvTuning(randomForest,store~.,
             data =rd_train,
             tuning =params,
             folds = cvFolds(nrow(rd_train), K=10, type = "random"),
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

## from another run following values were obtained

#myerror=1.870957
best_params=data.frame(mtry=20,
                       ntree=200,
                       maxnodes=50,
                       nodesize=10)

## Final model with obtained best parameters

ld.rf.final=randomForest(store~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=rd_train)

test.pred=predict(ld.rf.final,newdata = rd_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)

ld.rf.final
