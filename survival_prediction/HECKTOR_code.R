#######HECTOR######
library(survival)
library(parallelMap)
library(tidyverse)
library(mboost)
library(mlr)
library(readxl)
library(mRMRe)
#Read train and test data
train_df=as.date.frame(read_xlsx("path/to/file"))
validate_df=as.date.frame(read_xlsx("path/to/file"))
####Featrue Slection
Data=train_df
surv_obj=Surv(Data$time,Data$censor)
Data$censor = NULL
Data$time = NULL
Data=data.frame(surv_obj,Data)

MI_matrix=mim(mRMR.data(data=Data))
Mu_Info=unname(MI_matrix[1,])
Variables = colnames(Data)
Var_Ranking = arrange(data.frame(Variables,Mu_Info),desc(Mu_Info))
Var_Ranking=Var_Ranking[1:10,]#" that means you select 10 features. you can change this value to each number you want.
Selected_Vars = Data %>% dplyr::select(as.character(Var_Ranking[c(1:10),]$Variables))
mi.fs=colnames(Selected_Vars)
vec.fs=list()
vec.fs[[1]]=mi.fs[1]
vec.fs[[2]]=mi.fs[1:2]
vec.fs[[3]]=mi.fs[1:3]
vec.fs[[4]]=mi.fs[1:4]
vec.fs[[5]]=mi.fs[1:5]
vec.fs[[6]]=mi.fs[1:6]
vec.fs[[7]]=mi.fs[1:7]
vec.fs[[8]]=mi.fs[1:8]
vec.fs[[9]]=mi.fs[1:9]
vec.fs[[10]]=mi.fs
fs_vec=c(paste0("MI_",1:10))
names(vec.fs)=fs_vec
###For loop across feature selection method
# and extact hazrard ratio (HR) for each model
# we also save rds file for cross validation result.
HR=rownames(validate_df)
for (fs in 2:length(vec.fs)) {
  print(fs_vec[[fs]])
  train_dfr=train_df[,vec.fs[[fs]]]
  train_dfr=cbind(train_df[,(length(train_df)-1):length(train_df)],train_dfr)
  validate_dfr=validate_df[,vec.fs[[fs]]]
  surv.task=makeSurvTask(id="HN",data=train_dfr,target=c("time","censor"))
  #########Tune models########################
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 5L,stratify = T)
  #parallelStartSocket(4)
  psgb = makeParamSet(makeDiscreteParam("mstop", values = c(50,70,100,150,200,250,350,400,450,500)))
  hp_gb = tuneParams("surv.glmboost", surv.task, rdesc, par.set = psgb, control = ctrl)
  gb_lrn = setHyperPars(makeLearner("surv.glmboost"), mstop = hp_gb$x$mstop)
  psgn=makeParamSet(
    makeNumericParam("s", lower = 0.001, upper = 0.1),
    makeNumericParam("alpha", lower = 0.0, upper = 1.0))
  hp_gn = tuneParams("surv.glmnet", surv.task, rdesc, par.set = psgn, control = ctrl)
  gn_lrn = setHyperPars(makeLearner("surv.glmnet"), s = hp_gn$x$s,alpha=hp_gn$x$alpha)
  glmnet_train=train(gn_lrn,surv.task)
    svlearners=list(
    makeLearner("surv.coxph"),
    gb_lrn,
    gn_lrn)
  
  rdesc_train=makeResampleDesc("CV",iters=5,stratify = T)
  # # Initalising the Benchmark study in train data with 5F CV
  set.seed(123)
  svbnmrk=benchmark(svlearners,surv.task,rdesc_train)
  saveRDS(svbnmrk,paste0(fs_vec[fs],".rds"))
  #####Prediction Score for external test
  Coxph_train=train("surv.coxph",surv.task)
  Coxph_test= predict(Coxph_train,  newdata = validate_dfr)
  Coxph_score=Coxph_test$data
  glmboost_train=train(gb_lrn,surv.task)
  glmboost_test= predict(glmboost_train,  newdata = validate_dfr)
  glmboost_score=glmboost_test$data
  glmnet_test= predict(glmnet_train,  newdata = validate_dfr)
  glmnet_score=glmnet_test$data
  predction_score=cbind(Coxph_score,glmboost_score,glmnet_score)
  colnames(predction_score)=paste0(fs_vec[fs],c("_Coxph","_glmboost","_glmnet"))
  HR=cbind(HR,predction_score)
}
write.csv(HR,"Predction_Score.csv")