library(gdata)
library(ggplot2)
library(MASS)
library(ggplot2)
library("ggthemes")
library("GGally")
library("extracat")
library(hdrcde)
library(KernSmooth)
library("ggplot2")
library("gridExtra")
library("vcd")
library(class)
library(sqldf)
library(cccrm)
set.seed(40001)
df = read.xls ("data1.xlsx", sheet = 1, header = TRUE,na.strings=c("NA","<NA>","","*","unclear"))
head(df)
df[,6]
df[,6]<-as.numeric(as.character(df[,6]))
data_2<-df
data_2<-df[complete.cases(df), ]
df<-df[complete.cases(df), ]
colnames(data_2)<-c('C1','C2','C3','C4','EC1','EC2','EC3')

##
head(data_2)
data_2[,1][df[,1]<100]<-0
data_2[,2][df[,2]==0]<-0
data_2[,3][df[,3]<40]<-0
data_2[,4][df[,4]<1.6]<-0
data_2[,5][df[,5]<2.0]<-0
data_2[,6][df[,6]<2.0]<-0
data_2[,7][df[,7]<1.8]<-0
##
data_2[,1][df[,1]>=100]<-1
data_2[,2][df[,2]!=0]<-1
data_2[,3][df[,3]>=40]<-1
data_2[,4][df[,4]>=1.6]<-1
data_2[,5][df[,5]>=2.0]<-1
data_2[,6][df[,6]>=2.0]<-1
data_2[,7][df[,7]>=1.8]<-1
##
C1<-data_2[,1]
C2<-data_2[,2]
C3<-data_2[,3]
C4<-data_2[,4]
EC1<-data_2[,5]
EC2<-data_2[,6]
EC3<-data_2[,7]
#
library(fmsb)
CEC11<-Kappa.test(C1,EC1,conf.level = 0.90)
CEC21<-Kappa.test(C2,EC1,conf.level = 0.90)
CEC31<-Kappa.test(C3,EC1,conf.level = 0.90)
CEC41<-Kappa.test(C4,EC1,conf.level = 0.90)
CEC12<-Kappa.test(C1,EC2,conf.level = 0.90)
CEC22<-Kappa.test(C2,EC2,conf.level = 0.90)
CEC32<-Kappa.test(C3,EC2,conf.level = 0.90)
CEC42<-Kappa.test(C4,EC2,conf.level = 0.90)
CEC13<-Kappa.test(C1,EC3,conf.level = 0.90)
CEC23<-Kappa.test(C2,EC3,conf.level = 0.90)
CEC33<-Kappa.test(C3,EC3,conf.level = 0.90)
CEC43<-Kappa.test(C4,EC3,conf.level = 0.90)
Kappa_values<-c(CEC11$Result$estimate,CEC21$Result$estimate,CEC31$Result$estimate,CEC41$Result$estimate,
                CEC12$Result$estimate,CEC22$Result$estimate,CEC32$Result$estimate,CEC42$Result$estimate,
                CEC13$Result$estimate,CEC23$Result$estimate,CEC33$Result$estimate,CEC43$Result$estimate)
pvalues<-c(CEC11$Result$p.value,CEC21$Result$p.value,CEC31$Result$p.value,CEC41$Result$p.value,
           CEC12$Result$p.value,CEC22$Result$p.value,CEC32$Result$p.value,CEC42$Result$p.value,
           CEC13$Result$p.value,CEC23$Result$p.value,CEC33$Result$p.value,CEC43$Result$p.value)
Judgements<-c(CEC11$Judgement,CEC21$Judgement,CEC31$Judgement,CEC41$Judgement,
              CEC12$Judgement,CEC22$Judgement,CEC32$Judgement,CEC42$Judgement,
              CEC13$Judgement,CEC23$Judgement,CEC33$Judgement,CEC43$Judgement)        
results_cp<-cbind(Kappa_values,pvalues,Judgements)
rownames(results_cp)<-c('C1&EC1','C2&EC1','C3&EC1','C4&EC1',
                        'C1&EC2','C2&EC2','C3&EC2','C4&EC2',
                        'C1&EC3','C2&EC3','C3&EC3','C4&EC3')
results_cp

#
C1_C2_EC1<-Kappa.test(C1*C2,EC1,conf.level = 0.90)
C1_C3_EC1<-Kappa.test(C1*C3,EC1,conf.level = 0.90)
C1_C4_EC1<-Kappa.test(C1*C4,EC1,conf.level = 0.90)
C2_C3_EC1<-Kappa.test(C2*C3,EC1,conf.level = 0.90)
C2_C4_EC1<-Kappa.test(C2*C4,EC1,conf.level = 0.90)
C3_C4_EC1<-Kappa.test(C3*C4,EC1,conf.level = 0.90)
#
C1_C2_EC2<-Kappa.test(C1*C2,EC2,conf.level = 0.90)
C1_C3_EC2<-Kappa.test(C1*C3,EC2,conf.level = 0.90)
C1_C4_EC2<-Kappa.test(C1*C4,EC2,conf.level = 0.90)
C2_C3_EC2<-Kappa.test(C2*C3,EC2,conf.level = 0.90)
C2_C4_EC2<-Kappa.test(C2*C4,EC2,conf.level = 0.90)
C3_C4_EC2<-Kappa.test(C3*C4,EC2,conf.level = 0.90)
#
C1_C2_EC3<-Kappa.test(C1*C2,EC3,conf.level = 0.90)
C1_C3_EC3<-Kappa.test(C1*C3,EC3,conf.level = 0.90)
C1_C4_EC3<-Kappa.test(C1*C4,EC3,conf.level = 0.90)
C2_C3_EC3<-Kappa.test(C2*C3,EC3,conf.level = 0.90)
C2_C4_EC3<-Kappa.test(C2*C4,EC3,conf.level = 0.90)
C3_C4_EC3<-Kappa.test(C3*C4,EC3,conf.level = 0.90)

#
Kappa_values.2<-c(C1_C2_EC1$Result$estimate,C1_C3_EC1$Result$estimate,C1_C4_EC1$Result$estimate,C2_C3_EC1$Result$estimate,C2_C4_EC1$Result$estimate,C3_C4_EC1$Result$estimate,
                  C1_C2_EC2$Result$estimate,C1_C3_EC2$Result$estimate,C1_C4_EC2$Result$estimate,C2_C3_EC2$Result$estimate,C2_C4_EC2$Result$estimate,C3_C4_EC2$Result$estimate,
                  C1_C2_EC3$Result$estimate,C1_C3_EC3$Result$estimate,C1_C4_EC3$Result$estimate,C2_C3_EC3$Result$estimate,C2_C4_EC3$Result$estimate,C3_C4_EC3$Result$estimate)
pvalues.2<-c(C1_C2_EC1$Result$p.value,C1_C3_EC1$Result$p.value,C1_C4_EC1$Result$p.value,C2_C3_EC1$Result$p.value,C2_C4_EC1$Result$p.value,C3_C4_EC1$Result$p.value,
             C1_C2_EC2$Result$p.value,C1_C3_EC2$Result$p.value,C1_C4_EC2$Result$p.value,C2_C3_EC2$Result$p.value,C2_C4_EC2$Result$p.value,C3_C4_EC2$Result$p.value,
             C1_C2_EC3$Result$p.value,C1_C3_EC3$Result$p.value,C1_C4_EC3$Result$p.value,C2_C3_EC3$Result$p.value,C2_C4_EC3$Result$p.value,C3_C4_EC3$Result$p.value)
Judgements.2<-c(C1_C2_EC1$Judgement,C1_C3_EC1$Judgement,C1_C4_EC1$Judgement,C2_C3_EC1$Judgement,C2_C4_EC1$Judgement,C3_C4_EC1$Judgement,
                C1_C2_EC2$Judgement,C1_C3_EC2$Judgement,C1_C4_EC2$Judgement,C2_C3_EC2$Judgement,C2_C4_EC2$Judgement,C3_C4_EC2$Judgement,
                C1_C2_EC3$Judgement,C1_C3_EC3$Judgement,C1_C4_EC3$Judgement,C2_C3_EC3$Judgement,C2_C4_EC3$Judgement,C3_C4_EC3$Judgement)        
results_cp.2<-cbind(Kappa_values.2,pvalues.2,Judgements.2)
rownames(results_cp.2)<-c('C1_C2&EC1','C1_C3&EC1','C1_C4&EC1','C2_C3&EC1','C2_C4&EC1','C3_C4&EC1',
                          'C1_C2&EC2','C1_C3&EC2','C1_C4&EC2','C2_C3&EC2','C2_C4&EC2','C3_C4&EC2',
                          'C1_C2&EC3','C1_C3&EC3','C1_C4&EC3','C2_C3&EC3','C2_C4&EC3','C3_C4&EC3')
#write.table(results_cp.2, "/Users/hemu/Desktop/re_2_results18.txt", sep="\t")
##3 combinations 

C1_C2_C3_EC1<-Kappa.test(C1*C2*C3,EC1,conf.level = 0.90)
C1_C2_C4_EC1<-Kappa.test(C1*C2*C4,EC1,conf.level = 0.90)
C1_C3_C4_EC1<-Kappa.test(C1*C3*C4,EC1,conf.level = 0.90)
C2_C3_C4_EC1<-Kappa.test(C2*C3*C4,EC1,conf.level = 0.90)
C1_C2_C3_EC2<-Kappa.test(C1*C2*C3,EC2,conf.level = 0.90)
C1_C2_C4_EC2<-Kappa.test(C1*C2*C4,EC2,conf.level = 0.90)
C1_C3_C4_EC2<-Kappa.test(C1*C3*C4,EC2,conf.level = 0.90)
C2_C3_C4_EC2<-Kappa.test(C2*C3*C4,EC2,conf.level = 0.90)
C1_C2_C3_EC3<-Kappa.test(C1*C2*C3,EC3,conf.level = 0.90)
C1_C2_C4_EC3<-Kappa.test(C1*C2*C4,EC3,conf.level = 0.90)
C1_C3_C4_EC3<-Kappa.test(C1*C3*C4,EC3,conf.level = 0.90)
C2_C3_C4_EC3<-Kappa.test(C2*C3*C4,EC3,conf.level = 0.90)
Kappa_values.3<-c(C1_C2_C3_EC1$Result$estimate,C1_C2_C4_EC1$Result$estimate,C1_C3_C4_EC1$Result$estimate,C2_C3_C4_EC1$Result$estimate,
                  C1_C2_C3_EC2$Result$estimate,C1_C2_C4_EC2$Result$estimate,C1_C3_C4_EC2$Result$estimate,C2_C3_C4_EC2$Result$estimate,
                  C1_C2_C3_EC3$Result$estimate,C1_C2_C4_EC3$Result$estimate,C1_C3_C4_EC3$Result$estimate,C2_C3_C4_EC3$Result$estimate)
pvalues.3<-c(C1_C2_C3_EC1$Result$p.value,C1_C2_C4_EC1$Result$p.value,C1_C3_C4_EC1$Result$p.value,C2_C3_C4_EC1$Result$p.value,
             C1_C2_C3_EC2$Result$p.value,C1_C2_C4_EC2$Result$p.value,C1_C3_C4_EC2$Result$p.value,C2_C3_C4_EC2$Result$p.value,
             C1_C2_C3_EC3$Result$p.value,C1_C2_C4_EC3$Result$p.value,C1_C3_C4_EC3$Result$p.value,C2_C3_C4_EC3$Result$p.value)
Judgements.3<-c(C1_C2_C3_EC1$Judgement,C1_C2_C4_EC1$Judgement,C1_C3_C4_EC1$Judgement,C2_C3_C4_EC1$Judgement,
                C1_C2_C3_EC2$Judgement,C1_C2_C4_EC2$Judgement,C1_C3_C4_EC2$Judgement,C2_C3_C4_EC2$Judgement,
                C1_C2_C3_EC3$Judgement,C1_C2_C4_EC3$Judgement,C1_C3_C4_EC3$Judgement,C2_C3_C4_EC3$Judgement)
results_cp.3<-cbind(Kappa_values.3,pvalues.3,Judgements.3)
rownames(results_cp.3)<-c('C1_C2_C3&EC1','C1_C2_C4&EC1','C1_C3_C4&EC1','C2_C3_C4&EC1','C1_C2_C3&EC2','C1_C2_C4&EC2','C1_C3_C4&EC2','C2_C3_C4&EC2','C1_C2_C3&EC3','C1_C2_C4&EC3','C1_C3_C4&EC3','C2_C3_C4&EC3')
##

C1_EC1_EC2<-Kappa.test(C1,EC1*EC2,conf.level = 0.90)
C1_EC1_EC3<-Kappa.test(C1,EC1*EC3,conf.level = 0.90)
C1_EC2_EC3<-Kappa.test(C1,EC2*EC3,conf.level = 0.90)
C2_EC1_EC2<-Kappa.test(C2,EC1*EC2,conf.level = 0.90)
C2_EC1_EC3<-Kappa.test(C2,EC1*EC3,conf.level = 0.90)
C2_EC2_EC3<-Kappa.test(C2,EC2*EC3,conf.level = 0.90)
C3_EC1_EC2<-Kappa.test(C3,EC1*EC2,conf.level = 0.90)
C3_EC1_EC3<-Kappa.test(C3,EC1*EC3,conf.level = 0.90)
C3_EC2_EC3<-Kappa.test(C3,EC2*EC3,conf.level = 0.90)
C4_EC1_EC2<-Kappa.test(C4,EC1*EC2,conf.level = 0.90)
C4_EC1_EC3<-Kappa.test(C4,EC1*EC3,conf.level = 0.90)
C4_EC2_EC3<-Kappa.test(C4,EC2*EC3,conf.level = 0.90)
Kappa_values.4<-c(C1_EC1_EC2$Result$estimate,C1_EC1_EC3$Result$estimate,C1_EC2_EC3$Result$estimate,
                  C2_EC1_EC2$Result$estimate,C2_EC1_EC3$Result$estimate,C2_EC2_EC3$Result$estimate,
                  C3_EC1_EC2$Result$estimate,C3_EC1_EC3$Result$estimate,C3_EC2_EC3$Result$estimate,
                  C4_EC1_EC2$Result$estimate,C4_EC1_EC3$Result$estimate,C4_EC2_EC3$Result$estimate)
pvalues.4<-c(C1_EC1_EC2$Result$p.value,C1_EC1_EC3$Result$p.value,C1_EC2_EC3$Result$p.value,
             C2_EC1_EC2$Result$p.value,C2_EC1_EC3$Result$p.value,C2_EC2_EC3$Result$p.value,
             C3_EC1_EC2$Result$p.value,C3_EC1_EC3$Result$p.value,C3_EC2_EC3$Result$p.value,
             C4_EC1_EC2$Result$p.value,C4_EC1_EC3$Result$p.value,C4_EC2_EC3$Result$p.value)
Judgements.4<-c(C1_EC1_EC2$Judgement,C1_EC1_EC3$Judgement,C1_EC2_EC3$Judgement,
                C2_EC1_EC2$Judgement,C2_EC1_EC3$Judgement,C2_EC2_EC3$Judgement,
                C3_EC1_EC2$Judgement,C3_EC1_EC3$Judgement,C3_EC2_EC3$Judgement,
                C4_EC1_EC2$Judgement,C4_EC1_EC3$Judgement,C4_EC2_EC3$Judgement)
results_cp.4<-cbind(Kappa_values.4,pvalues.4,Judgements.4)
rownames(results_cp.4)<-c('C1&EC1_EC2','C1&EC1_EC3','C1&EC2_EC3',
                          'C2&EC1_EC2','C2&EC1_EC3','C2&EC2_EC3',
                          'C3&EC1_EC2','C3&EC1_EC3','C3&EC2_EC3',
                          'C4&EC1_EC2','C4&EC1_EC3','C4&EC2_EC3')
##
C1_EC1_EC2_EC3<-Kappa.test(C1,EC1*EC2*EC3,conf.level = 0.90)
C2_EC1_EC2_EC3<-Kappa.test(C2,EC1*EC2*EC3,conf.level = 0.90)
C3_EC1_EC2_EC3<-Kappa.test(C3,EC1*EC2*EC3,conf.level = 0.90)
C4_EC1_EC2_EC3<-Kappa.test(C4,EC1*EC2*EC3,conf.level = 0.90)
Kappa_values.5<-c(C1_EC1_EC2_EC3$Result$estimate,C2_EC1_EC2_EC3$Result$estimate,
                  C3_EC1_EC2_EC3$Result$estimate,C4_EC1_EC2_EC3$Result$estimate)
pvalues.5<-c(C1_EC1_EC2_EC3$Result$p.value,C2_EC1_EC2_EC3$Result$p.value,
             C3_EC1_EC2_EC3$Result$p.value,C4_EC1_EC2_EC3$Result$p.value)
Judgements.5<-c(C1_EC1_EC2_EC3$Judgement,C2_EC1_EC2_EC3$Judgement,
                C3_EC1_EC2_EC3$Judgement,C4_EC1_EC2_EC3$Judgement)
results_cp.5<-cbind(Kappa_values.5,pvalues.5,Judgements.5)
rownames(results_cp.5)<-c('C1&EC1_EC2_EC3','C2&EC1_EC2_EC3','C3&EC1_EC2_EC3','C4&EC1_EC2_EC3')
##
C1_C2_C3_C4_EC1_EC2_EC3<-Kappa.test(C1*C2*C3*C4,EC1*EC2*EC3,conf.level = 0.90)
Kappa_values.6<-c(C1_C2_C3_C4_EC1_EC2_EC3$Result$estimate)
pvalues.6<-c(C1_C2_C3_C4_EC1_EC2_EC3$Result$p.value)
Judgements.6<-c(C1_C2_C3_C4_EC1_EC2_EC3$Judgement)
results_cp.6<-cbind(Kappa_values.6,pvalues.6,Judgements.6)
rownames(results_cp.6)<-c('C1_C2_C3_C4&EC1_EC2_EC3')
##
cbresults<-rbind(results_cp,results_cp.2,results_cp.3,results_cp.4,results_cp.5,results_cp.6)
dim(results_cp.2)

colnames(cbresults)<-c('Kappa_values','pvalues','Judgements')
cbresults

dim(cbresults)
write.table(cbresults, "/Users/hemu/Desktop/ECG_NEW_RESULTS14.txt", sep="\t")
##
Kappa.test(C1*C2*C3*C4,EC3,conf.level = 0.90)
##
colnames(df)<-c('C1','C2','C3','C4','EC1','EC2','EC3')
data_com<-df[complete.cases(df), ]
#cor(df)
cor(data_com)
##
#ggpairs(data_withoutna)
#data_2

library(pROC)
data(aSAH)
roc(aSAH$outcome, aSAH$s100b)
roc(outcome ~ s100b, aSAH, smooth=TRUE)
roc1 <- roc(aSAH$outcome,
            aSAH$s100b, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)
roc2 <- roc(aSAH$outcome, aSAH$wfns,
            plot=TRUE, add=TRUE, percent=roc1$percent)
# Coordinates of the curve ##
coords(roc1, "best", ret=c("threshold", "specificity", "1-npv"))
coords(roc2, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))
## Confidence intervals ##
# CI of the AUC
ci(roc2)
## Not run:
# CI of the curve
sens.ci <- ci.se(roc1, specificities=seq(0, 100, 5))
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")
are.paired
## End(Not run)
# need to re-add roc2 over the shape
plot(roc2, add=TRUE)
## Not run:
# CI of thresholds
plot(ci.thresholds(roc2))
## End(Not run)
# In parallel
if (require(doParallel)) {
  registerDoParallel(cl <- makeCluster(getOption("mc.cores", 2L)))
  ## Not run: ci(roc2, method="bootstrap", parallel=TRUE)
  stopCluster(cl)
}
## Comparisons ##
# Test on the whole AUC
roc.test(roc1, roc2, reuse.auc=FALSE)
## Not run:
# Test on a portion of the whole AUC
roc.test(roc1, roc2, reuse.auc=FALSE, partial.auc=c(100, 90),
         partial.auc.focus="se", partial.auc.correct=TRUE)
# With modified bootstrap parameters
roc.test(roc1, roc2, reuse.auc=FALSE, partial.auc=c(100, 90),
         partial.auc.correct=TRUE, boot.n=1000, boot.stratified=FALSE)
## End(Not r


data_2
head(data_2)
tn_count<-rep(NA,length(C1))
tn_count<-0
tp_count<-0
fp_count<-0
fn_count<-0
for(i in 1: length(C1)){
  if(C1[i]==0 && EC1[i]==0){tn_count=tn_count+1}
  if(C1[i]==1 && EC1[i]==1){tp_count=tp_count+1}
  if(C1[i]==0 && EC1[i]==1){fn_count=fn_count+1}
  if(C1[i]==1 && EC1[i]==0){fp_count=fp_count+1}
}
C1_EC1 <- matrix(c(tp_count,fp_count,fn_count,tn_count),ncol=2,byrow=TRUE)
colnames(C1_EC1) <- c("EC1=1","EC1=0")
rownames(C1_EC1) <- c("C1=1","C1=0")
TPR<-tp_count/(tp_count+fn_count)
TPR
FPR<-fp_count/(fp_count+tn_count)
FPR
PPV<-tp_count/(tp_count+fp_count)
PPV
ACC<-(tp_count+tn_count)/(tp_count+tn_count+fp_count+fn_count)
ACC
c(TPR,FPR,PPV,ACC)
###
head(data_2)
#lvs <- c("normal", "abnormal")
#truth <- factor(rep(lvs, times = c(86, 258)),levels = rev(lvs))
#pred <- factor(c(rep(lvs, times = c(54, 32)),rep(lvs, times = c(27, 231))),levels = rev(lvs))
#xtab <- table(pred, truth)
# load Caret package for computing Confusion matrix
library(caret) 
###C1
t_C1_EC1<-table(C1,EC1)
confusionMatrix(t_C1_EC1)
t_C1_EC2<-table(C1,EC2)
confusionMatrix(t_C1_EC2)
t_C1_EC3<-table(C1,EC3)
confusionMatrix(t_C1_EC3)
###C2
t_C2_EC1<-table(C2,EC1)
confusionMatrix(t_C2_EC1)
t_C2_EC2<-table(C2,EC2)
confusionMatrix(t_C2_EC2)
t_C2_EC3<-table(C2,EC3)
confusionMatrix(t_C2_EC3)
###C3
t_C3_EC1<-table(C3,EC1)
confusionMatrix(t_C3_EC1)
t_C3_EC2<-table(C3,EC2)
confusionMatrix(t_C3_EC2)
t_C3_EC3<-table(C3,EC3)
confusionMatrix(t_C3_EC3)
###C4
t_C4_EC1<-table(C4,EC1)
confusionMatrix(t_C4_EC1)
t_C4_EC2<-table(C4,EC2)
confusionMatrix(t_C4_EC2)
t_C4_EC3<-table(C4,EC3)
confusionMatrix(t_C4_EC3)
###C1_C2 and EC1
t_C1_C2_EC1<-table(C1*C2,EC1)
confusionMatrix(t_C1_C2_EC1)
t_C1_C3_EC1<-table(C1*C3,EC1)
confusionMatrix(t_C1_C3_EC1)
t_C1_C4_EC1<-table(C1*C4,EC1)
confusionMatrix(t_C1_C4_EC1)
t_C2_C3_EC1<-table(C2*C3,EC1)
confusionMatrix(t_C2_C3_EC1)
t_C2_C4_EC1<-table(C2*C4,EC1)
confusionMatrix(t_C2_C4_EC1)
t_C3_C4_EC1<-table(C3*C4,EC1)
confusionMatrix(t_C3_C4_EC1)
####
t_C1_C2_EC2<-table(C1*C2,EC2)
confusionMatrix(t_C1_C2_EC2)
t_C1_C3_EC2<-table(C1*C3,EC2)
confusionMatrix(t_C1_C3_EC2)
t_C1_C4_EC2<-table(C1*C4,EC2)
confusionMatrix(t_C1_C4_EC2)
t_C2_C3_EC2<-table(C2*C3,EC2)
confusionMatrix(t_C2_C3_EC2)
t_C2_C4_EC2<-table(C2*C4,EC2)
confusionMatrix(t_C2_C4_EC2)
t_C3_C4_EC2<-table(C3*C4,EC2)
confusionMatrix(t_C3_C4_EC2)
###
t_C1_C2_EC3<-table(C1*C2,EC3)
confusionMatrix(t_C1_C2_EC3)
t_C1_C3_EC3<-table(C1*C3,EC3)
confusionMatrix(t_C1_C3_EC3)
t_C1_C4_EC3<-table(C1*C4,EC3)
confusionMatrix(t_C1_C4_EC3)
t_C2_C3_EC3<-table(C2*C3,EC3)
confusionMatrix(t_C2_C3_EC3)
t_C2_C4_EC3<-table(C2*C4,EC3)
confusionMatrix(t_C2_C4_EC3)
t_C3_C4_EC3<-table(C3*C4,EC3)
confusionMatrix(t_C3_C4_EC3)
###