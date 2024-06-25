#데이터전처리 함수.

#rm(list=ls())

#setwd("C:/Users/이다현/Desktop/2022 4학년 1학기/데이터마이닝 팀플/마이닝 팀플")
getwd()
library(tidyverse)
library(spatstat)
library(ggplot2)
library(car)
library(corrplot)
library(scales)
library(gridExtra)

#READ ME============================
#source("data_cleaning_function_ldh220517.R") #한 후 fin(MOD[i]), i=1~32하면 상응하는 데이터 반환함.
#함수는 데이터를 반환하므로 str(함수)또는 변수에 저장하길 권함.

#데이터함수(fin)정의에 필요한 함수들:out,add,std
#표준화:(x-mean(x))/sd(x)=scale(x) #정규화:(x-min(x))/(max(x)-min(x))
#str(fin(MOD[32]))
#=====================================
#데이터 특징 및 MOD벡터 대해.
#첫번째기준)결측치
#-1:결측치제거.
#두번째기준)이상치 처리여부=
#-1:이상치제거x,n=7032, 2:churnyes중tchar만제거,n=6923 3.(tenure*mchar-tchar)만제거,n=6354 4.모두제거,n=6287
#세번째기준)(internetService변수)인코딩 여부=
#-1:더미(dsl,fiber,no), p=35, 2:이진(0,1), p=33
#네번째 기준)(연속형변수)분포변환 여부=
#-1:변환x, 2:log, 3:정규화, 4:표준화

#데이터예시: dim(fin(1424))
#fin(1424)는 결측치제거&이상치모두제거&이진인코딩&표준화한 데이터
#fin함수에 들어가는 값은 MOD벡터의 원소1개.
#데이터 개수:32개. (1*4*2*4*(1)).

MOD=c(1111, 1211, 1311, 1411,
      1121, 1221, 1321, 1421,
      
      1112, 1212, 1312, 1412,
      1122, 1222, 1322, 1422,
      
      1113, 1213, 1313, 1413,
      1123, 1223, 1323, 1423,
      
      1114, 1214, 1314, 1414,
      1124, 1224, 1324, 1424)
#===========================================

#=====================
#out함수: 결측치제거(1)&이상치제거여부(1.제거x,2.churnyes중 tchar만제거 3.(tenure*mchar-tchar)만제거 4.모두제거)
#인수: mod=c(11,12,13,14) 중 하나의 값
#반환: 데이터
out=function(mod){  
  data = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv") #read data
  data1 = na.omit(data) #data1: 결측치 제거
  if (mod==11){
    data11=data1
    return(data11)
  }else { #이상치 제거
    tt_ch_yes = subset(data1, subset = (Churn == "Yes"))
    upperq1 = quantile(tt_ch_yes$TotalCharges)[4]
    lowerq1 =quantile(tt_ch_yes$TotalCharges)[2]
    iqr1 = upperq1 - lowerq1
    
    t_M_Charges = data1$tenure * data1$MonthlyCharges
    upperq2 = quantile(t_M_Charges - data1$TotalCharges, na.rm = T)[4]
    lowerq2 = quantile(t_M_Charges - data1$TotalCharges, na.rm = T)[2]
    iqr2 = upperq2 - lowerq2
    
    if (mod==12){
      data1[data1$Churn == "Yes","TotalCharges"] = ifelse(tt_ch_yes$TotalCharges > upperq1 + iqr1*1.5, NA, tt_ch_yes$TotalCharges)
      data12=na.omit(data1)
      return(data12)
    }else if (mod==13){
      data1$TotalCharges = ifelse((t_M_Charges - data1$TotalCharges) > (upperq2 + iqr2*1.5) | (t_M_Charges - data1$TotalCharges) < (lowerq2 - iqr2*1.5), NA, data1$TotalCharges)
      data13=na.omit(data1)
      return(data13)
    }else if (mod==14){
      data1[data1$Churn == "Yes","TotalCharges"] = ifelse(tt_ch_yes$TotalCharges > upperq1 + iqr1*1.5, NA, tt_ch_yes$TotalCharges)
      data1$TotalCharges = ifelse((t_M_Charges - data1$TotalCharges) > (upperq2 + iqr2*1.5) | (t_M_Charges - data1$TotalCharges) < (lowerq2 - iqr2*1.5), NA, data1$TotalCharges)
      data14=na.omit(data1)
      return(data14)
    }else {
      print("error:wrong input")}
  }
}
#dim(data); dim(out(11)); dim(out(12)); dim(out(13)); dim(out(14)) #dim(data)->7043, dim(out(14))->6287
#======================
#add함수: 범주변수 인코딩, 파생변수 생성함수.
#인수: 데이터
#반환: 데이터
add=function(data1){    
  data1$SeniorCitizen <- factor(data1$SeniorCitizen, levels = c(0,1), labels = c("No", "Yes"))
  data1$MultipleLines = as.factor(gsub("No phone service", "No",data1$MultipleLines))
  data1$OnlineSecurity  = as.factor(gsub("No internet service", "No", data1$OnlineSecurity))
  data1$OnlineBackup  = as.factor(gsub("No internet service", "No", data1$OnlineBackup))
  data1$DeviceProtection  = as.factor(gsub("No internet service", "No", data1$DeviceProtection))
  data1$TechSupport  = as.factor(gsub("No internet service", "No", data1$TechSupport))
  data1$StreamingTV  = as.factor(gsub("No internet service", "No", data1$StreamingTV))
  data1$StreamingMovies  = as.factor(gsub("No internet service", "No", data1$StreamingMovies))
  
  #연속형, 범주형 split
  for(i in 1:ncol(data1)){
    if(typeof(data1[,i]) == "character"){
      data1[,i] = as.factor(data1[,i])
    }else{
      data1[,i] = data1[,i]
    }
  }
  data1_fac = select_if(data1, is.factor)
  data1_fac = data1_fac[,-1]
  
  #데이터-범주(before 파생변수 생성)
  data1_bin = data1_fac[,-c(1, 7, 14, 16)]  
  data1_bin = as.data.frame(sapply(data1_bin, function(x){ifelse(x == "Yes", 1, 0)}))   #인코딩(no=0, yes=1, InternetService(dsl,fiber,no))
  data1_dum = data1_fac[,c('gender', 'InternetService', 'Contract', 'PaymentMethod')]
  #데이터-연속(before정규화 등 분포변환)
  data1_con = select_if(data1, is.numeric)
  dataad = cbind(customerID = data1$customerID ,data1_con, dummify(data1_dum),data1_bin)
  #파생변수
  dataad$InternetService_dum=ifelse(data1_dum$InternetService=="No",0,1) #인코딩(InternetService_dum(0,1))
  dataad$phs=dataad$PhoneService+dataad$MultipleLines
  dataad$streaming=dataad$StreamingTV+dataad$StreamingMovies
  dataad$security=with(dataad, OnlineSecurity+OnlineBackup+DeviceProtection)
  dataad$nsatisfy=with(dataad, 1+MultipleLines+streaming+security+TechSupport) 
  dataad$SubjCost=with(dataad, MonthlyCharges/nsatisfy)
  dataad$SubjBene=with(dataad, 100*nsatisfy/MonthlyCharges)
  
  return(dataad)
}
#================================================================
#std함수: 연속형 변수 대해 분포변환여부.(1.변환x,2.log 3.정규화 4.표준화)
#인수: mod=c(1,2,3,4)중 하나, 데이터 
#반환: 데이터
std=function(mod, dataad){ 
  data1_dum=subset(dataad,select=-c(tenure, MonthlyCharges, TotalCharges, SubjCost, SubjBene))
  data1_con=subset(dataad,select=c(tenure, MonthlyCharges, TotalCharges, SubjCost, SubjBene)) #nothing, log, 표준화, 정규화(변수명은 그대로) 
  if (mod==1){ #nothing
  }else if (mod==2){ #log
    data1_con=as.data.frame(lapply(data1_con, log))
  }else if (mod==3){ #정규화
    normalize=function(x){
      return((x-min(x))/(max(x)-min(x)))
    }
    data1_con=as.data.frame(lapply(data1_con, normalize))
    
  }else if (mod==4){ #표준화
    data1_con=as.data.frame(lapply(data1_con, scale))
  }else{
    print("error:wrong input")
  }
  datastd=cbind(data1_dum, data1_con)
  return(datastd)
}
#===============================================================
#===============================================================
#fin함수: 데이터전처리 함수.
#인수: MOD=c(1111,...)중 하나. 첫번째값은 1, 두번째값은 1~4, 세번째값은 1~2, 네번째값은 1~4.
#반환: 데이터
fin=function(mod){
  outmode=as.numeric(substr(as.character(mod), 1, 2)) #결측치제거&이상치 처리 여부
  dataout=out(outmode)
  dataad=add(dataout) #파생변수 추가
  stdmode=as.numeric(substr(as.character(mod), 4, 4)) #분포 변환(표준화 등) 여부
  datastd=std(stdmode, dataad)
  incomode=as.numeric(substr(as.character(mod), 3, 3)) #인코딩(더미/이진) 여부
  if (incomode==1){
    datainco=subset(datastd, select=-c(InternetService_dum))
  }else if(incomode==2){
    datainco=subset(datastd,select=-c(InternetService.DSL,InternetService.Fiber.optic,InternetService.No))    
  }else {print("error:wrong input")}
  datafin=datainco
  return(datafin) 
}
#=====================================