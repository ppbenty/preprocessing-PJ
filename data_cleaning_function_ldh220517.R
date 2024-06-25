#��������ó�� �Լ�.

#rm(list=ls())

#setwd("C:/Users/�̴���/Desktop/2022 4�г� 1�б�/�����͸��̴� ����/���̴� ����")
getwd()
library(tidyverse)
library(spatstat)
library(ggplot2)
library(car)
library(corrplot)
library(scales)
library(gridExtra)

#READ ME============================
#source("data_cleaning_function_ldh220517.R") #�� �� fin(MOD[i]), i=1~32�ϸ� �����ϴ� ������ ��ȯ��.
#�Լ��� �����͸� ��ȯ�ϹǷ� str(�Լ�)�Ǵ� ������ �����ϱ� ����.

#�������Լ�(fin)���ǿ� �ʿ��� �Լ���:out,add,std
#ǥ��ȭ:(x-mean(x))/sd(x)=scale(x) #����ȭ:(x-min(x))/(max(x)-min(x))
#str(fin(MOD[32]))
#=====================================
#������ Ư¡ �� MOD���� ����.
#ù��°����)����ġ
#-1:����ġ����.
#�ι�°����)�̻�ġ ó������=
#-1:�̻�ġ����x,n=7032, 2:churnyes��tchar������,n=6923 3.(tenure*mchar-tchar)������,n=6354 4.�������,n=6287
#����°����)(internetService����)���ڵ� ����=
#-1:����(dsl,fiber,no), p=35, 2:����(0,1), p=33
#�׹�° ����)(����������)������ȯ ����=
#-1:��ȯx, 2:log, 3:����ȭ, 4:ǥ��ȭ

#�����Ϳ���: dim(fin(1424))
#fin(1424)�� ����ġ����&�̻�ġ�������&�������ڵ�&ǥ��ȭ�� ������
#fin�Լ��� ���� ���� MOD������ ����1��.
#������ ����:32��. (1*4*2*4*(1)).

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
#out�Լ�: ����ġ����(1)&�̻�ġ���ſ���(1.����x,2.churnyes�� tchar������ 3.(tenure*mchar-tchar)������ 4.�������)
#�μ�: mod=c(11,12,13,14) �� �ϳ��� ��
#��ȯ: ������
out=function(mod){  
  data = read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv") #read data
  data1 = na.omit(data) #data1: ����ġ ����
  if (mod==11){
    data11=data1
    return(data11)
  }else { #�̻�ġ ����
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
#add�Լ�: ���ֺ��� ���ڵ�, �Ļ����� �����Լ�.
#�μ�: ������
#��ȯ: ������
add=function(data1){    
  data1$SeniorCitizen <- factor(data1$SeniorCitizen, levels = c(0,1), labels = c("No", "Yes"))
  data1$MultipleLines = as.factor(gsub("No phone service", "No",data1$MultipleLines))
  data1$OnlineSecurity  = as.factor(gsub("No internet service", "No", data1$OnlineSecurity))
  data1$OnlineBackup  = as.factor(gsub("No internet service", "No", data1$OnlineBackup))
  data1$DeviceProtection  = as.factor(gsub("No internet service", "No", data1$DeviceProtection))
  data1$TechSupport  = as.factor(gsub("No internet service", "No", data1$TechSupport))
  data1$StreamingTV  = as.factor(gsub("No internet service", "No", data1$StreamingTV))
  data1$StreamingMovies  = as.factor(gsub("No internet service", "No", data1$StreamingMovies))
  
  #������, ������ split
  for(i in 1:ncol(data1)){
    if(typeof(data1[,i]) == "character"){
      data1[,i] = as.factor(data1[,i])
    }else{
      data1[,i] = data1[,i]
    }
  }
  data1_fac = select_if(data1, is.factor)
  data1_fac = data1_fac[,-1]
  
  #������-����(before �Ļ����� ����)
  data1_bin = data1_fac[,-c(1, 7, 14, 16)]  
  data1_bin = as.data.frame(sapply(data1_bin, function(x){ifelse(x == "Yes", 1, 0)}))   #���ڵ�(no=0, yes=1, InternetService(dsl,fiber,no))
  data1_dum = data1_fac[,c('gender', 'InternetService', 'Contract', 'PaymentMethod')]
  #������-����(before����ȭ �� ������ȯ)
  data1_con = select_if(data1, is.numeric)
  dataad = cbind(customerID = data1$customerID ,data1_con, dummify(data1_dum),data1_bin)
  #�Ļ�����
  dataad$InternetService_dum=ifelse(data1_dum$InternetService=="No",0,1) #���ڵ�(InternetService_dum(0,1))
  dataad$phs=dataad$PhoneService+dataad$MultipleLines
  dataad$streaming=dataad$StreamingTV+dataad$StreamingMovies
  dataad$security=with(dataad, OnlineSecurity+OnlineBackup+DeviceProtection)
  dataad$nsatisfy=with(dataad, 1+MultipleLines+streaming+security+TechSupport) 
  dataad$SubjCost=with(dataad, MonthlyCharges/nsatisfy)
  dataad$SubjBene=with(dataad, 100*nsatisfy/MonthlyCharges)
  
  return(dataad)
}
#================================================================
#std�Լ�: ������ ���� ���� ������ȯ����.(1.��ȯx,2.log 3.����ȭ 4.ǥ��ȭ)
#�μ�: mod=c(1,2,3,4)�� �ϳ�, ������ 
#��ȯ: ������
std=function(mod, dataad){ 
  data1_dum=subset(dataad,select=-c(tenure, MonthlyCharges, TotalCharges, SubjCost, SubjBene))
  data1_con=subset(dataad,select=c(tenure, MonthlyCharges, TotalCharges, SubjCost, SubjBene)) #nothing, log, ǥ��ȭ, ����ȭ(�������� �״��) 
  if (mod==1){ #nothing
  }else if (mod==2){ #log
    data1_con=as.data.frame(lapply(data1_con, log))
  }else if (mod==3){ #����ȭ
    normalize=function(x){
      return((x-min(x))/(max(x)-min(x)))
    }
    data1_con=as.data.frame(lapply(data1_con, normalize))
    
  }else if (mod==4){ #ǥ��ȭ
    data1_con=as.data.frame(lapply(data1_con, scale))
  }else{
    print("error:wrong input")
  }
  datastd=cbind(data1_dum, data1_con)
  return(datastd)
}
#===============================================================
#===============================================================
#fin�Լ�: ��������ó�� �Լ�.
#�μ�: MOD=c(1111,...)�� �ϳ�. ù��°���� 1, �ι�°���� 1~4, ����°���� 1~2, �׹�°���� 1~4.
#��ȯ: ������
fin=function(mod){
  outmode=as.numeric(substr(as.character(mod), 1, 2)) #����ġ����&�̻�ġ ó�� ����
  dataout=out(outmode)
  dataad=add(dataout) #�Ļ����� �߰�
  stdmode=as.numeric(substr(as.character(mod), 4, 4)) #���� ��ȯ(ǥ��ȭ ��) ����
  datastd=std(stdmode, dataad)
  incomode=as.numeric(substr(as.character(mod), 3, 3)) #���ڵ�(����/����) ����
  if (incomode==1){
    datainco=subset(datastd, select=-c(InternetService_dum))
  }else if(incomode==2){
    datainco=subset(datastd,select=-c(InternetService.DSL,InternetService.Fiber.optic,InternetService.No))    
  }else {print("error:wrong input")}
  datafin=datainco
  return(datafin) 
}
#=====================================