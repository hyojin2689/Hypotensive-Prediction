1.사용 

library(dplyr)
library(stringr)
library(tidyverse)
library(e1071)
library(changepoint)
library(RWeka)

2.데이터 
●250 Hz
●관찰기간 : 1분
●저혈압 기준 : 최소값이 50이하

SRATE<-250
MINUTES_AHEAD=1
Data_set<-data.frame() #샘플 생성 후 저장할 공간 

#moving average 사용자 정의 함수 생성
ma<-function(x,n=5){stats::filter(x,rep(1/n,n),sides=2)}

#(:불필요한 행과 문자 삭제 및 필요한 값만 추출) 후 객체화
for (file in fls){
  df<- read.csv(file)
  df<-df[-c(1,2),] 
  df$signal<-str_replace(df$signal,'\\[','')
  df$signal<-str_replace(df$signal,'\\]','')
  c = str_split(df$signal,",")
  class <- c()
  for(i in 1:length(c)){
    class <- c(class, c[[i]][2])
  }
  df$signal<-class
  assign(file,df) #전처리 된 파일들을 객체화해서 다시 file에 넣어줌
}

Data_set<-list() #샘플 생성 후 저장할 공간

#데이터셋만들기
for (file in fls)){
  IBP<-as.numeric(get(file)$IBP) #문자인 signal컬럼 내용을 숫자로 변환
  i<-1
  IBP_data<-data.frame()
  while(i<length(IBP)-SRATE*(1+1+MINUTES_AHEAD)*60){
    segx <- IBP[i:(i+SRATE*1*60-1)] #1분간의 데이터 관찰
    segy<-IBP[(i+SRATE*(1+MINUTES_AHEAD)*60):(i+SRATE*(1+1+MINUTES_AHEAD)*60-1)] #1분뒤 저혈압 발생여부를 1분동안 관찰하여 class화 
    segxd<-IBP[i:(i+SRATE*(1+MINUTES_AHEAD)*60-1)]
    if(is.na(mean(segx))| #데이터 제외조건들
       is.na(mean(segy))|
       max(segx)>200| min(segx)<20|
       max(segy)>200| min(segy)<20|
       max(segx)-min(segx)<30|
       max(segy)-min(segy)<30|(min(segxd,na.rm=T)<=50)){
    }else{ #나머지의 경우
      segy<-ma(segy,2*SRATE)
      event<-ifelse(min(segy,na.rm=T)<=50,1,0)
      IBP_data<-rbind(IBP_data,cbind(t(segx),event))
    } #저혈압 조건 : 평균 혈압을 계산하여 50이하면 저혈압
    i<-i+1*60*SRATE #1분 간격으로 샘플 만들기
  }
  Data_set[[file]]<-IBP_data
}  

#전체 데이터 합치기 : 파일들을 하나의 데이터 프레임으로 합치기(final_data)
final_data<-data.frame()
for(file in fls){
  data<- Data_set[[file]]
  final_data<-rbind(final_data,data)
}

#Feature추가
●Statistical Features : 통계특징
-mean, max, min, sd, skewness, rms, rss, IQR, kurtosis (평균, 최대, 최소, 표준편차, 왜도, rms, rss, 첨도)

final_data2<-subset(final_data,select=-event)

#rss/skewness함수 정의
rss<-function(x) rms(x)*(length(x))^0.5
skewness<-function(x){
  (sum((x-mean(x))^3)/length(x))/((sum((x-mean(x))^2)/length(x)))^(3/2)
}

#행별로 mean, max,min 등의 함수를 적용하여 v_mean. v_max 등의 컬럼 생성하여 추가하기
fun_list<- c("mean", "max","min","sd","skewness","rms","rss","IQR","kurtosis")
for(fun in fun_list){
  final_data2[,str_c("v_", fun)]<- as.numeric(apply(final_data2, 1, fun))
}

#final_data3 : 위에서 행별로 함수를 적용하여 추출한 컬럼과 원래 final_data의 event 컬럼만 뽑아서 합치기 
final_data3<-final_data2%>%select(v_mean,v_max,v_min,v_sd,v_skewness,v_rms,v_rss,v_kurtosis)
final_data3<-cbind(final_data3,final_data$event)
names(final_data3)[9]<-"event"
View(final_data3)

●ChangePoint Features : 변화분석
-cpt.mean, cpt.var, cpt.meanvar (평균의 변화, 분산의 변화, 평균과 분산의 변화)

chpt_df <- data.frame()
for(i in 1:nrow(final_data2)){ #event 컬럼을 제거해 놓았던 final_data2 데이터 이용
  
  cp_mean<- cpt.mean(as.numeric(final_data2[i,]))
  cp_mean<- cpts(cp_mean)
  cp_var<- cpt.var(as.numeric(final_data2[i,]))
  cp_var<- cpts(cp_var)
  cp_m_var<- cpt.meanvar(as.numeric(final_data2[i,]))
  cp_m_var<- cpts(cp_m_var)
  
  chpt_df <- 
    rbind(chpt_df, data.frame(cp1 = length(cp_mean), cp2 = length(cp_var), cp3 = length(cp_m_var)))
}

View(chpt_df)

#real_fianl : 변화분석 + 통계분석 합쳐서 데이터 만들기
real_final<-cbind(final_data3,chpt_df)
View(real_final)

3.모델링
●Random Forest model 사용

RF<-make_Weka_classifier("weka/classifiers/trees/RandomForest")
m<-RF(event~., data=real_final)

4.평가:10 Fold cross-validation**

e<-evaluate_Weka_classifier(m,numFolds=10,complexity=TRUE,class=TRUE)
e


