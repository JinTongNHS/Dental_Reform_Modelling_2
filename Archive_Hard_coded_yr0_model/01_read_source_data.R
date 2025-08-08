#This file is read all source data file required in this modelling

#All source data files are saved in [00_Source_data] folder, and can be refreshed/replaced when refreshing the model

library(tidyverse)
library(magrittr)
library(openxlsx)


#### Source data files ----
setwd("C:/Users/jin.tong/Documents/Rprojects/Dental_Reform_Modelling_2")

dental_stats_1c<-read.csv("Archive_Hard_coded_yr0_model/dental_stats_1c.csv", skip=4)
dental_stats_2c<-read.csv("Archive_Hard_coded_yr0_model/dental_stats_2c.csv", skip=4)
dental_stats_5a<-read.csv("Archive_Hard_coded_yr0_model/dental_stats_5a.csv", skip=4)
dental_stats_6a<-read.csv("Archive_Hard_coded_yr0_model/dental_stats_6a.csv", skip=4)
pcr_2425<-read.csv("Archive_Hard_coded_yr0_model/PCR_2425.csv")

smt_pack<- read.csv("Archive_Hard_coded_yr0_model/SMT_pack.csv")%>%
  mutate(`calendar_month`= as.Date(paste0(`calendar_month`, "-01"), , "%Y-%m-%d"))%>%
  collect()

t1<-smt_pack%>%filter(calendar_month >= "2024-03-01" & calendar_month <= "2024-10-01")## ??? why choose these 8 months ????


uda_projection<- read.csv("Archive_Hard_coded_yr0_model/UDA_projection.csv", skip=2)[c(1, 14:25)]

hn_pat_dist<-read.csv("Archive_Hard_coded_yr0_model/HN_patients_distribution.csv",skip=1, col.names=c("Lab", "Pathway 1", "Pathway 2") )

input_var<-read.xlsx("Archive_Hard_coded_yr0_model/Input_for_testing.xlsx", sheet = "Assumptions", startRow = 1)

get_Var<-function(Parameter_ID="1"){
  v=(input_var%>%filter(id==Parameter_ID))$Value
  v
}

get_cot<-function(patient="Child", b="Band.1"){
  if(patient=="Child"){
    data<- dental_stats_1c %>% 
      filter( `Patient.Type`== "Child")%>%
      select(all_of(b))}
  else if(patient=="Non-Exempt"){
    data<- dental_stats_1c %>% 
      filter( `Patient.Type`== "Non-Exempt")%>%
      select(all_of(b))}
  else{ data<- dental_stats_1c %>% 
    filter( `Patient.Type`!= "Child")%>%
    select(all_of(b))}
  
  v=sum(data)
  v
}

get_uda<-function(FY="2023/2024", patient="Child",b="Band.1"){
  if(patient=="Child"){
    data<-dental_stats_2c %>% 
      filter(`Financial.Year`==FY, `Patient.Type`== "Child")%>%
      select(all_of(b))}
  else{data<-dental_stats_2c %>% 
    filter(`Financial.Year`==FY, `Patient.Type`!= "Child")%>%
    select(all_of(b))}
  
  v=sum(data)
  v
}

total_uda<- get_uda("2023/2024", "Child", "Total")+get_uda("2023/2024", "non_Child", "Total")

get_pcr_fee<-function(b="Band 1"){
  data<-pcr_2425%>%filter(band==b)%>%collect()
  v=data$pcr_2425
  v
}

get_pcr_total<-function( b=c("Band.1")){
  data<- dental_stats_6a %>% 
    filter( `Financial.Year`== "inflated to 2024/25 - 4%")%>%
    select(all_of(b))
  
  v=sum(data)
  v
}

