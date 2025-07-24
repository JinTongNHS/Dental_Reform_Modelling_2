#This file models 10 years trend based on behaviour changes.
library(magrittr)
library(scales)
library(tidyverse)
library(dplyr)

period=c("year1","year2","year3","year4","year5","year6","year7","year8","year9","year10")
normal_new_b23=as.numeric(get_Var(6)) #% new band 2/3 that are normal
`%_unmodelled_activity`= as.numeric(get_Var(7))/commisioned_spend #cost of unmodelled activities
inflation=as.numeric(get_Var(8)) #inflation applied from 26/27 onward

#To calculate % of total UDA for each segment over 10 years taking behaviour changes impacts into account
`trend_%_UDA`<-function(){
model<-get_yr0_pre_model()%>%
  select(Seg_short,`yr0_pre_%_UDA`=`Total_spend_%`)%>%
  left_join(get_yr0_post_model()%>% select(Seg_short,`yr0_post_%_UDA`=`Total_spend_%`), "Seg_short")%>%
  mutate(`yr0_pre_%_UDA`=as.numeric(`yr0_pre_%_UDA`), 
         `yr0_post_%_UDA`=as.numeric(`yr0_post_%_UDA`), 
         year1=as.numeric(`yr0_post_%_UDA`), 
         year2=year1, year3=year2, year4=year3, 
         year5=year4, year6=year5, year7=year6, 
         year8=year7, year9=year8,year10=year9)%>% #Taking yr0 post change %total spend as the starting values
  filter(Seg_short!="Unused_UDA")%>%
  collect()

rownames(model)<-model$Seg_short

#replacing starting values where segments affected by behaviour changes. The ones not affected remain the same as yr0 post change values.
for (i in (1:length(period)) ){
  model["new_b1", period[i]]<- model["new_b1", "yr0_post_%_UDA"]*get_behaviour(2)[1,i+2] #behavior changes 2 impact on new band 1 segment
  model["return_b1", period[i]]<- model["return_b1", "yr0_post_%_UDA"]*get_behaviour(1)[1,i+2]#behavior changes 1 impact on returning band 1 segment
  model["new_hn_pat", period[i]]<-model["new_hn_pat", "yr0_post_%_UDA"]+ (1-normal_new_b23)*get_behaviour(4)[1,i+2] #number 4 in behavior input is reluctant increase in new band 2/3
  model["new_b23", period[i]]<-model["new_b23", "yr0_post_%_UDA"]+ normal_new_b23*get_behaviour(4)[1,i+2] #number 4 in behavior input is reluctant increase in new band 2/3
  
}
unused_udat=1-sum(model$`Total_spend_%`, na.rm=T)
model[nrow(model) + 1, ] <- c("Unused_UDA",
                              (1-sum(model$`yr0_pre_%_UDA`, na.rm=T)),
                              (1-sum(model$`yr0_post_%_UDA`, na.rm=T)),
                              (1-sum(model$year1, na.rm=T)),
                              (1-sum(model$year2, na.rm=T)),
                              (1-sum(model$year3, na.rm=T)),
                              (1-sum(model$year4, na.rm=T)),
                              (1-sum(model$year5, na.rm=T)),
                              (1-sum(model$year6, na.rm=T)),
                              (1-sum(model$year7, na.rm=T)),
                              (1-sum(model$year8, na.rm=T)),
                              (1-sum(model$year9, na.rm=T)),
                              (1-sum(model$year10, na.rm=T)))
model[,c("yr0_pre_%_UDA", "yr0_post_%_UDA", period)]<-lapply(model[,c("yr0_pre_%_UDA", "yr0_post_%_UDA", period)], as.numeric)

model
}

#To calculate discounted inflation rate at 26/27 prices
disc_inflation<-function(){
  model<-discount_input
  
  for (i in (1:length(period)) ){
    model[1, period[i]]<- inflation/model[1, period[i]] #behavior changes 2 impact on new band 1 segment
  }
  
  model 
  
}

trend_spend<-function(){
  unused_uda_post= as.numeric((get_yr0_post_model()%>%filter(Seg_short=="Unused_UDA"))$`Total_spend_%`)- `%_unmodelled_activity`
  
  model<- get_behaviour(3)%>%
    mutate(yr0_pre_change=unused_uda, yr0_post_change=unused_uda_post)# number 3 change - total undelivered UDAs
  
  for (i in (1:length(period)) ){
    model[1, period[i]]<- model[1, period[i]]- `%_unmodelled_activity` #behavior changes 2 impact on new band 1 segment
  }
  
model<-model%>%
  mutate(row_name="% Unused UDAs taking non-modelled activities into account")%>%
  select(row_name,yr0_pre_change,yr0_post_change,year1,year2,year3,year4,year5,year6,year7,year8,year9,year10)

model[nrow(model) + 1, ] <- c("Underspend with inflation £",
                              model$yr0_pre_change*commisioned_spend ,
                              model$yr0_post_change*commisioned_spend*inflation,
                              model$year1*commisioned_spend*inflation,
                              model$year2*commisioned_spend*inflation,
                              model$year3*commisioned_spend*inflation,
                              model$year4*commisioned_spend*inflation,
                              model$year5*commisioned_spend*inflation,
                              model$year6*commisioned_spend*inflation,
                              model$year7*commisioned_spend*inflation,
                              model$year8*commisioned_spend*inflation,
                              model$year9*commisioned_spend*inflation,
                              model$year10*commisioned_spend*inflation)

disc_inflation<-disc_inflation()
model[nrow(model) + 1, ] <- c("Commissioned spend (Real) £",
                              commisioned_spend,
                              commisioned_spend,
                              commisioned_spend*disc_inflation$year1,
                              commisioned_spend*disc_inflation$year2,
                              commisioned_spend*disc_inflation$year3,
                              commisioned_spend*disc_inflation$year4,
                              commisioned_spend*disc_inflation$year5,
                              commisioned_spend*disc_inflation$year6,
                              commisioned_spend*disc_inflation$year7,
                              commisioned_spend*disc_inflation$year8,
                              commisioned_spend*disc_inflation$year9,
                              commisioned_spend*disc_inflation$year10)

model[,c("yr0_pre_change", "yr0_post_change", period)]<-lapply(model[,c("yr0_pre_change", "yr0_post_change", period)], as.numeric)
model
}

trend_seg_spend<-function(case="do nothing"){
  
  real_commisioned<-trend_spend()%>%filter(row_name=="Commissioned spend (Real) £")
  uda_split<-`trend_%_UDA`()%>%filter(Seg_short!="Unused_UDA")
  
  if (case== "do nothing"){
    model<-uda_split%>%
      select(Seg_short, `yr0_pre_%_UDA`)%>%
      mutate(Year0_pre_Change=`yr0_pre_%_UDA`*real_commisioned$`yr0_pre_change`,
             Year0_post_Change="")
    
    for (i in (1:length(period)) ){
      model<-model%>%mutate(!!paste0("year",i):=`yr0_pre_%_UDA`*real_commisioned[1, i+3])
    }
  }else { 
    model<-uda_split%>%
      select(-`yr0_pre_%_UDA`)%>%
      mutate(Year0_pre_Change="",
             Year0_post_Change=`yr0_post_%_UDA`*real_commisioned$`yr0_post_change`)
  
    for (i in (1:length(period)) ){
      model[[paste0("year",i)]]=model[[paste0("year",i)]]*real_commisioned[1, i+3]
    }
  
  }
  model<-model%>%select(Seg_short, Year0_pre_Change, Year0_post_Change, year1,year2,year3,year4,year5,year6,year7,year8,year9,year10)
  model[,c("Year0_pre_Change", "Year0_post_Change", period)]<-lapply(model[,c("Year0_pre_Change", "Year0_post_Change", period)], as.numeric)
  
  total<-colSums(subset(model, select=(-Seg_short)))
   model<-rbind(model, c("Total spend (Real)", total))
  
  model
}
