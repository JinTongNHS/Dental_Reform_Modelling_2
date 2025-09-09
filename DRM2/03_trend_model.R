#This file models 10 years trend based on behaviour changes.

library(magrittr)
library(scales)
library(tidyverse)
library(dplyr)

n_year= as.numeric(get_assumption(9)) # total number of years modelled in trend analysis
period<-NULL
for (i in (1:n_year)){ period<-append(period, paste0("year",i))} #a list of years covering entire period

normal_new_b23=as.numeric(get_assumption(6)) #% new band 2/3 that are normal
`%_unmodelled_activity`= as.numeric(get_assumption(7))/commisioned_spend #cost of unmodelled activities
inflation=as.numeric(get_assumption(8)) #inflation applied from 26/27 onward


#To calculate discounted inflation rate at 26/27 prices
disc_inflation<-function(){
  model<-discount_input
  
  for (i in (1:n_year) ){
    model[1, period[i]]<- inflation/model[1, period[i]] #behavior changes 2 impact on new band 1 segment
  }
  
  model 
  
}

#To calculate % of total UDA for each segment over 10 years taking behaviour changes impacts into account
# `trend_%_UDA`<-function(){
# model<-get_yr0_pre_model()%>%
#   select(Seg_short,`yr0_pre_%_UDA`=`Total_spend_%`)%>%
#   left_join(get_yr0_post_model()%>% select(Seg_short,`yr0_post_%_UDA`=`Total_spend_%`), "Seg_short")%>%
#   mutate(`yr0_pre_%_UDA`=as.numeric(`yr0_pre_%_UDA`), 
#          `yr0_post_%_UDA`=as.numeric(`yr0_post_%_UDA`))%>% 
#   filter(Seg_short!="Unused_UDA")%>%
#   collect()
# 
# for (i in (1:n_year) ){
#   model<-model%>%mutate(!!paste0("year",i):=as.numeric(`yr0_post_%_UDA`))
# }
# 
# rownames(model)<-model$Seg_short
# 
# #replacing starting values where segments affected by behaviour changes. The ones not affected remain the same as yr0 post change values.
# for (i in (1:n_year) ){
#   model["new_b1", period[i]]<- model["new_b1", "yr0_post_%_UDA"]*get_behaviour("yr0_new_b1")[1,period[i]] #behavior changes 2 impact on new band 1 segment
#   model["return_b1", period[i]]<- model["return_b1", "yr0_post_%_UDA"]*get_behaviour("yr0_return_b1")[1,period[i]]#behavior changes 1 impact on returning band 1 segment
#   model["new_hn_pat", period[i]]<-model["new_hn_pat", "yr0_post_%_UDA"]+ (1-normal_new_b23)*get_behaviour("increased_new_b23")[1,period[i]] #number 4 in behavior input is reluctant increase in new band 2/3
#   model["new_b23", period[i]]<-model["new_b23", "yr0_post_%_UDA"]+ normal_new_b23*get_behaviour("increased_new_b23")[1,period[i]] #number 4 in behavior input is reluctant increase in new band 2/3
#   
# }
# 
# new_row<-c("Unused_UDA",
#            (1-sum(model$`yr0_pre_%_UDA`, na.rm=T)),
#            (1-sum(model$`yr0_post_%_UDA`, na.rm=T)) )
# 
# for (i in 1:n_year){ new_row<-append(new_row,(1-sum(model[paste0("year",i)], na.rm=T)) ) }
# 
# model[nrow(model) + 1, ] <- new_row
# 
# model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
# 
# model
# }
# 

trend_spend<-function(){
  unused_uda_post= as.numeric((get_yr0_post_model()%>%filter(Seg_short=="Unused_UDA"))$`Total_spend_%`)- `%_unmodelled_activity`
  
  model<- get_behaviour("undelivered_uda")%>%
    mutate(yr0_pre_change=unused_uda, yr0_post_change=unused_uda_post)# number 3 change - total undelivered UDAs
  
  for (i in (1:n_year) ){
    model[1, period[i]]<- model[1, period[i]]- `%_unmodelled_activity` #behavior changes 2 impact on new band 1 segment
  }
  
model<-model%>%
  mutate(row_name="% Unused UDAs taking non-modelled activities into account")%>%
  select(row_name,yr0_pre_change,yr0_post_change, all_of(period))


new_row_1<-c("Underspend with inflation £",
           model$yr0_pre_change*commisioned_spend ,
           model$yr0_post_change*commisioned_spend*inflation)
for (i in 1:n_year){ new_row_1<-append(new_row_1,model[paste0("year",i)]*commisioned_spend*inflation) }

disc_inflation<-disc_inflation()
new_row_2<- c("Commissioned spend (Real) £",
              commisioned_spend,
              commisioned_spend)
for (i in 1:n_year){ new_row_2<-append(new_row_2,disc_inflation[paste0("year",i)]*commisioned_spend) }

model[nrow(model) + 1, ] <- new_row_1
model[nrow(model) + 1, ] <- new_row_2

model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
model
}


trend<-function(metric="PCR"){
  if(metric=="%_UDA"){yr0_metric ="Total_spend_%"} else if(metric=="PCR"){yr0_metric="Total_PCR"} else if(metric=="COT"){yr0_metric="COT"}
  
  model<-get_yr0_pre_model()%>%
    select(Seg_short,!!paste0("yr0_pre_",metric):=any_of(yr0_metric))%>%
    left_join(get_yr0_post_model()%>% select(Seg_short,!!paste0("yr0_post_",metric):=any_of(yr0_metric)), "Seg_short")%>%
    filter(Seg_short!="Unused_UDA")%>%
    collect()
  
  #use year0 post change total PCR figures as the starting values to apply behaviour change impact where needed. 
  for (i in (1:n_year) ){
    model[[period[i]]]<-as.numeric(model[[paste0("yr0_post_",metric)]])
  }
  
  rownames(model)<-model$Seg_short
  
  #replacing starting values where segments affected by behaviour changes. The ones not affected remain the same as yr0 post change values.
  if(metric=="%_UDA"){
    for (i in (1:n_year) ){
      model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
      model["new_b1", period[i]]<- model["new_b1", "yr0_post_%_UDA"]*get_behaviour("yr0_new_b1")[1,period[i]] #behavior changes 2 impact on new band 1 segment
      model["return_b1", period[i]]<- model["return_b1", "yr0_post_%_UDA"]*get_behaviour("yr0_return_b1")[1,period[i]]#behavior changes 1 impact on returning band 1 segment
      model["new_hn_pat", period[i]]<-model["new_hn_pat", "yr0_post_%_UDA"]+ (1-normal_new_b23)*get_behaviour("increased_new_b23")[1,period[i]] #number 4 in behavior input is reluctant increase in new band 2/3
      model["new_b23", period[i]]<-model["new_b23", "yr0_post_%_UDA"]+ normal_new_b23*get_behaviour("increased_new_b23")[1,period[i]] #number 4 in behavior input is reluctant increase in new band 2/3
    }
    new_row<-c("Unused_UDA",
               (1-sum(model$`yr0_pre_%_UDA`, na.rm=T)),
               (1-sum(model$`yr0_post_%_UDA`, na.rm=T)) )
    
    for (i in 1:n_year){ new_row<-append(new_row,(1-sum(model[paste0("year",i)], na.rm=T)) ) }
    
    model[nrow(model) + 1, ] <- new_row
  } 
  else {
    if(metric=="PCR"){
      for (i in (1:n_year) ){
        model["return_b1", period[i]]<- model["return_b1", period[i]]*get_behaviour("yr0_return_b1")[1,period[i]]#behavior changes 1 impact on returning band 1 segment
        model["new_hn_pat", period[i]]<-model["new_hn_pat", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
        model["new_b23", period[i]]<-model["new_b23", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
      }
    } 
    else if(metric=="COT"){
      for (i in (1:n_year) ){
        model["new_b1", period[i]]<- model["new_b1", period[i]]*get_behaviour("yr0_new_b1")[1,period[i]] #behavior changes 2 impact on new band 1 segment
        model["return_b1", period[i]]<- model["return_b1", period[i]]-model["return_b1", period[i]]*(1-get_behaviour("yr0_new_b1")[1,period[i]])#behavior changes 1 impact on returning band 1 segment
        model["new_hn_pat", period[i]]<-model["new_hn_pat", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
        model["new_b23", period[i]]<-model["new_b23", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
      }
    } else{}
    
    model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
    total<-colSums(subset(model, select=(-Seg_short)))
    model<-rbind(model, c(paste0("Total ",metric), total), setNames(as.list(rep(NA, ncol(model))), names(model)))
    
    model["1", "Seg_short"]<- paste0("Change in ", metric," (compared to Year0 PreChange total ", metric)
    model["1", paste0("yr0_pre_",metric)]<- ""
    model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
    model["1", paste0("yr0_post_",metric)]<- model["10", paste0("yr0_post_",metric)]- model["10", paste0("yr0_pre_",metric)]
    
    for (i in (1:n_year) ){model["1", period[i]]<- model["10", period[i]]- model["10", paste0("yr0_pre_",metric)]}
  }
  
  model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)       
  model
  
}

trend_seg_spend<-function(DoNothing=TRUE){
  
  real_commisioned<-trend_spend()%>%filter(row_name=="Commissioned spend (Real) £")
  uda_split<-trend("%_UDA")%>%filter(Seg_short!="Unused_UDA")
  
  if (DoNothing== TRUE){
    model<-uda_split%>%
      select(Seg_short, `yr0_pre_%_UDA`)%>%
      mutate(Year0_pre_Change=`yr0_pre_%_UDA`*real_commisioned$`yr0_pre_change`,
             Year0_post_Change="")
    
    for (i in (1:n_year) ){
      model<-model%>%mutate(!!paste0("year",i):=`yr0_pre_%_UDA`*real_commisioned[1, period[i]])
    }
  }else { 
    model<-uda_split%>%
      select(-`yr0_pre_%_UDA`)%>%
      mutate(Year0_pre_Change="",
             Year0_post_Change=`yr0_post_%_UDA`*real_commisioned$`yr0_post_change`)
  
    for (i in (1:n_year) ){
      model[[paste0("year",i)]]=model[[paste0("year",i)]]*real_commisioned[1, period[i]]
    }
  
  }
  model<-model%>%select(Seg_short, Year0_pre_Change, Year0_post_Change, all_of(period))
  model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
  
  total<-colSums(subset(model, select=(-Seg_short)))
   model<-rbind(model, c("Total spend (Real)", total))
  
  model
}

trend_COT<-function(){
  model<-get_yr0_pre_model()%>%
    select(Seg_short,`yr0_pre_COT`=COT)%>%
    left_join(get_yr0_post_model()%>% select(Seg_short,`yr0_post_COT`=COT), "Seg_short")%>%
    filter(Seg_short!="Unused_UDA")%>%
    collect()
  #use year0 post change cOT figures as the starting values to apply behaviour change impact where needed.
  for (i in (1:n_year) ){
    model<-model%>%mutate(!!paste0("year",i):=as.numeric(`yr0_post_COT`))
  }

  rownames(model)<-model$Seg_short

  #replacing starting values where segments affected by behaviour changes. The ones not affected remain the same as yr0 post change values.
  for (i in (1:n_year) ){
    model["new_b1", period[i]]<- model["new_b1", period[i]]*get_behaviour("yr0_new_b1")[1,period[i]] #behavior changes 2 impact on new band 1 segment
    model["return_b1", period[i]]<- model["return_b1", period[i]]-model["return_b1", period[i]]*(1-get_behaviour("yr0_return_b1")[1,period[i]])#behavior changes 1 impact on returning band 1 segment
    model["new_hn_pat", period[i]]<-model["new_hn_pat", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
    model["new_b23", period[i]]<-model["new_b23", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
  }

  model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
  total<-colSums(subset(model, select=(-Seg_short)))
  model<-rbind(model, c("Total COT", total), setNames(as.list(rep(NA, ncol(model))), names(model)))

  model["1", "Seg_short"]<- "Change in COT (compared to Year0 PreChange total COT)"
  model["1", "yr0_pre_COT"]<- ""
  model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
  model["1", "yr0_post_COT"]<- model["10", "yr0_post_COT"]- model["10", "yr0_pre_COT"]

  for (i in (1:n_year) ){
    model["1", period[i]]<- model["10", period[i]]- model["10", "yr0_pre_COT"]
  }

  model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
 model

}
# 
# trend_PCR<-function(){
#   model<-get_yr0_pre_model()%>%
#     select(Seg_short,`yr0_pre_PCR`=Total_PCR)%>%
#     left_join(get_yr0_post_model()%>% select(Seg_short,`yr0_post_PCR`=Total_PCR), "Seg_short")%>%
#     filter(Seg_short!="Unused_UDA")%>%
#     collect()
#   #use year0 post change total PCR figures as the starting values to apply behaviour change impact where needed. 
#   for (i in (1:n_year) ){
#     model<-model%>%mutate(!!paste0("year",i):=as.numeric(`yr0_post_PCR`))
#   }
#   
#   rownames(model)<-model$Seg_short
#   
#   #replacing starting values where segments affected by behaviour changes. The ones not affected remain the same as yr0 post change values.
#   for (i in (1:n_year) ){
#     model["return_b1", period[i]]<- model["return_b1", period[i]]*get_behaviour("yr0_return_b1")[1,period[i]]#behavior changes 1 impact on returning band 1 segment
#     model["new_hn_pat", period[i]]<-model["new_hn_pat", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
#     model["new_b23", period[i]]<-model["new_b23", period[i]]*(1+get_behaviour("increased_new_b23")[1,period[i]]) #number 4 in behavior input is reluctant increase in new band 2/3
#   }
#   
#   model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
#   total<-colSums(subset(model, select=(-Seg_short)))
#   model<-rbind(model, c("Total PCR", total), setNames(as.list(rep(NA, ncol(model))), names(model)))
#   
#   model["1", "Seg_short"]<- "Change in PCR (compared to Year0 PreChange total PCR)"
#   model["1", "yr0_pre_PCR"]<- ""
#   model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)
#   model["1", "yr0_post_PCR"]<- model["10", "yr0_post_PCR"]- model["10", "yr0_pre_PCR"]
#   
#   for (i in (1:n_year) ){
#     model["1", period[i]]<- model["10", period[i]]- model["10", "yr0_pre_PCR"]
#   }
#   
#   model[,-c(1)]<-lapply(model[,-c(1)], as.numeric)       
#   model
#   
# }

