library(magrittr)
library(scales)
library(tidyverse)

commisioned_uda= as.numeric(get_assumption(1)) 
avg_pay_UDA= as.numeric(get_assumption(2)) #average payment per UDA
commisioned_spend=commisioned_uda*avg_pay_UDA
projected_uda= as.numeric(get_assumption(3))  #Projected UDAs
spend_projected_uda=projected_uda*avg_pay_UDA 
unused_uda= 1- as.numeric(get_assumption(4)) #unused (%) UDA in 2024/25
#total_uda<- get_uda(TRUE, "total")+get_uda(FALSE, "total")
geo_level<-ifelse(get_assumption(100)=="England", paste0(get_assumption(99), " level"), paste0("Regional level - ",get_assumption(100)))
Scenario_name<-seg_input[nrow(seg_input), ncol(seg_input)-1] #Get the short label/name for policy being tested in each run
b_23_name<-c("band.2", "band.2a", "band.2b", "band.2c", "band.3") #a label for all band 2 &3

get_yr0_pre_model<-function(){
  
model<-seg_input%>%
  filter(Model=="pre-model")%>%
  select(-`Comments`)%>%
  mutate(`Total_PCR`=`COT`*`PCR_per_COT`)%>%
  mutate(`Total_spend_%`=`COT`*`Cost_per_COT`/commisioned_spend)%>%   #Calculate % of total spend and Total PCR for each segment using the simple method - The complicated one used in orinigal model has been commented out
  collect()
  
#   Original method of calculating Total_spend_% below 
#   mutate(`Total_spend_%`=case_when(`Seg_short`== "new_b1"~ (`COT`/commisioned_uda),
#                                    `Seg_short`== "new_b23"|`Seg_short`== "perio"|`Seg_short`== "new_hn_pat"~ (`COT`*`Cost_per_COT`/commisioned_spend),
#                                    `Seg_short`== "child_b1"~ (get_uda(TRUE, "band.1")/total_uda*(spend_projected_uda/commisioned_spend)),
#                                    `Seg_short`== "child_b23"~ (get_uda( TRUE, b_23_name)/total_uda*(spend_projected_uda/commisioned_spend)),
#                                    `Seg_short`== "urgent"~ ((get_uda(TRUE, "urgent")+get_uda(FALSE, "urgent"))/total_uda*(1-unused_uda)),
#                                    `Seg_short`== "return_b23"~ ((get_uda(FALSE, b_23_name)+
#                                                                          get_uda(FALSE, "free")+
#                                                                          get_uda(FALSE, "regulation.11.replacement.appliance"))/total_uda
#                                                                       *(spend_projected_uda/commisioned_spend)), #this still includes new band2&3, perio and new high needs patients
#                                    `Seg_short`== "return_b1"~ (get_uda(FALSE, "band.1")/total_uda*(spend_projected_uda/commisioned_spend) ),  #this still includes new band 1
#                                    TRUE ~ (`COT`*`Cost_per_COT`/commisioned_spend) #for any potential new segment
#   ))%>%
#   collect()
# 
# #excluding new patients segs in returning ones
# model$`Total_spend_%`[model$`Seg_short`== "return_b1"]=  (model$`Total_spend_%`[model$`Seg_short`== "return_b1"]- model$`Total_spend_%`[model$`Seg_short`== "new_b1"])
# model$`Total_spend_%`[model$`Seg_short`== "return_b23"]=  (model$`Total_spend_%`[model$`Seg_short`== "return_b23"]
#                                                            - model$`Total_spend_%`[model$`Seg_short`== "new_b23"]
#                                                            - model$`Total_spend_%`[model$`Seg_short`== "new_hn_pat"]
#                                                            - model$`Total_spend_%`[model$`Seg_short`== "perio"])


# model$`Total_PCR`[model$`Seg_short`== "return_b1"]= (get_pcr_total("band.1")- model$`Total_PCR`[model$`Seg_short`== "new_b1"])
# model$`Total_PCR`[model$`Seg_short`== "return_b23"]=  (get_pcr_total(b_23_name)
#                                                                      +get_pcr_total("regulation.11.replacement.appliance")
#                                                                      - model$`Total_PCR`[model$`Seg_short`== "new_b23"]
#                                                                      - model$`Total_PCR`[model$`Seg_short`== "new_hn_pat"]
#                                                                      - model$`Total_PCR`[model$`Seg_short`== "perio"])



unused_uda_post=1-sum(model$`Total_spend_%`, na.rm=T)
model[nrow(model) + 1, ] <- c("pre-model" ,"Unused_UDA","" ,"" ,"","","","", unused_uda_post)

model<-model%>%mutate(`Total_spend_% (chr)`=label_percent()(as.numeric(model$`Total_spend_%`)))

model

}

get_yr0_post_model<-function(){

  model<-seg_input%>%
    filter(Model=="post-model")%>%
    select(-`Comments`)%>%
    #mutate(`Total_PCR`=`COT`*`PCR_per_COT`)%>%
    left_join(subset(get_yr0_pre_model()%>%filter(Seg_short !="Unused_UDA"), select=c(Seg_short, `Total_PCR`,`Total_spend_%`)), "Seg_short")%>%
    collect()

  #first check if a patient segment is affected - if yes, re-calculate total spend %, otherwise keep pre-model values
  for (s in seg){
    
    data<- seg_input%>%
      filter(Seg_short==s)
    
    if(data$COT[data$`Model`== "post-model"] !=data$COT[data$`Model`== "pre-model"])
    {model$`Total_spend_%`[model$`Seg_short`== s]= (model$`COT`[model$`Seg_short`== s]* model$`Cost_per_COT`[model$`Seg_short`== s]/commisioned_spend)
     model$`Total_PCR`[model$`Seg_short`== s]= model$`COT`[model$`Seg_short`== s]* model$`PCR_per_COT`[model$`Seg_short`== s]}
    else if(data$`Cost_per_COT`[data$`Model`== "post-model"] !=data$`Cost_per_COT`[data$`Model`== "pre-model"])   
      {model$`Total_spend_%`[model$`Seg_short`== s]= (model$`COT`[model$`Seg_short`== s]* model$`Cost_per_COT`[model$`Seg_short`== s]/commisioned_spend)}
    else if(data$`PCR_per_COT`[data$`Model`== "post-model"] !=data$`PCR_per_COT`[data$`Model`== "pre-model"])   
    {model$`Total_PCR`[model$`Seg_short`== s]= model$`COT`[model$`Seg_short`== s]* model$`PCR_per_COT`[model$`Seg_short`== s]}
    else {}
  }
 
  unused_uda_post=1-sum(as.numeric(model$`Total_spend_%`), na.rm=T)
  model[nrow(model) + 1, ] <- c("post-model","Unused_UDA","" ,"" ,"","","","", unused_uda_post)
  
  model<-model%>%mutate(`Total_spend_% (chr)`=label_percent()(as.numeric(model$`Total_spend_%`)))
  
  model
  
}
