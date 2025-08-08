library(magrittr)
library(scales)
library(tidyverse)

commisioned_uda= as.numeric(get_assumption(1)) 
avg_pay_UDA= as.numeric(get_assumption(2)) #average payment per UDA
commisioned_spend=commisioned_uda*avg_pay_UDA
projected_uda= as.numeric(get_assumption(3))  #Projected UDAs
spend_projected_uda=projected_uda*avg_pay_UDA 
unused_uda= 1- as.numeric(get_assumption(4)) #unused (%) UDA in 2024/25
total_uda<- get_uda(TRUE, "Total")+get_uda(FALSE, "Total")
Scenario_name<-seg_input[nrow(seg_input), ncol(seg_input)] #Get the short label/name for policy being tested in each run
b_23_name<-c("Band.2", "Band.2a", "Band.2b", "Band.2c", "Band.3") #a label for all band 2 &3

get_yr0_pre_model<-function(){
  
model<-seg_input%>%
  filter(Model=="pre-model")%>%
  select(-`Comments`)%>%
  mutate(`Total_PCR`=`COT`*`PCR_per_COT`)%>%
  #Calculate % of total spend for each segment
  mutate(`Total_spend_%`=case_when(`Seg_short`== "new_b1"~ (`COT`/commisioned_uda),
                                   `Seg_short`== "new_b23"|`Seg_short`== "perio"|`Seg_short`== "new_hn_pat"~ (`COT`*`Cost_per_COT`/commisioned_spend),
                                   `Seg_short`== "child_b1"~ (get_uda(TRUE, "Band.1")/total_uda*(spend_projected_uda/commisioned_spend)),
                                   `Seg_short`== "child_b23"~ (get_uda( TRUE, b_23_name)/total_uda*(spend_projected_uda/commisioned_spend)),
                                   `Seg_short`== "urgent"~ ((get_uda(TRUE, "Urgent")+get_uda(FALSE, "Urgent"))/total_uda*(1-unused_uda)),
                                   `Seg_short`== "return_b23"~ ((get_uda(FALSE, b_23_name)+
                                                                         get_uda(FALSE, "Free")+
                                                                         get_uda(FALSE, "Regulation.11.Replacement.Appliance"))/total_uda
                                                                      *(spend_projected_uda/commisioned_spend)), #this still includes new band2&3, perio and new high needs patients
                                   `Seg_short`== "return_b1"~ (get_uda(FALSE, "Band.1")/total_uda*(spend_projected_uda/commisioned_spend) ) #this still includes new band 1
  ))%>%
  collect()

#excluding new patients segs in returning ones
model$`Total_spend_%`[model$`Seg_short`== "return_b1"]=  (model$`Total_spend_%`[model$`Seg_short`== "return_b1"]- model$`Total_spend_%`[model$`Seg_short`== "new_b1"])
model$`Total_spend_%`[model$`Seg_short`== "return_b23"]=  (model$`Total_spend_%`[model$`Seg_short`== "return_b23"]
                                                           - model$`Total_spend_%`[model$`Seg_short`== "new_b23"]
                                                           - model$`Total_spend_%`[model$`Seg_short`== "new_hn_pat"]
                                                           - model$`Total_spend_%`[model$`Seg_short`== "perio"])


model$`Total_PCR`[model$`Seg_short`== "return_b1"]=   (get_pcr_total("Band.1")- model$`Total_PCR`[model$`Seg_short`== "new_b1"])
model$`Total_PCR`[model$`Seg_short`== "return_b23"]=  (get_pcr_total(b_23_name)
                                                                     +get_pcr_total("Regulation.11.Replacement.Appliance")
                                                                     - model$`Total_PCR`[model$`Seg_short`== "new_b23"]
                                                                     - model$`Total_PCR`[model$`Seg_short`== "new_hn_pat"]
                                                                     - model$`Total_PCR`[model$`Seg_short`== "perio"])



unused_uda_post=1-sum(model$`Total_spend_%`, na.rm=T)
model[nrow(model) + 1, ] <- c("pre-model" ,"Unused_UDA","" ,"" ,"","","", unused_uda_post)

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
  model[nrow(model) + 1, ] <- c("post-model","Unused_UDA","" ,"" ,"","","", unused_uda_post)
  
  model<-model%>%mutate(`Total_spend_% (chr)`=label_percent()(as.numeric(model$`Total_spend_%`)))
  
  model
  
}
