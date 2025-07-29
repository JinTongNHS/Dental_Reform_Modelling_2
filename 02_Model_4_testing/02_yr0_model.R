library(magrittr)
library(scales)
library(tidyverse)

commisioned_uda= as.numeric(get_Var(1)) 
avg_pay_UDA= as.numeric(get_Var(2)) #average payment per UDA
commisioned_spend=commisioned_uda*avg_pay_UDA
projected_uda= as.numeric(get_Var(3))  #Projected UDAs
spend_projected_uda=projected_uda*avg_pay_UDA 
unused_uda= 1- as.numeric(get_Var(4)) #unused (%) UDA in 2024/25
FY=get_Var(5) # FY used for UDA figures from dental stats
total_uda<- get_uda(FY, "Child", "Total")+get_uda(FY, "non_Child", "Total")
Scenario_name<-(get_seg_input("post-model",1))$Comments #Get the short label/name for policy being tested in each run

get_yr0_pre_model<-function(){
  
m="pre-model"

b_23_name<-c("Band.2", "Band.2a", "Band.2b", "Band.2c", "Band.3") #a label for all band 2 &3

model<-rbind(get_seg_input(m,1), 
             get_seg_input(m,2), 
             get_seg_input(m,3), 
             get_seg_input(m,4), 
             get_seg_input(m,5), 
             get_seg_input(m,6), 
             get_seg_input(m,7), 
             get_seg_input(m,8),
             get_seg_input(m,9))%>%
  select(-`Comments`)%>%
  mutate(`Total_PCR`=`COT`*`PCR_per_COT`)%>%
  #Calculate % of total spend for each segment
  mutate(`Total_spend_%`=case_when(`Seg_short`== "new_b1"~ (`COT`/commisioned_uda),
                                   `Seg_short`== "new_b23"|`Seg_short`== "perio"|`Seg_short`== "new_hn_pat"~ (`COT`*`Cost_per_COT`/commisioned_spend),
                                   `Seg_short`== "child_b1"~ (get_uda(FY, "Child", "Band.1")/total_uda*(spend_projected_uda/commisioned_spend)),
                                   `Seg_short`== "child_b23"~ (get_uda(FY, "Child", b_23_name)/total_uda*(spend_projected_uda/commisioned_spend)),
                                   `Seg_short`== "urgent"~ ((get_uda(FY, "Child", "Urgent")+get_uda(FY, "non-Child", "Urgent"))/total_uda*(1-unused_uda)),
                                   `Seg_short`== "return_b23"~ ((get_uda(FY, "non-Child", b_23_name)+
                                                                         get_uda(FY, "non-Child", "Free")+
                                                                         get_uda(FY, "non-Child", "Regulation.11.Replacement.Appliance"))/total_uda
                                                                      *(spend_projected_uda/commisioned_spend)), #this still includes new band2&3, perio and new high needs patients
                                   `Seg_short`== "return_b1"~ (get_uda(FY, "non-Child", "Band.1")/total_uda*(spend_projected_uda/commisioned_spend) ) #this still includes new band 1
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
model[nrow(model) + 1, ] <- c(m ,"Unused_UDA","" ,"" ,"","","", unused_uda_post)

model<-model%>%mutate(`Total_spend_% (chr)`=label_percent()(as.numeric(model$`Total_spend_%`)))

model

}

get_yr0_post_model<-function(){
  
  m="post-model"
  b_23_name<-c("Band.2", "Band.2a", "Band.2b", "Band.2c", "Band.3") #a label for all band 2 &3
  
  model<-rbind(get_seg_input(m,1), 
               get_seg_input(m,2), 
               get_seg_input(m,3), 
               get_seg_input(m,4), 
               get_seg_input(m,5), 
               get_seg_input(m,6), 
               get_seg_input(m,7), 
               get_seg_input(m,8),
               get_seg_input(m,9))%>%
    select(-`Comments`)%>%
    mutate(`Total_PCR`=`COT`*`PCR_per_COT`)%>%
    left_join(subset(get_yr0_pre_model()%>%filter(Seg_short !="Unused_UDA"), select=c(Seg_short, `Total_spend_%`)), "Seg_short")%>%
    collect()

  #first check if a patient segment is affected - if yes, re-calculate total spend %, otherwise keep pre-model values
  for (s in (1:length(seg))){
    
    data<- seg_input%>%
      filter(Seg_short==seg[s])
    
    if(data$COT[data$`Model`== "post-model"] !=data$COT[data$`Model`== "pre-model"])
    {model$`Total_spend_%`[model$`Seg_short`== seg[s]]= (model$`COT`[model$`Seg_short`== seg[s]]* model$`Cost_per_COT`[model$`Seg_short`== seg[s]]/commisioned_spend)}
    else if(data$`Cost_per_COT`[data$`Model`== "post-model"] !=data$`Cost_per_COT`[data$`Model`== "pre-model"])   
      {model$`Total_spend_%`[model$`Seg_short`== seg[s]]= (model$`COT`[model$`Seg_short`== seg[s]]* model$`Cost_per_COT`[model$`Seg_short`== seg[s]]/commisioned_spend)}
    else {}
    
  }
  
  # data<- seg_input%>%
  #   filter(Seg_short=="urgent")
  # if(data$COT[data$`Model`== "post-model"] !=data$COT[data$`Model`== "pre-model"])
  # {model$`Total_spend_%`[model$`Seg_short`== "urgent"]= (model$`COT`[model$`Seg_short`== "urgent"]* model$`Cost_per_COT`[model$`Seg_short`== "urgent"]/commisioned_spend)}
  # else{}
  # 
  #   model$`Total_spend_%`[model$`Seg_short`== "perio"]= (model$`COT`[model$`Seg_short`== "perio"]* model$`Cost_per_COT`[model$`Seg_short`== "perio"]/commisioned_spend)
  #   model$`Total_spend_%`[model$`Seg_short`== "new_hn_pat"]= (model$`COT`[model$`Seg_short`== "new_hn_pat"]* model$`Cost_per_COT`[model$`Seg_short`== "new_hn_pat"]/commisioned_spend)
  #   
  
  unused_uda_post=1-sum(as.numeric(model$`Total_spend_%`), na.rm=T)
  model[nrow(model) + 1, ] <- c(m ,"Unused_UDA","" ,"" ,"","","", unused_uda_post)
  
  model<-model%>%mutate(`Total_spend_% (chr)`=label_percent()(as.numeric(model$`Total_spend_%`)))
  
  model
  
}
