library(magrittr)
library(scales)

source("Archive_Hard_coded_yr0_model/01_read_source_data.R")

#define patient segments
seg<-c("new_b1","new_b23", "new_hn_pat", "return_b23","perio", "return_b1", "child_b1","child_b23", "urgent")
avg_pay_UDA= as.numeric(get_Var(3)) #p3- average payment per UDA
b_23_name<-c("Band.2", "Band.2a", "Band.2b", "Band.2c", "Band.3") #a label for all band 2 &3

### Additional urgent care ----
add_urgent_patients = as.numeric(get_Var(2)) #p2- no. additional patients receiving urgent care

### HN patients charge ----
hn_patients = as.numeric(get_Var(5)) # p5- total number of existing high needs patients
hn_uda = as.numeric(get_Var(6)) # p6- total UDAs of existing high needs patients
pcr_uplift=as.numeric(get_Var(11)) #p11 - 4% PCR uplift on 1st April 2024
pcr_hn_pat_2324=as.numeric(get_Var(12)) #p12 - PCR to high needs patients in 23/24
no_hn_nopay=as.numeric(get_Var(13)) #p13- no. patients who do not need to pay in 23/24

### New periodontal diagnosis ----
inc_perio_million= as.numeric(get_Var(15))  # p15- incidence of periodontal disease (millions)
prevalence= as.numeric(get_Var(16)) # p16 - prevalence

### Commissioned UDAs ----
Last_month = max(smt_pack$calendar_month) # latest month available in SMT pack data
commisioned_uda= (smt_pack%>%filter(calendar_month==Last_month))$UDAs_annual_contracted
commisioned_spend=commisioned_uda*avg_pay_UDA
unused_uda= 1- as.numeric(get_Var(21)) #p21 - unused (%) UDA in 2024/25

### Projected UDAs ----
projected_uda= sum((uda_projection%>%filter(geography_name=="England"))[-c(1)])
spend_projected_uda=projected_uda*avg_pay_UDA ##???? why this figure does not match ???

cot<-function(seg=seg[1]){
  ### new band 1
  if(seg=="new_b1"){
    cot= sum(subset(t1,select=c(`adult_NPP_band_1`)))/8*12  #number of courses of treatment new patients band 1
  } 
  ### new band 2&3
  else if(seg=="new_b23"){
    
    hn_patients = as.numeric(get_Var(5)) # p5- total number of existing high needs patients
    cot= sum(subset(t1,select=c(`adult_NPP_band_23`)))/8*12-hn_patients  #number of courses of treatment new patients normal  band 2/3 
  } 
  ### returning band 1
  else if(seg=="return_b1"){
    b1_all = get_cot("Non-child", "Band.1")
    cot_npp_b1=sum(subset(t1,select=c(`adult_NPP_band_1`)))/8*12  #number of courses of treatment new patients band 1
    cot= b1_all - cot_npp_b1 #no.  courses of treatment returning patients Band 1 (not matching yet??)
  }
  ### returning band 2&3
  else if(seg=="return_b23"){
    hn_patients = as.numeric(get_Var(5)) # p5- total number of existing high needs patients
    cot_npp_b23=sum(subset(t1,select=c(`adult_NPP_band_23`)))/8*12-hn_patients  #number of courses of treatment new patients normal  band 2/3
    
    cot= sum(subset(dental_stats_1c %>% filter( `Patient.Type`!= "Child"), select=c( `Band.2a`, `Band.2b`, `Band.2c`, `Band.3`))) - cot_npp_b23 - hn_patients- cot("perio")#no. courses of treatment returning patients Band 2/3 
  }
  ### new high needs patients
  else if(seg=="new_hn_pat"){
    cot= hn_patients # p5- total number of existing high needs patients
  } 
  ### new perio patients
  else if(seg=="perio"){
    uk_adult=as.numeric(get_Var(14)) # p14- UK adults
    total_pat_seen_2324=as.numeric(get_Var(17)) #p17- total adult population seen in 2023/24 NHS dental stats (BSA publication)
    
    perc_adult=prevalence/(uk_adult/1000000) # prevalence as percetage adult population
    perio_pat = total_pat_seen_2324*perc_adult #Perio prevalence number of patients
    cot = perio_pat*(inc_perio_million/prevalence)*3 #High needs perio patients currently seen
  } 
  ### child non_urgent band 1
  else if(seg=="child_b1"){

    cot_b1_child=get_cot("Child", "Band.1") #no. courses of treatment Children Band 1
    cot= cot_b1_child
    
  }
  ### child non_urgent band 2&3
  else if(seg=="child_b23"){
    cot_b23_child= get_cot("Child", b_23_name) #no.courses of treatment  Children Band 2/3
    cot= cot_b23_child
    
  }
  ### urgent care patients 
  else if(seg=="urgent"){
    t<-smt_pack%>%filter(calendar_month <= "2024-03-01" & calendar_month >= "2023-04-01")
    cot= sum(subset(t, select=c(`CoT_urgent_delivered_incl_FD`))) #This include both child and adults
    
  }
  else{}
  
}

pcr_per_cot<-function(seg=seg[1]){
  ### all band 1
  if(seg=="new_b1"|seg=="return_b1"){
  prc_b1_fee = get_pcr_fee("Band 1") #24/25 charges
    pcr_b1_non_exempt = get_cot("Non-Exempt", "Band.1")
    b1_all = get_cot("Non-child", "Band.1")
    pcr_per_cot= (pcr_b1_non_exempt*prc_b1_fee)/b1_all #PCR per CoT for Band1 = PCR band1 charge multiplied by number of non-exempt band 1 patients then divided by total number of exempt and non-exempt patients together in 23/24
  } 
  ### all band 2&3
  else if(seg=="new_b23"|seg=="return_b23"){
    pcr_b2_fee=get_pcr_fee("Band 2")
    pcr_b2_non_exempt = get_cot("Non-Exempt", c("Band.2", "Band.2a", "Band.2b", "Band.2c"))
    pcr_b3_fee = get_pcr_fee("Band 3")
    pcr_b3_non_exempt = get_cot("Non-Exempt", "Band.3")
    b2_3_all= get_cot("Non-child", b_23_name)
    pcr_per_cot=(pcr_b2_non_exempt*pcr_b2_fee+pcr_b3_non_exempt*pcr_b3_fee)/b2_3_all #PCR per CoT for Band2/3
  } 
  ### new high needs patients
  else if(seg=="new_hn_pat"){
      pcr_per_cot = pcr_hn_pat_2324/ hn_patients*(1+pcr_uplift) #PCR per CoT for existing high-needs patients = PCR to high needs patients divided by number of patients adjusted with 4% PCR uplift on 1st April 2024
    
  } 
  ### new perio patients
  else if(seg=="perio"){
    prc_b2a_fee = get_pcr_fee("Band 2") #24/25 charges
    pcr_b2a_non_exempt = get_cot("Non-Exempt", "Band.2a")
    b2a_all = get_cot("Non-child", "Band.2a")
    pcr_per_cot= (pcr_b2a_non_exempt*prc_b2a_fee)/b2a_all #PCR per CoT for Band2a
  } 
  ### child non_urgent band 1
  else if(seg=="child_b1"){
    pcr_per_cot=""
  }
  ### child non_urgent band 2&3
  else if(seg=="child_b23"){
    pcr_per_cot=""
  }
  ### urgent care patients 
  else if(seg=="urgent"){
    
    prc_urgent_fee = get_pcr_fee("urgent") #24/25 charges
    pcr_urgent_non_exempt = get_cot("Non-Exempt", "Urgent")
    urgent_all = sum(subset(dental_stats_1c, select=c(`Urgent`))) #This include both child and adults
    pcr_per_cot= (pcr_urgent_non_exempt*prc_urgent_fee)/urgent_all #PCR per CoT for urgent care
    
  }
  else{}
  pcr_per_cot
}

avg_cost_per_cot<-function(seg=seg[1]){
  ### new band 1
  if(seg=="new_b1"|seg=="return_b1"){
   
    avg_uda_b1=as.numeric(get_Var(4)) #p4- average UDA per CoT for Band1
    avg_cost_COT= avg_uda_b1*avg_pay_UDA
    
  } 
  ### new band 2&3
  else if(seg=="new_b23"|seg=="return_b23"){
    avg_uda_b2_3= get_uda("2023/2024", "non_Child", b_23_name)/get_cot("non_child", b_23_name) #average UDA per CoT for Band2/3
    avg_cost_COT= avg_uda_b2_3*avg_pay_UDA
  } 
  ### new high needs patients
  else if(seg=="new_hn_pat"){
   avg_uda_hn_pat = hn_uda/hn_patients #average UDA per CoT for existing high-needs patients
   avg_cost_COT= avg_uda_hn_pat*avg_pay_UDA 
  } 
  ### new perio patients
  else if(seg=="perio"){
    avg_uda_b2a=as.numeric(get_Var(7))
    avg_cost_COT=avg_uda_b2a*avg_pay_UDA 
  } 
  ### child non_urgent band 1
  else if(seg=="child_b1"){
    avg_uda_b1_childPre=as.numeric(get_Var(8)) #p8- average UDA per CoT for Band 1 children pre
    avg_cost_COT= avg_uda_b1_childPre*avg_pay_UDA 
  }
  ### child non_urgent band 2&3
  else if(seg=="child_b23"){
    avg_uda_b2_3_child=get_uda("2023/2024", "Child", b_23_name)/cot("child_b23") #average UDA per CoT for Band 2/3 children
    avg_cost_COT= avg_uda_b2_3_child*avg_pay_UDA 
  }
  ### urgent care patients 
  else if(seg=="urgent"){
 
    avg_uda_urgent=as.numeric(get_Var(10)) #p10- average UDA per CoT for urgent care
    avg_cost_COT=avg_uda_urgent*avg_pay_UDA 
  }
  else{}
  
  avg_cost_COT
}


#### Pre Contract change model ----
  df<- function(i=1){data.frame("Patient_Segment"=seg[i],
                  "COT" = as.numeric(cot(seg[i])),
                  "PCR_per_COT" = as.numeric(pcr_per_cot(seg[i])), 
                  "Average_cost_per_COT" = as.numeric(avg_cost_per_cot(seg[i])) )}
  
  pre_model<-rbind(df(1), df(2), df(3), df(4), df(5), df(6), df(7), df(8),df(9))%>%
    mutate(`Total_PCR`=`COT`*`PCR_per_COT`)%>%
   #Calculate % of total spend for each segment
    mutate(`Total_spend_%`=case_when(`Patient_Segment`== "new_b1"~ (`COT`/commisioned_uda),
                                      `Patient_Segment`== "new_b23"|`Patient_Segment`== "perio"|`Patient_Segment`== "new_hn_pat"~ (`COT`*`Average_cost_per_COT`/commisioned_spend),
                                      `Patient_Segment`== "child_b1"~ (get_uda("2023/2024", "Child", "Band.1")/total_uda*(spend_projected_uda/commisioned_spend)),
                                      `Patient_Segment`== "child_b23"~ (get_uda("2023/2024", "Child", b_23_name)/total_uda*(spend_projected_uda/commisioned_spend)),
                                      `Patient_Segment`== "urgent"~ ((get_uda("2023/2024", "Child", "Urgent")+get_uda("2023/2024", "non-Child", "Urgent"))/total_uda*(1-unused_uda)),
                                     `Patient_Segment`== "return_b23"~ ((get_uda("2023/2024", "non-Child", b_23_name)+
                                                                          get_uda("2023/2024", "non-Child", "Free")+
                                                                          get_uda("2023/2024", "non-Child", "Regulation.11.Replacement.Appliance"))/total_uda
                                                                        *(spend_projected_uda/commisioned_spend)), #this still includes new band2&3, perio and new high needs patients
                                     `Patient_Segment`== "return_b1"~ (get_uda("2023/2024", "non-Child", "Band.1")/total_uda*(spend_projected_uda/commisioned_spend) ) #this still includes new band 1
                                     ))%>%
    collect()
  
  #excluding new patients segs in returning ones
  pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "return_b1"]=  (pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "return_b1"]- pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "new_b1"])
  pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "return_b23"]=  (pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "return_b23"]
                                                         - pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "new_b23"]
                                                         - pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "new_hn_pat"]
                                                         - pre_model$`Total_spend_%`[pre_model$`Patient_Segment`== "perio"])
  
  
  pre_model$`Total_PCR`[pre_model$`Patient_Segment`== "return_b1"]=   (get_pcr_total("Band.1")- pre_model$`Total_PCR`[pre_model$`Patient_Segment`== "new_b1"])
  pre_model$`Total_PCR`[pre_model$`Patient_Segment`== "return_b23"]=  (get_pcr_total(b_23_name)
                                                         +get_pcr_total("Regulation.11.Replacement.Appliance")
                                                         - pre_model$`Total_PCR`[pre_model$`Patient_Segment`== "new_b23"]
                                                         - pre_model$`Total_PCR`[pre_model$`Patient_Segment`== "new_hn_pat"]
                                                         - pre_model$`Total_PCR`[pre_model$`Patient_Segment`== "perio"])
  
  
  pre_model<-pre_model%>%mutate(`Total_spend_% (chr)`=label_percent()(pre_model$`Total_spend_%`))
  
  pre_model_Seg<-function(seg="new_b1"){
    seg_output<-pre_model%>%filter(`Patient_Segment`== seg)
    seg_output
  }
  
  View(pre_model)
  View( pre_model_Seg("new_b1"))

