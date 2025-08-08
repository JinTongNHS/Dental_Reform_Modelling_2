
# define scenario number 1 to 8
scenario_df<-data.frame("scenario_id"=c(1,2,3,4,5,6,7,8))
scenario_df<-scenario_df%>%mutate(Scenario_short=case_when(scenario_id==1~ "High needs perio",
                                                   scenario_id==2~ "High needs caries",
                                                   scenario_id==3~ "Fluoride varnish do not use",
                                                   scenario_id==4~ "Fissuresealants do not use",
                                                   scenario_id==5~ "Urgent care",
                                                   scenario_id==6~ "Everything else",
                                                   scenario_id==7~ "All policies",
                                                   scenario_id==8~ "All high needs") )

scenario=scenario_df$scenario_id

source("Archive_Hard_coded_yr0_model/02a_hn_patient.R")
source("Archive_Hard_coded_yr0_model/02b_perio_&_urgent.R")


post_model<-function(s=1){
  post_model<-pre_model%>%select(-"Total_spend_% (chr)")
  scenario_short=filter(scenario_df, `scenario_id`==s)$Scenario_short
  
  if(s == 1){
    
    #COT remains the same post contract change
    #average cost per COT changed to counting total uda for 2 band 2a and 1 band 1
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "perio"]<- (avg_uda_urgent(2,1)*avg_pay_UDA) 
    #Post change PCR per COT remain the same
    # %total spend changed to reflect changes in cost per COT
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "perio"]=  (post_model$`COT`[post_model$`Patient_Segment`== "perio"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "perio"]/commisioned_spend)
    #Post change total PCR remain the same
    
    }
  else if(s == 2){
    #Contract change: to match all proportion fo patients in each pathway with pre 2006 data
    #COT remains the same post contract change
    #Post change cost per COT changed 
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]= weighted_avg_hn_cost("base") #(Proportion of patients in each pathway match those in pre 2006 data.)
    #Post change PCR per COT changed
    post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]= weighted_avg_hn_cost_exempt("base")
    #post change total spend changed
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "new_hn_pat"]= (post_model$`COT`[post_model$`Patient_Segment`== "new_hn_pat"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]/commisioned_spend)
    #Post change total PCR changed
    post_model$`Total_PCR`[post_model$`Patient_Segment`== "new_hn_pat"]=(post_model$`COT`[post_model$`Patient_Segment`== "new_hn_pat"]* post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"])
  }
  else if(s== 5){
    #Post change COT added additional 700k urgent care
    post_model$`COT`[post_model$`Patient_Segment`== "urgent"]<- (pre_model$`COT`[pre_model$`Patient_Segment`== "urgent"]+ add_urgent_patients)
    #Post change cost per COT changed
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "urgent"]<- as.numeric(get_Var(18)) #p18 - average cost per COT for urgent care post intervention
    #Post change PCR per COT remain the same
    #post change total spend changed
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "urgent"]= (post_model$`COT`[post_model$`Patient_Segment`== "urgent"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "urgent"]/commisioned_spend)
    #Post change total PCR changed
    post_model$`Total_PCR`[post_model$`Patient_Segment`== "urgent"]=(post_model$`COT`[post_model$`Patient_Segment`== "urgent"]* post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "urgent"])
    
  }
  else if(s == 7){
    post_model$`COT`[post_model$`Patient_Segment`== "urgent"]<- (pre_model$`COT`[pre_model$`Patient_Segment`== "urgent"]+ add_urgent_patients)
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "perio"]<- (avg_uda_urgent(2,1)*avg_pay_UDA) 
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]= weighted_avg_hn_cost("base") #(Proportion of patients in each pathway match those in pre 2006 data.)
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "urgent"]<- as.numeric(get_Var(18)) #p18 - average cost per COT for urgent care post intervention
    post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]= weighted_avg_hn_cost_exempt("base")
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "perio"]=  (post_model$`COT`[post_model$`Patient_Segment`== "perio"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "perio"]/commisioned_spend)
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "new_hn_pat"]= (post_model$`COT`[post_model$`Patient_Segment`== "new_hn_pat"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]/commisioned_spend)
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "urgent"]= (post_model$`COT`[post_model$`Patient_Segment`== "urgent"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "urgent"]/commisioned_spend)
    post_model$`Total_PCR`[post_model$`Patient_Segment`== "urgent"]=(post_model$`COT`[post_model$`Patient_Segment`== "urgent"]* post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "urgent"])
    post_model$`Total_PCR`[post_model$`Patient_Segment`== "new_hn_pat"]=(post_model$`COT`[post_model$`Patient_Segment`== "new_hn_pat"]* post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"])
    
  }
  else if(s == 8){
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "perio"]<- (avg_uda_urgent(2,1)*avg_pay_UDA) 
    post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]= weighted_avg_hn_cost("base") #(Proportion of patients in each pathway match those in pre 2006 data.)
    post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]= weighted_avg_hn_cost_exempt("base")
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "perio"]=  (post_model$`COT`[post_model$`Patient_Segment`== "perio"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "perio"]/commisioned_spend)
    post_model$`Total_spend_%`[post_model$`Patient_Segment`== "new_hn_pat"]= (post_model$`COT`[post_model$`Patient_Segment`== "new_hn_pat"]* post_model$`Average_cost_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"]/commisioned_spend)
    post_model$`Total_PCR`[post_model$`Patient_Segment`== "new_hn_pat"]=(post_model$`COT`[post_model$`Patient_Segment`== "new_hn_pat"]* post_model$`PCR_per_COT`[post_model$`Patient_Segment`== "new_hn_pat"])
    
  }
  else {
    #COT remains the same post contract change
    #Post change cost per COT remain the same
    #Post change PCR per COT remain the same
    #post change total spend remain the same
    #Post change total PCR remain the same
  }
  
  unused_uda=1-sum(post_model$`Total_spend_%`)
  post_model[nrow(post_model) + 1, ] <- c("Unused_UDA","" ,"","" ,"",unused_uda)
  
  post_model<-post_model%>%mutate(Scenario=scenario_short)
  
  post_model
  
}

#write modelling outputs into a single excel file
dataset_names<- list('Year0_Pre_Change'=pre_model
                     ,'Year0_Post_Change_1'=post_model(1)
                     ,'Year0_Post_Change_2'=post_model(2)
                     ,'Year0_Post_Change_3'=post_model(3)
                     ,'Year0_Post_Change_4'=post_model(4)
                     ,'Year0_Post_Change_5'=post_model(5)
                     ,'Year0_Post_Change_6'=post_model(6)
                     ,'Year0_Post_Change_7'=post_model(7)
                     ,'Year0_Post_Change_8'=post_model(8)
)


openxlsx::write.xlsx(dataset_names, file = paste0('Archive_Hard_coded_yr0_model/Model_outputs_fixed.xlsx')) 
