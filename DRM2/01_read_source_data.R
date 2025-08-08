#This file read all source data required in this modelling
### Different data sources are saved as individual tabs in [Model_inputs.xlsx] file, including:
# 1. [Assumptions] has a list of assumed values used in this model, all can be replaced. Please do not change the [id] column. 
# 2. [Patient_Segment] tab lists all patients segments covered in this modelling. Column [D] to [F] allow different inputs values to be tested for each segment. 
# 3. [Behaviour_Change] tab allows inputs to be tested for potential behaviour impacts
# 4. [Discount_index] can be replaced with latest published figures once available
# 5. [dental_stats_2c] & [dental_stats_6a] can be replaced with latest published figures once available

# All source data are extracted using defined functions below

library(tidyverse)
library(magrittr)
library(openxlsx)

#define function to extract individual assumptions
assumption<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "Assumptions", startRow = 1)
get_assumption<-function(Parameter_ID="1"){
  v=(assumption%>%filter(id==Parameter_ID))$Value
  v
}

FY=get_assumption(5) # Latest FY in Dental Stats

#define function to extract testing inputs for patient segments
seg_input<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "Patient_Segment", startRow = 3) #Using "All policies" scenario by default

#Excel [Scenario_tested.xlsx] contains all scenarios tested in orinigal excel model
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "All policies", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "HN perio", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "HN caries", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "FluorideVarnish", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "FissureSealants", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "Urgent Care", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "Everything else", startRow = 3)
#seg_input<-read.xlsx("Scenario_tested.xlsx", sheet = "All high needs", startRow = 3)

seg<-unique(seg_input$Seg_short)
#model<-unique(seg_input$Model)


#identify changes in input files - to be used in the log file to record policy changes being tested
get_policy_change<-function(){
 change=NULL
 
  for (s in seg){
    data<- seg_input%>%
    filter(Seg_short==s)
  
 change1= ifelse(data$COT[data$`Model`== "post-model"] !=data$COT[data$`Model`== "pre-model"], paste0("Impacts on [", s, "] segment, changing COT from ", round(data$COT[data$`Model`== "pre-model"],1), " to ", round(data$COT[data$`Model`== "post-model"],1), "; \n"), "\n")
 change2= ifelse(data$PCR_per_COT[data$`Model`== "post-model"] !=data$PCR_per_COT[data$`Model`== "pre-model"], paste0("Impacts on [", s, "] segment, changing PCR fee per COT from £", round(data$PCR_per_COT[data$`Model`== "pre-model"],1), " to £", round(data$PCR_per_COT[data$`Model`== "post-model"],1), "; \n"),"")
 change3= ifelse(data$Cost_per_COT[data$`Model`== "post-model"] !=data$Cost_per_COT[data$`Model`== "pre-model"], paste0("Impacts on [", s, "] segment, changing average cost per COT from £", round(data$Cost_per_COT[data$`Model`== "pre-model"],1), " to £", round(data$Cost_per_COT[data$`Model`== "post-model"],1), "; \n"), "")

 change<-paste(change, change1, change2, change3)
  }
 
 change
}


#define function to extract UDA for each segment from dental stats 
dental_stats_2c<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "dental_stats_2c", startRow = 5)

get_uda<-function(child=TRUE,b="Band.1"){
  if(child==TRUE){
    data<-dental_stats_2c %>% 
      filter(`Financial.Year`==FY, `Patient.Type`== "Child")%>%
      select(all_of(b))}
  else{data<-dental_stats_2c %>% 
    filter(`Financial.Year`==FY, `Patient.Type`!= "Child")%>%
    select(all_of(b))}
  
  v=sum(data)
  v
}


#define function to extract PCR total for each segment from dental stats 
dental_stats_6a<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "dental_stats_6a", startRow = 5)

get_pcr_total<-function( b=c("Band.1")){
  data<- dental_stats_6a %>% 
    filter(`Financial.Year`== FY)%>% #select latest FY 
    mutate(across(where(is.numeric), ~. *(1+as.numeric(get_assumption(10)))))%>% # uplift with any inflation
    select(all_of(b))
  
  v=sum(data)
  v
}


#define function to extract testing inputs for patient segments
behaviour_input<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "Behaviour_Change", startRow = 1)
get_behaviour<-function(name="yr0_return_b1"){
  trend_input<-behaviour_input%>%
    filter(short_name==name)%>%collect()
  
  trend_input
}

#read latest discount index values
discount_input<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "Discount_index", startRow = 1)

#define function to summarise behaviour impacts to be used in log file

