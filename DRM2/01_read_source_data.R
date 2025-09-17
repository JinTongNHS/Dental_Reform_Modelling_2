#This file read all source data required in this modelling
### Different data sources are saved as individual tabs in [Model_inputs.xlsx] file, including:
# 1. [Assumptions] has a list of assumed values used in this model, all can be replaced. Please do not change the [id] column. 
# 2. [Patient_Segment] tab lists all patients segments covered in this modelling. Column [D] to [F] allow different inputs values to be tested for each segment. 
# 3. [Behaviour_Change] tab allows inputs to be tested for potential behaviour impacts
# 4. [Discount_index] can be replaced with latest published figures once available

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
seg_input<-seg_input%>%filter(`Region`==get_assumption(100))


seg<-unique(seg_input$Seg_short)


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



#define function to extract testing inputs for patient segments
behaviour_input<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "Behaviour_Change", startRow = 1)
get_behaviour<-function(name="yr0_return_b1"){
  trend_input<-behaviour_input%>%
    filter(short_name==name)%>%collect()
  
  trend_input
}

#read latest discount index values
discount_input<-read.xlsx("DRM2/Model_inputs.xlsx", sheet = "Discount_index", startRow = 1)


