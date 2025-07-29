setwd("C:/Users/jin.tong/Documents/Rprojects/Dental_Reform_Modelling_2")

source("02_Model_4_testing/01_read_source_data.R")
source("02_Model_4_testing/02_yr0_model.R")
source("02_Model_4_testing/03_trend_model.R")

#write modelling outputs into a single excel file
dataset_names<- list( 'Test_time_stamp'=Sys.time()
                    ,'Year0_Pre_Change'=get_yr0_pre_model()
                     ,'Year0_Post_Change'=get_yr0_post_model()
                     ,'Trend_spend'=trend_spend()
                     ,'Trend_%_of_total_UDA'=`trend_%_UDA`()
                    ,'Real_spend_DoNothing'=trend_seg_spend("do nothing")
                    ,'Real_spend_PolicyChange'=trend_seg_spend("change")
                    ,'Trend_COT'=trend_COT()
                    ,'Trend_PCR'=trend_PCR()
                     )

openxlsx::write.xlsx(dataset_names, file = paste0('02_Model_4_testing/Model_outputs.xlsx')) 


source("02_Model_4_testing/04_test_log.R")