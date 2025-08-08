#Install all required packages before running the model - First time only!!!
#source("Install_packages.R")

source("DRM2/01_read_source_data.R")
source("DRM2/02_yr0_model.R")
source("DRM2/03_trend_model.R")

#write modelling outputs into a single excel file
dataset_names<- list( 'Test_time_stamp'=Sys.time()
                    ,'Year0_Pre_Change'=get_yr0_pre_model()
                     ,'Year0_Post_Change'=get_yr0_post_model()
                     ,'Trend_spend'=trend_spend()
                     ,'Trend_%_of_total_UDA'=trend("%_UDA")
                    ,'Real_spend_DoNothing'=trend_seg_spend(TRUE)
                    ,'Real_spend_PolicyChange'=trend_seg_spend(FALSE)
                    ,'Trend_COT'=trend("COT")
                    ,'Trend_PCR'=trend("PCR")
                     )
openxlsx::write.xlsx(dataset_names, file = paste0('DRM2/05_Model_outputs/Model_outputs_',format(Sys.time(), "%Y-%m-%d_%Hh%Mm"),'.xlsx')) 

#Write/Update log file
source("DRM2/04_test_log.R")