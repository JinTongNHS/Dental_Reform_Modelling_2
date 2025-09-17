# Dental_Reform_Modelling_2

This is a re-vamp of the dental contract reform model, which was done in excel originally. 
The model was designed to estimate the impact of reform on 1) payments to dentists; 2) Patient Charge Revenue; and 3) Number of patients seen. 

[DRM2] folder contains scripts for the core model which allows different inputs to be tested, so impacts of potential policy changes can be evaluated. 

Please following the steps below to run this model. 
1. Update/Add new input figures into model input file - [Model_input.xlsx]; Please make sure to select correct [Geographic level tested] and [Region] in the Assumption tab. 
2. Save edited input file into [DRM2] sub folder
3. Source [00_run_test.R]
4. Outputs of the model run will be saved as [Model_outputs_{Region}_{yyyy-mm-dd}_{hh}h{mm}m.xlsx] and updated [Test_logfile.txt] will be saved into subfolder [05_Model_outputs]

