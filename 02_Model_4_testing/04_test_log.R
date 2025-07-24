library(log4r)
library(formattable)

my_logfile = "02_Model_4_testing/Test_logfile.txt"

my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", 
                           appenders= list(my_console_appender,my_file_appender))

log4r_info <- function() {
  log4r::info(my_logger, paste0("---------------------------------------------------------------------"))
  log4r::info(my_logger, paste0("Inputs/Assumptions used in this test\n"))

  log4r::info(my_logger, paste0("Commissioned UDA:  ", comma(commisioned_uda, digits=0)))
  log4r::info(my_logger, paste0("Commissioned spend:  £", comma(commisioned_spend, digits=0)))
  log4r::info(my_logger, paste0("Projected UDA:  ", comma(projected_uda, digits=0)))
  log4r::info(my_logger, paste0("Average £ per UDA:  ",avg_pay_UDA))
  log4r::info(my_logger, paste0("FY used for total UDA delivery:  ",FY))
  log4r::info(my_logger, paste0("Total UDA delivery in selected FY:  ",comma(total_uda, digits=0), "\n"))

  log4r::info(my_logger, paste0("Scenario tested in post-model:  ",Scenario_name))
  log4r::info(my_logger, get_cot_change())
  log4r::info(my_logger, get_pcr_change())
  log4r::info(my_logger, get_cost_change())

  log4r::info(my_logger, paste0("Behaviours changes applied to ",length(period), " years: "))
  log4r::info(my_logger, paste0("Assuming % new band 2/3 that are normal: ",label_percent()(normal_new_b23)))
  log4r::info(my_logger, paste0("Assuming % of unmodelled activities: ",label_percent()(`%_unmodelled_activity`)))
  log4r::info(my_logger, paste0("Assuming inflation from 2027/28: ",round(inflation,3), "\n"))
  
  log4r::info(my_logger, paste0("Please refer to [Input_for_test.xlsx] to view full list of inputs that have been used in this test.\n"))
  log4r::info(my_logger, paste0("Test end !!!! Outputs from this test is saved in [Model_outputs.xlsx]"))
}


log4r_info() 
