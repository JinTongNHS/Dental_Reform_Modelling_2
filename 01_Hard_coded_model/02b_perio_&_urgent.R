

####For new periodontal diagnosis ----
inc_perio_million= as.numeric(get_Var(15))  # p15- incidence of periodontal disease (millions)
prevalence= as.numeric(get_Var(16)) # p16 - prevalence

perc_prevalence=case_when(scenario ==1~ inc_perio_million/prevalence,#incidence of periodontal disease (millions) divided by prevalence
                          scenario %in% c(2,3,5, 6,7,8)~ inc_perio_million/prevalence*3,
                          scenario == 4~ inc_perio_million/prevalence*4) # Scenario1 is 6.5%, scenario 2, 3, 5,6,7,8 are all 3 times and scenario 4 is 4 times


#### For urgent care patients ----

#average uda per COT for urgent care patients
avg_uda_urgent<-function(no_b2a=1, no_b1=1){ 
  avg_uda_b2a=as.numeric(get_Var(7))
  avg_uda_b1 = as.numeric(get_Var(4))

  total_uda = no_b2a*avg_uda_b2a+no_b1*avg_uda_b1
  total_uda
}

#average cost per CoT for urgent care post intervention
avg_cost_urgent = case_when(scenario %in% c(1,3,5,7,8)~ avg_cost_per_cot("urgent"),#(Proportion of patients in each pathway match those in pre 2006 data.)
                            scenario ==2~ NA)
