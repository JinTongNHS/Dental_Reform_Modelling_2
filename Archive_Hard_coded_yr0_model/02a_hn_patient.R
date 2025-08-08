
### HN patients charge ----
perc_exempt= no_hn_nopay/hn_patients #Patient exempt
hn_charge_wo_lab=get_pcr_fee("Band 2") #High needs patients charges without lab for both pathways
hn_charge_wt_lab = get_pcr_fee("Band 3") #High needs patients charges with lab for both pathways

path1_wo_lab= avg_pay_UDA*8 #payment for patients falling into pathway 1 without lab
path1_wt_lab=avg_pay_UDA*(8+12) #payment for patients falling into pathway 1 with lab
path2_wo_lab=avg_pay_UDA*20 #payment for patients falling into pathway 2 without lab
path2_wt_lab=avg_pay_UDA*(8+12+12) #payment for patients falling into pathway 2 with lab


### HN patients distribution by pathways ----
hn_distr<-function(case ="base"){ #scenario 3 base case
  if(case =="base"){
dist_path1_wo_lab= hn_pat_dist[1,2] # % patients falling into pathway 1 without lab
dist_path1_wt_lab= hn_pat_dist[2,2] # % patients falling into pathway 1 with lab
dist_path2_wo_lab= hn_pat_dist[1,3] # % patients falling into pathway 2 without lab
dist_path2_wt_lab= hn_pat_dist[2,3] # % patients falling into pathway 2 with lab
  
} else if(case=="doubling"){ #doubling percentage of unstable perio
  dist_path2_wo_lab= hn_pat_dist[1,3]*2 # % patients falling into pathway 2 without lab
  dist_path2_wt_lab= hn_pat_dist[2,3]*2 # % patients falling into pathway 2 with lab
  dist_path1_wo_lab= (hn_pat_dist[1,2]+hn_pat_dist[1,3])- dist_path2_wo_lab # % patients falling into pathway 1 without lab
  dist_path1_wt_lab= (hn_pat_dist[2,2]+hn_pat_dist[2,3])- dist_path2_wt_lab # % patients falling into pathway 1 with lab

  
  } else { #equal split with/without labs pathway
    dist_path2_wo_lab= (hn_pat_dist[1,3]+hn_pat_dist[2,3])/2 # % patients falling into pathway 2 without lab
    dist_path2_wt_lab= (hn_pat_dist[1,3]+hn_pat_dist[2,3])/2 # % patients falling into pathway 2 with lab
     dist_path1_wo_lab= 0.5- dist_path2_wo_lab # % patients falling into pathway 1 without lab
    dist_path1_wt_lab= 0.5 - dist_path2_wt_lab # % patients falling into pathway 1 with lab

  }
  
  v=c(dist_path1_wo_lab,dist_path1_wt_lab,dist_path2_wo_lab,dist_path2_wt_lab)
  v
}
#weighted average cost per high-needs patient
weighted_avg_hn_cost<-function(case="base"){
   v<- hn_distr(case)
   avg_hn_cost= v[1]*path1_wo_lab + v[2]*path1_wt_lab + v[3]*path2_wo_lab + v[4]*path2_wt_lab
   avg_hn_cost
  }
weighted_avg_hn_cost_exempt <-function(case="base"){
  v<- hn_distr(case)
  avg_hn_cost=(v[1]*hn_charge_wo_lab + v[2]*hn_charge_wt_lab + v[3]*hn_charge_wo_lab + v[4]*hn_charge_wt_lab)*(1-perc_exempt)
  avg_hn_cost
  }
#average cost per CoT for high-needs patients 
avg_cost_hn_pat = case_when(scenario %in% c(1,3,4,5,7,8)~ weighted_avg_hn_cost("base"),#(Proportion of patients in each pathway match those in pre 2006 data.)
                            scenario ==2~ weighted_avg_hn_cost("doubling"), #(Proportion of patients in with/without labs split the same as in pre 2006 data but higher rates of unstable perio disease (57%).)
                            scenario == 6~ weighted_avg_hn_cost("split")) #(Proportion of patients with/without perio same as in pre-2006 data but 50% of high caries patients needing laboratory restorations.)


#PCR per CoT for high-needs patients 
pcr_hn_pat_new= case_when(scenario %in% c(1,3,5,7,8)~ weighted_avg_hn_cost_exempt("base"),#(Proportion of patients in each pathway match those in pre 2006 data.)
                          scenario ==2~ weighted_avg_hn_cost_exempt("doubling"), #(Proportion of patients in with/without labs split the same as in pre 2006 data but higher rates of unstable perio disease (57%).)
                          scenario == 6~ weighted_avg_hn_cost_exempt("split"), #(Proportion of patients with/without perio same as in pre-2006 data but 50% of high caries patients needing laboratory restorations.)
                          scenario ==4~ NA)

