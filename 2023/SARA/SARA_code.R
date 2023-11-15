
 report << "SARA file for Angie Greig" << endl;

 report << "ATF        # stock  " << endl;
 report << "BSAI       # region     (AI AK BOG BSAI EBS GOA SEO WCWYK)" << endl;
 report << "2013       # ASSESS_YEAR - year assessment is presented to the SSC" << endl;
 report << "3a         # TIER  (1a 1b 2a 2b 3a 3b 4 5 6) " << endl;
 report << "none       # TIER2  if mixed (none 1a 1b 2a 2b 3a 3b 4 5 6)" << endl;
 report << "partial    # UPDATE (new benchmark full partial)" << endl;
 report << "2          # LIFE_HIST - SAIP ratings (0 1 2 3 4 5)" << endl;
 report << "2          # ASSES_FREQ - SAIP ratings (0 1 2 3 4 5) " << endl;
 report << "5          # ASSES_LEV - SAIP ratings (0 1 2 3 4 5)" << endl;
 report << "5          # CATCH_DAT - SAIP ratings (0 1 2 3 4 5) " << endl;
 report << "3          # ABUND_DAT - SAIP ratings (0 1 2 3 4 5)" << endl;
 report << "567000     # Minimum B  Lower 95% confidence interval for spawning biomass in assessment year" << endl;
 report << "665000     # Maximum B  Upper 95% confidence interval for spawning biomass in assessment year" << endl;
 report << "202138     # BMSY  is equilibrium spawning biomass at MSY (Tiers 1-2) or 7/8 x B40% (Tier 3)" << endl;
 report << "ADMB       # MODEL - Required only if NMFS toolbox software used; optional otherwise " << endl;
 report << "NA         # VERSION - Required only if NMFS toolbox software used; optional otherwise" << endl;
 report << "2          # number of sexes  if 1 sex=ALL elseif 2 sex=(FEMALE, MALE) " << endl;
 report << "1          # number of fisheries" << endl;
 report << "1          # multiplier for recruitment, N at age, and survey number (1,1000,1000000)" << endl;
 report << "1          # recruitment age used by model or size" << endl;
 report << "1          # age+ or mmCW+ used for biomass estimate" << endl;
 report << "\"Single age\"        # Fishing mortality type such as \"Single age\" or \"exploitation rate\"" << endl;
 report << "\"Age model\"         # Fishing mortality source such as \"Model\" or \"(total catch (t))/(survey biomass (t))\"" << endl;
 report << "\"Age of maximum F\"  # Fishing mortality range such as \"Age of maximum F\"" << endl; 
 report << "#FISHERYDESC -list of fisheries (ALL TWL LGL POT FIX FOR DOM TWLJAN LGLMAY POTAUG ...)" << endl; 
 report << "ALL" << endl; 

 report <<"#FISHERYYEAR - list years used in the model " << endl;
   for (i=styr;  i<=endyr; i++)
      report << i << "	";
      report<<endl;  

 report<<"#AGE - list of ages used in the model"<<endl;
   for (i=1; i<=21;i++)
      report << i << "	";
      report<<endl;    

 report <<"#RECRUITMENT - Number of recruits by year " << endl;
   for (i=styr;  i<=endyr;  i++)
	   report  << 2*natage(1,i,1) << "	";
	   report<<endl;     
	
 report <<"#SPAWNBIOMASS - Spawning biomass by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << fspbio(i) << "	";
      report<<endl;  

 report <<"#TOTALBIOMASS - Total biomass by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << natage(1,i)*wt(1) + natage(2,i)*wt(2) << "	";
      report<<endl;
	
 report <<"#TOTFSHRYMORT - Fishing mortality rate by year " << endl;
	for (i=styr;  i<=endyr;  i++)
	   report  << (F(1,i,21)+ F(2,i,21))/2<< "	";
	   report<<endl;
	  
 report <<"#TOTALCATCH - Total catch by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << catch_bio(i) << "	";
      report<<endl;
  
report <<"#MATURITY - Maturity ratio by age (females only)" << endl;  
      for (i=1;  i<=21;  i++) 
      report  << maturity(i) <<"	";
      report<< endl; 

report <<"#SPAWNWT - Average spawning weight (in kg) by age"<< endl; 
      report <<"0.019	0.041	0.113	0.224	0.376	0.566	0.784	1.028	1.292	1.569	1.855	2.142	2.417	2.667	2.881	3.057	3.198	3.308	3.393"<<endl;                              
      report<<endl;
                    
report <<"#NATMORT - Natural mortality rate for females then males"<< endl; 
for (i=1;  i<=21;  i++) 
report  << 0.2 <<"	";
report<< endl;   
for (i=1;  i<=21;  i++) 
report  << 0.35 <<"	";
report<< endl;

report << "#N_AT_AGE - Estimated numbers of female (first) then male (second) fish at age " << endl;
  for (i=styr; i<=endyr;i++)
    report <<natage(1,i)<< "	";
    report<<endl;

  for (i=styr; i<=endyr;i++)
    report <<natage(2,i)<< "	";
    report<<endl;

report <<"#FSHRY_WT_KG - Fishery weight at age (in kg) females (first) males (second), only one fishery"<< endl;   
   report <<wt(1)*1000  << "	";
   report<<endl; //1 is females        
 
   report <<wt(2)*1000  << "	";
   report<<endl; //2 is males


 report << "#SELECTIVITY - Estimated fishery selectivity for females (first) males (second) at age " << endl;
   for (j=1; j<=nages;j++)
     report <<" " <<sel(1,j)<< "	";
     report<<endl;

   for (j=1; j<=nages;j++)
     report <<" "  <<sel(2,j)<< "	";
     report<<endl;
report << "#SURVEYDESC"<<endl;
report<<"EBS_trawl_survey BS_slope_trawl_survey AI_trawl_survey"<<endl;

report<<"SURVEYMULT"<<endl;
report<<"1 1 1"<<endl;
report << "#EBS_trawl_survey - Bering Sea shelf survey biomass (Year, Obs_biomass, Pred_biomass) " << endl;
   for (i=1; i<=nobs_srv1;i++)
     report << yrs_srv1(i) << "	";
     report<<endl;
   for (i=1; i<=nobs_srv1;i++) 
     report<< obs_srv1(i)<< "	";
     report<< endl;

report << "#BS_slope_trawl_survey - Bering Sea slope survey biomass (Year, Obs_biomass, Pred_biomass) " << endl;
   for (i=1; i<=nobs_srv2;i++)
     report << yrs_srv2(i) << "	";
     report<<endl;
   for (i=1; i<=nobs_srv2;i++)
     report << obs_srv2(i) << "	";
     report<<endl;

report << "#AI_trawl_survey - Aleutian Islands survey biomass (Year, Obs_biomass, Pred_biomass) "  << endl;
   for (i=1; i<=nobs_srv3;i++)
     report << yrs_srv3(i) << "	"; 
     report<<endl;
   for (i=1; i<=nobs_srv3;i++)
     report << obs_srv3(i) << "	";
     report<<endl;	

report<<"#STOCKNOTES"<<endl;
report<<"\"SAFE report indicates that this stock was not subjected to overfishing in 2012 and is neither overfished nor approaching a condition of being overfished in 2013.\""<<endl;
