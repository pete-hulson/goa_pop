//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+
//
//  Statistical, separable age-structured population model for Gulf of Alaska Pacific Ocean perch 
//  Alaska Fisheries Science Center, Auke Bay Laboratories
//  P. Hulson: pete.hulson@noaa.gov
//  Input file:   goa_pop_YEAR.dat
//  Control file: goa_pop_YEAR.ctl
//  Program file: model_scenario_name.tpl
//  Output files: model_scenario_name.rep, model_scenario_name.std, proj.dat (for projections)
//  Historical model structural revisions:
//    2003 - Current Size-Age transition matrix updated
//         - Size-Age transition matrix added for 60s-70s length data
//         - Begin to estimate M and q
//    2005 - Age comps input sample size changed from hauls (maxed to 100) to square root of sample size
//    2009 - Fishery selectivity changed from non-parameteric to 3 time-block logistic - avg logistic/gamma - gamma
//    2014 - Maturity estimated internally with addition of new maturity data
//    2015 - Growth estimated with length-stratified methods
//         - Ageing error matrix changed to accommodate differing number of data and model ages
//  Toggles, functions, etc. no longer in use were removed from the tpl script in 2017, to reference those see the 2015 final model
//==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//==============================================================================================================================
DATA_SECTION
//==============================================================================================================================
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  !!CLASS ofstream evalout("evalout.prj");

// Read data from the control file
  !! ad_comm::change_datafile_name("goa_pop_2019.ctl");        // Read in phases, penalties and priors from "tem.ctl"
  !! *(ad_comm::global_datafile) >>  model_name; 
  !! *(ad_comm::global_datafile) >>  data_file; 

// Spawner-recruit relationship, recruitment years and likelihood
  init_int             styr_rec_est
  init_int             endyr_rec_est
  int                  nrecs_est;
  !! nrecs_est = endyr_rec_est-styr_rec_est+1;

// Phases that general parameter estimation begins
  init_int             ph_Fdev                                 // Phase for fishing mortality deviations
  init_int             ph_avg_F                                // Phase for estimating average fishing mortality
  init_int             ph_recdev                               // Phase for estimating recruitment deviations
  init_int             ph_fish_sel                             // Phase for estimating fishing selectivity
  init_int             ph_srv1_sel                             // Phase for estimating survey selectivity
  init_int             ph_srv2_sel                             // Phase for estimating survey selectivity

// Priors and parameter phases
  init_number          mprior                                  // Prior mean for natural mortality
  number               log_mprior
  !! log_mprior = log(mprior);
  init_number          cvmprior                                // Prior CV for natural mortality
  init_int             ph_m                                    // Phase for estimating natural mortality
  init_number          sigrprior                               // Prior mean for recruitment deviations
  init_number          cvsigrprior                             // Prior CV for recruitment deviations
  init_int             ph_sigr                                 // Phase for recruiment deviations
  init_number          q_srv1prior                             // Prior mean for catchability coefficient
  number               log_q_srv1prior                         // Prior mean for catchability coefficient
  !! log_q_srv1prior = log(q_srv1prior);
  init_number          cvq_srv1prior                           // Prior CV for catchability coefficient
  init_int             ph_q_srv1                               // Phase for estimating catchability
  init_number          q_srv2prior                             // Prior mean for catchability coefficient
  number               log_q_srv2prior                         // Prior mean for catchability coefficient
  !! log_q_srv2prior = log(q_srv2prior);
  init_number          cvq_srv2prior                           // Prior CV for catchability coefficient
  init_int             ph_q_srv2                               // Phase for estimating catchability

// Data and penalty weightings
  init_int             yr_catchwt                              // year catch-wt changes...... 
  init_number          wt_ssqcatch                             // Weight for catch estimation
  init_number          wt_ssqcatch2                            // Weight for catch estimation
  init_number          wt_cpue                                 // Weight for fishery cpue estimation
  init_number          wt_srv1                                 // Weight for survey biomass estimation
  init_number          wt_srv2                                 // Weight for survey biomass estimation
  init_number          wt_fish_age                             // Weight for fishery age compositions
  init_number          wt_srv1_age                             // Weight for survey age compositions
  init_number          wt_fish_size                            // Weight for fishery size compositions
  init_number          wt_srv1_size                            // Weight for survey size compostiions
  init_number          wt_srv2_size                            // Weight for survey size compostiions
  init_number          wt_rec_var                              // Weight for estimation recruitment variatiions penalty
  init_number          wt_fmort_reg                            // Weight on fishing mortality regularity
  init_number          wt_avg_sel                              // Average selectivity penalty
  init_number          initial_LMR                             // Initial value for log mean recruitment
  init_number          yieldratio                              // Ratio of catch to ABC over most recent 3 years

//==============================================================================================================================

// Read data from the data file
  !! ad_comm::change_datafile_name(data_file);                 // Read data from the data file

// Start and end years, recruitment age, number of age and length bins
  init_int             styr
  init_int             endyr
  init_int             recage
  init_int             nages_D
  init_int             nages_M
  init_int             nlenbins
  init_int             n_ageage_mat
  init_int             n_sizeage_mat
  init_vector          len_bin_labels(1,nlenbins)
  int                  styr_rec
  int                  styr_sp
  int                  endyr_sp
  int                  nyrs
  !!  nyrs = endyr - styr + 1;
  !! styr_rec = (styr - nages_M) + 1;                          // First year of recruitment
  !! styr_sp  = styr_rec - recage ;                            // First year of spawning biomass  
  !! endyr_sp = endyr   - recage - 1;                          // endyr year of (main) spawning biomass
  vector               yy(styr,endyr);
  !! yy.fill_seqadd(styr,1) ;
  vector               aa(1,nages_M);
  !! aa.fill_seqadd(recage,1) ;
  vector               aa_D(1,nages_D);
  !! aa_D.fill_seqadd(recage,1) ;
  int                  ph_F50;
  !! ph_F50 = 4;
  init_number          spawn_fract;                            // Spawning Month
  !! spawn_fract = (spawn_fract - 1) / 12;

// Weight at age
  init_vector          wt(1,nages_M)

// Observed catches
  init_vector          obs_catch_early(styr,yr_catchwt)
  init_vector          obs_catch_later(yr_catchwt+1,endyr)
  init_int             nyrs_cpue                               // number of years of survey biomass estimates
  init_ivector         yrs_cpue(1,nyrs_cpue)                   // years survey conducted in
  init_vector          obs_cpue(1,nyrs_cpue)                   // mean estimate of biomass
  number               mean_obs_cpue;
  !! if (nyrs_cpue>0) mean_obs_cpue = exp(mean(log(obs_cpue))); 

// Trawl Survey biomass estimates
  init_int             nyrs_srv1                               // number of years of survey biomass estimates
  init_ivector         yrs_srv1(1,nyrs_srv1)                   // years survey conducted in
  init_vector          obs_srv1_biom(1,nyrs_srv1)              // mean estimate of biomass
  init_vector          obs_srv1_se(1,nyrs_srv1)                // standard error of survey biomass estimates
  init_vector          obs_srv1_lci(1,nyrs_srv1)               // lower confidence interval, for graphing not used in estimation
  init_vector          obs_srv1_uci(1,nyrs_srv1)               // upper confidence interval
  
// Longline Survey biomass estimates
  init_int             nyrs_srv2                               // number of years of survey biomass estimates
  init_ivector         yrs_srv2(1,nyrs_srv2)                   // years survey conducted in
  init_vector          obs_srv2_biom(1,nyrs_srv2)              // mean estimate of biomass
  init_vector          obs_srv2_se(1,nyrs_srv2)                // standard error of survey biomass estimates
  init_vector          obs_srv2_lci(1,nyrs_srv2)               // lower confidence interval, for graphing not used in estimation
  init_vector          obs_srv2_uci(1,nyrs_srv2)               // upper confidence interval

// Fishery age composition data
  init_int             nyrs_fish_age                           // number of years of fishery age compos
  init_ivector         yrs_fish_age(1,nyrs_fish_age)           // the years of age comps
  init_vector          nsamples_fish_age(1,nyrs_fish_age)      // total sample size for each age comp.
  init_vector          nhauls_fish_age(1,nyrs_fish_age)        // number of hauls for for each age comp.
  init_ivector         age_age_ind_fsh(1,nyrs_fish_age)        // some measure of relative sample size for each fishery comp
  init_matrix          oac_fish(1,nyrs_fish_age,1,nages_D)     // the actual year by year age comps
  vector               nmulti_fish_age(1,nyrs_fish_age)        // Input N for fishery age

// Bottom trawl survey age composition data
  init_int             nyrs_srv1_age                           // number of years of survey age compositions
  init_ivector         yrs_srv1_age(1,nyrs_srv1_age)           // the years of survey age comps
  init_vector          nsamples_srv1_age(1,nyrs_srv1_age)      // total sample size for each age comp.
  init_vector          nhauls_srv1_age(1,nyrs_srv1_age)        // number of hauls for for each age comp.
  init_ivector         age_age_ind_srv(1,nyrs_srv1_age)        // some measure of relative sample size for each fishery comp
  init_matrix          oac_srv1(1,nyrs_srv1_age,1,nages_D)     // the year by year age survey age comps
  vector               nmulti_srv1_age(1,nyrs_srv1_age)        // Input N for survey 1 age

// Fishery size composition data
  init_int             nyrs_fish_size                          // number of years of fishery size comps
  init_ivector         yrs_fish_size(1,nyrs_fish_size)         // the years of fishery size comps
  init_vector          nsamples_fish_size(1,nyrs_fish_size)    // totals for fish lengths
  init_vector          nhauls_fish_size(1,nyrs_fish_size)      // hauls for fish lengths by year
  init_ivector         siz_age_ind_fsh(1,nyrs_fish_size)       // some measure of relative sample size for each fishery comp
  init_matrix          osc_fish(1,nyrs_fish_size,1,nlenbins)   // year by year fishery size comps
  vector               nmulti_fish_size(1,nyrs_fish_size)      // Input N for fishery size

// Bottom trawl survey size composition data
  init_int             nyrs_srv1_size                          // number of years of survey size comps
  init_ivector         yrs_srv1_size(1,nyrs_srv1_size)         // the years of survey size comps
  init_vector          nsamples_srv1_size(1,nyrs_srv1_size)    // total lengths for survey 1 by year
  init_vector          nhauls_srv1_size(1,nyrs_srv1_size)      // total hauls for length samples in survey 1 by year
  init_ivector         siz_age_ind_srv1(1,nyrs_srv1_size)      // some measure of relative sample size for each fishery comp
  init_matrix          osc_srv1(1,nyrs_srv1_size,1,nlenbins)   // year by year size comps

// Alternative survey size composition data
  init_int             nyrs_srv2_size                          // number of years of survey size comps
  init_ivector         yrs_srv2_size(1,nyrs_srv2_size)         // the years of survey size comps
  init_vector          nsamples_srv2_size(1,nyrs_srv2_size)    // total lengths
  init_vector          nhauls_srv2_size(1,nyrs_srv2_size)      // hauls
  init_ivector         siz_age_ind_srv2(1,nyrs_srv2_size)      // some measure of relative sample size for each fishery comp
  init_matrix          osc_srv2(1,nyrs_srv2_size,1,nlenbins)   // year by year size comps

// Size-age transition matrix:  proportion at size given age
  init_3darray         sizeage(1,n_sizeage_mat,1,nages_M,1,nlenbins)

// Ageing error transition matrix:  proportion at reader age given true age
  init_3darray         ageage(1,n_ageage_mat,1,nages_M,1,nages_D)

// Declare index variables
  int                  iyr
  int                  i
  int                  j
  int                  l;

//EOF Marker
  init_int             eof;
  !! cout <<obs_catch_later<<endl;


//==============================================================================================================================

// Read maturity data from the data file
  !! ad_comm::change_datafile_name("mat.dat");                 // Read data from the data file

  init_int             nages_mat
  init_vector          ages_mat(1,nages_mat)
  init_vector          L_tot_na(1,nages_mat)
  init_vector          L_mat_na(1,nages_mat)
  init_vector          C_tot_na(1,nages_mat)
  init_vector          C_mat_na(1,nages_mat)


//==============================================================================================================================

// Add local calcs section for checking if data file is correct
 LOCAL_CALCS

   if(eof==42) cout<<"The data has been read correctly!";
   else { cout <<"You f'ed up your data file!"<<endl;exit(1); }
   if(wt_rec_var==0) {
     if (ph_sigr>0) {
       cout << "Warning, wt_rec_var is zero, so can't estimate sigr!@"<<endl;
       cout << "turning sigr off "<<endl;
       ph_sigr =-4;
       cout << "hit any key, then enter to continue"<<endl;
       char  xxx; cin >> xxx;
     }
   }

 END_CALCS

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//==============================================================================================================================
INITIALIZATION_SECTION
//==============================================================================================================================
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Starting values for estimated parameters; these values over-ride all others
  logm                 log_mprior
  log_mean_rec1        initial_LMR
  log_mean_rec2        initial_LMR
  log_mean_rec3        initial_LMR
  sigr1                sigrprior 
  sigr2                sigrprior 
  sigr3                sigrprior    
  a50                  2.5
  delta                4.5
  a502                 6
  delta2               1.5
  a503                 2.5
  delta3               4.5   
  a50_srv1             7.3
  delta_srv1           3.8
  a50_srv2             7.3
  delta_srv2           3.8

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//==============================================================================================================================
PARAMETER_SECTION
//==============================================================================================================================
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Stock-recruitment
  vector               Sp_Biom(styr,endyr)
  init_number          log_mean_rec1(1);                        // Unfish equil recruitment (logged)
  init_number          log_mean_rec2(1);                        // Unfish equil recruitment (logged)
  init_number          log_mean_rec3(1);                        // Unfish equil recruitment (logged)
  init_bounded_number  sigr1(0.3,10,ph_sigr);                   // Recruitment sderr parameter
  init_bounded_number  sigr2(0.3,10,ph_sigr);                   // Recruitment sderr parameter
  init_bounded_number  sigr3(0.3,10,ph_sigr);                   // Recruitment sderr parameter

// Fishery selectivity
  init_number          a50(ph_fish_sel);                       // age at 50% selection                                                   
  init_number          delta(ph_fish_sel);                     // age between 50% selection and 95% selection....
  init_number          a502(ph_fish_sel);                      // age at 50% selection                                                   
  init_number          delta2(ph_fish_sel);                    // age between 50% selection and 95% selection....
  init_number          a503(ph_fish_sel);                      // age at 50% selection                                                   
  init_number          delta3(ph_fish_sel);                    // age between 50% selection and 95% selection....
  number               expa50;                                 // gamma selectivity parameter
  number               expa502;                                // gamma selectivity parameter
  vector               fish_sel1(1,nages_M);                    // vectory of fishery selectivty parameters on arithmetic scale
  vector               fish_sel2(1,nages_M);                   // vectory of fishery selectivty parameters on arithmetic scale
  vector               fish_sel3(1,nages_M);                   // vectory of fishery selectivty parameters on arithmetic scale
  vector               fish_sel4(1,nages_M);                   // vectory of fishery selectivty parameters on arithmetic scale
 
// Trawl Survey selectivity
  init_number          a50_srv1(ph_srv1_sel);                  // age at 50% selection                                                   
  init_number          delta_srv1(ph_srv1_sel);                // age between 50% selection and 95% selection....
  vector               srv1_sel(1,nages_M);                    // vectory of survey selectivty parameters on arithmetic scale

// Alternate Survey selectivity
  init_number          a50_srv2(ph_srv2_sel);                  // age at 50% selection                                                   
  init_number          delta_srv2(ph_srv2_sel);                // age between 50% selection and 95% selection....
  vector               srv2_sel(1,nages_M);                    // vectory of survey selectivty parameters on arithmetic scale

// Fishing mortality
  init_number          log_avg_F(ph_avg_F);                    // Log average fishing mortality
  init_bounded_vector  log_F_devs(styr,endyr,-15.,15.,ph_Fdev); // Annual fishing mortality deviations
  vector               Fmort(styr,endyr);                      // Fishing mortality by year
  matrix               Z(styr,endyr,1,nages_M);                // Total mortality by year and age
  matrix               F(styr,endyr,1,nages_M);                // Fishing mortality by year and age
  matrix               S(styr,endyr,1,nages_M);                // Survivorship by year and age

//Parameters to estimate maturity
  init_number          mat_a50(1)                              // age at 50% maturity
  init_number          mat_delta(1)                            // slope parameter for maturity
  vector               L_pmat(1,nages_mat)                     // vector for old observed maturity proportions (a.k.a. Lunsford from notherns)                                 
  vector               C_pmat(1,nages_mat)                     // vector for new observed maturity proportions (a.k.a. Chilton from notherns)
  vector               Pred_pmat(1,nages_mat)                  // predicted maturity proportions
  vector               p_mature(1,nages_M)                     // estimated maturity at age
  vector               wt_mature(1,nages_M);                   // Weight of mature fish vector at age
  vector               Like_L_vec(1,nages_mat)                 // likelihood vector for old data
  vector               Like_C_vec(1,nages_mat)                 // likelihood vector for new data
  number               Like_L                                  // likelihood for old data
  number               Like_C                                  // likelihood for new data
  number               zero_pen_mat                            // penalty to ensure maturity is 0% for ages in which maturity has never been observed
  number               k                                       // constant
  !! k=0.00001;

// Create a vector of natual mortalities for proj.dat
  vector               natmortv(1,nages_M);

// Numbers at age
  init_bounded_vector  log_rec_dev(styr_rec+1,endyr,-10.,10.,ph_recdev); // Recruitment deviations from before the asssessment starts to present
  vector               rec_devs_early(styr,1977);              // Recruitment deviations from before the asssessment starts to present
  vector               rec_devs_mid(1978,1999);                // Recruitment deviations from before the asssessment starts to present
  vector               rec_devs_late(2000,endyr);              // Recruitment deviations from before the asssessment starts to present
  matrix               natage(styr,endyr,1,nages_M);           // Matrix of numbers at age from start year to end year

// Biomass at age
  matrix               batage(styr,endyr,1,nages_M);           // Matrix of biomass at age from start year to end year

// Catch at age
  matrix               catage(styr,endyr,1,nages_M)            // Matrix of predicted catch at age from start year to endyear
  vector               pred_catch_early(styr,yr_catchwt)       // Vector of predicted catches
  vector               pred_catch_later(yr_catchwt+1,endyr)    // Vector of predicted catches

// Predicted values
  init_number          log_q_srv1(ph_q_srv1);                  // Estimate Log survey catchability
  init_number          log_q_srv2(ph_q_srv2);                  // Estimate Log survey catchability
  init_number          cv_cpue(-1)                             // cv for cpue index  
  init_number          logm(ph_m);                             // Estimate log natural mortality
  number               q_cpue;
  vector               pred_cpue(1,nyrs_cpue);                 // Predicted CPUE
  vector               pred_srv1(1,nyrs_srv1);                 // Predicted survey
  vector               pred_srv2(1,nyrs_srv2);                 // Predicted survey
  matrix               eac_fish(1,nyrs_fish_age,1,nages_D)     // Expected proportion at age in fish
  matrix               eac_srv1(1,nyrs_srv1_age,1,nages_D)     // Expected proportion at age in survey
  matrix               esc_fish(1,nyrs_fish_size,1,nlenbins)   // Expected proportion at size in fishery
  matrix               esc_srv1(1,nyrs_srv1_size,1,nlenbins)   // Expected proportion at size in survey
  matrix               esc_srv2(1,nyrs_srv2_size,1,nlenbins)   // Expected proportion at size in survey 2

// Effective N and SDNR    
  vector               effn_fish_age(1,nyrs_fish_age)          // Effective N for fishery age
  vector               sdnr_fish_age(1,nyrs_fish_age)          // SDNR for fishery age
  vector               effn_fish_size(1,nyrs_fish_size)        // Effective N for fishery size
  vector               sdnr_fish_size(1,nyrs_fish_size)        // SDNR for fishery size
  vector               effn_srv1_age(1,nyrs_srv1_age)          // Effective N for survey 1 age
  vector               sdnr_srv1_age(1,nyrs_srv1_age)          // SDNR for survey 1 age
  vector               effn_srv1_size(1,nyrs_srv1_size)        // Effective N for survey 1 size
  vector               sdnr_srv1_size(1,nyrs_srv1_size)        // SDNR for survey 1 size  
  vector               effn_srv2_size(1,nyrs_srv2_size)        // Effective N for survey 2 size
  vector               sdnr_srv2_size(1,nyrs_srv2_size)        // SDNR for survey 2 size

// Standard deviation estimates for some estimated parameters
  sdreport_vector      tot_biom(styr,endyr);                   // Standard deviation report vector of total biomass
  sdreport_number      q_srv1;                                 // " " for Survey1 catchability
  number               q_srv2; 
  sdreport_vector      pred_rec(styr,endyr);                   // " " for predicted recruitments
  vector               expl_rate(styr,endyr);                  // " " for exploitation rate 
  sdreport_number      avg_rec;                                // " " for Average recruitment 
  sdreport_number      spbiom_trend;                           // " " of Trend in biomass over last 2 years (B(t)/B(t-1); t=endyr)
  number               Depletion;                              // " " for Depletion
  sdreport_vector      spawn_biom(styr,endyr);                 // " " for spawning biomass vector
  number               natmort;                                // " " for natural mortality
  sdreport_number      LMR;
  sdreport_number      cigar;
  sdreport_number      q2;
  sdreport_number      nattymort;

// Parameters for computing SPR rates 
  init_bounded_number  mF50(0.01,1.,ph_F50)                    // Estimated F50
  init_bounded_number  mF40(0.01,1.,ph_F50)                    // Estimated F40
  init_bounded_number  mF35(0.01,1.,ph_F50)                    // Estimated F35
  sdreport_number      F50;                                    // Standard deviation report for F50
  sdreport_number      F40;                                    // " " " F40
  sdreport_number      F35;                                    // " " " F35
  number               SB0                                     // Spawning biomass at no fishing
  number               SBF50                                   // " " at F50
  number               SBF40                                   // " " at F40
  number               SBF35                                   // " " at F35
  number               sprpen                                  // Likelihood penalty to make ADMB estimate spr rates
  matrix               Nspr(1,4,1,nages_M)                     // Matrix of number of spawners at age at each fishing mortality level

// Likelihoods and penalty functions
  vector               surv_like(1,3);                         // Likelihood values for survey biomasses, allowance for up to 3 surveys
  number               cpue_like;                              // Likelihood values for cpue index 
  vector               age_like(1,6);                          // Likelihood values for age and size compositions allowance for up 6 comps
  vector               offset(1,6);                            // Multinomial "offset"
  number               rec_like;                               // Likelihood value for recruitments
  vector               rec_like_vec(styr,endyr);                               // Likelihood value for recruitments
  number               ssqcatch;                               // Likelihood value for catch estimation
  number               F_mort_regularity;                      // Penalty value for fishing mortality regularity
  number               avg_sel_penalty;                        // Penalty value for selectivity regularity penalty

// Priors
  vector               priors(1,5);                            // Prior penalty values for sigr,q,natural mortality

// Define an objective function
  number               Like;                                   // Likelihood for data fits
  objective_function_value obj_fun;                            // Total likelihood for objective function value
  vector               pred_catch(styr,endyr);
  vector               obs_catch(styr,endyr);

// Population projection
  matrix               N_proj(endyr+1,endyr+15,1,nages_M);
  number               FABC_proj;
  vector               FABC_tot_proj(1,nages_M);
  number               FOFL_proj;
  vector               FOFL_tot_proj(1,nages_M);
  sdreport_number      ABC;                                    // Estimate of next year's ABC
  sdreport_number      B40;
  number               OFL;
  vector               Z_proj(1,nages_M);
  vector               ZOFL_proj(1,nages_M);
  vector               S_proj(1,nages_M);
  matrix               catage_proj(endyr+1,endyr+15,1,nages_M);
  matrix               catage_proj_OFL(endyr+1,endyr+15,1,nages_M);
  vector               pred_catch_proj(endyr+1,endyr+15);
  vector               pred_catch_proj_OFL(endyr+1,endyr+15);
  sdreport_vector      spawn_biom_proj(endyr+1,endyr+15);
  sdreport_vector      tot_biom_proj(endyr+1,endyr+15);
  number               stdev_rec;
  number               FOFL;
  number               FABC;
  number               FOFL2;
  number               FABC2;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//==============================================================================================================================
PROCEDURE_SECTION
//==============================================================================================================================
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  l=l+1;                                                       // Initiate counter for random seeds in projection
  Get_Selectivity();                                           // Call function to get selectivities
  Get_Maturity();                                              // Call function to get maturity proportions at age
  Get_Mortality_Rates();                                       // Call function to get fishing and natural mortality
  Get_Numbers_At_Age();                                        // Call function to get numbers at age per year
  Get_Catch_at_Age();                                          // Call function to get catch at age per year
  Get_Predicted_Values();                                      // Get predicted values for catch, survbio, age and size comps
  if (last_phase())
  {
    Get_Dependent_Vars();                                      // Solve for dependent variables like total bio, recruitment etc.
    Compute_SPR_Rates();                                       // Compute f40 etc.
    Get_Population_Projection();                               // Get 15 year population projection
  }
  Evaluate_Objective_Function();                               // Minimize objective function value
  if (mceval_phase())                                          // For outputting MCMC simulations in text format
  {
     evalout<<sigr1<<" "<<q_srv1<<" "<<q_srv2<<" "<<F40<<" "<<natmort<<" "<<" "<<ABC<<" "<<obj_fun<<" "<<tot_biom<<" "<<log_rec_dev<<" "<<spawn_biom<<" "<<log_mean_rec1<<" "<<spawn_biom_proj<<" "<<pred_catch_proj<<" "<<N_proj(endyr+1,1)<<" "<<N_proj(endyr+2,1)<<" "<<N_proj(endyr+3,1)<<" "<<N_proj(endyr+4,1)<<" "<<N_proj(endyr+5,1)<<" "<<N_proj(endyr+6,1)<<" "<<N_proj(endyr+7,1)<<" "<<N_proj(endyr+8,1)<<" "<<N_proj(endyr+9,1)<<" "<<N_proj(endyr+10,1)<<" "<<tot_biom_proj(endyr+1)<<" "<<tot_biom_proj(endyr+2)<<" "<<pred_srv1<<" "<<endl;
  }

//==============================================================================================================================
FUNCTION Get_Selectivity
//==============================================================================================================================

// Fishery Selectivity
  expa50 = mfexp(a50); 
  expa502 = mfexp(a503); 

  for (j=1;j<=nages_M;j++)  {
    fish_sel1(j) = 1./(1. + mfexp(-2.944438979*(double(j)-a502)/delta2));
    fish_sel3(j)=(pow(j/expa50,expa50/(0.5*(sqrt(square(expa50)+4*square(delta))-expa50)))*mfexp((expa50-j)/(0.5*(sqrt(square(expa50)+4*square(delta))-expa50))));
    fish_sel4(j)=(pow(j/expa502,expa502/(0.5*(sqrt(square(expa502)+4*square(delta3))-expa502)))*mfexp((expa502-j)/(0.5*(sqrt(square(expa502)+4*square(delta3))-expa502)))); }
  fish_sel2 = (fish_sel1 + fish_sel3)/2; 
  fish_sel3=fish_sel3/max(fish_sel3);
  fish_sel4=fish_sel4/max(fish_sel4);

// Bottom Trawl Survey Selectivity
  for (j=1;j<=nages_M;j++)
    srv1_sel(j) = 1./(1. + mfexp(-2.944438979*(double(j)-a50_srv1)/delta_srv1));

// Alternate Survey Selectivity, set equal to Bottom Trawl Survey Selectivity
  srv2_sel = srv1_sel;

//==============================================================================================================================
FUNCTION Get_Maturity
//==============================================================================================================================

  for (int i=1;i<=nages_mat;i++){
   if(L_tot_na(i)>0){L_pmat(i) = L_mat_na(i)/L_tot_na(i);}
   if(C_tot_na(i)>0){C_pmat(i) = C_mat_na(i)/C_tot_na(i);}
   Pred_pmat(i) = 1/(1+exp(-1.0*mat_delta*(ages_mat(i)-mat_a50)));}

  for (int i=1;i<=nages_M;i++){
  p_mature(i) = 1/(1+exp(-1.0*mat_delta*((i+1)-mat_a50)));
  wt_mature(i) = wt(i)*p_mature(i)/2;}

//==============================================================================================================================
FUNCTION Get_Mortality_Rates
//==============================================================================================================================

  natmort = mfexp(logm);                                       // setting natural mortality to arithmetic scale
  if(ph_m>0) nattymort=natmort; else nattymort=log_mean_rec1;
  Fmort = mfexp(log_avg_F +  log_F_devs);                      // setting fishing mortaltiy to arithmetic scale
  for (iyr=styr; iyr<=1976; iyr++)
    F(iyr) = Fmort(iyr) * fish_sel1;                           // Getting fully selected fishing mortality
  for (iyr=1977; iyr<=1995; iyr++)
    F(iyr) = Fmort(iyr) * fish_sel2;                           // Getting fully selected fishing mortality
  for (iyr=1996; iyr<=2006; iyr++)
    F(iyr) = Fmort(iyr) * fish_sel3;                           // Getting fully selected fishing mortality Z = F + natmort;
  for (iyr=2007; iyr<=endyr; iyr++)
    F(iyr) = Fmort(iyr) * fish_sel4;                           // Getting fully selected fishing mortality Z = F + natmort;
  Z = F + natmort;                                             // Fully selected total mortality
  S = mfexp(-1.0*Z);                                           // Fully selected survival

//==============================================================================================================================
FUNCTION Get_Numbers_At_Age  
//==============================================================================================================================
  rec_like_vec.initialize();

// Start year abundance
  int itmp;
  for (j=2;j<nages_M;j++) {
    itmp = styr+1-j;
    natage(styr,j) = mfexp(log_mean_rec1 - natmort * double(j-1)+ log_rec_dev(itmp)); 
  }
  natage(styr,nages_M) = mfexp(log_mean_rec1 - natmort * (nages_M-1)) / (1. - exp(-natmort));

// Remaining years
  for (i=styr;i<1978;i++) {
    natage(i,1) = mfexp(log_rec_dev(i) + log_mean_rec1);
    natage(i+1)(2,nages_M) = ++elem_prod(natage(i)(1,nages_M-1),S(i)(1,nages_M-1));       // Following year
    natage(i+1,nages_M) += natage(i,nages_M)*S(i,nages_M);
    Sp_Biom(i) = natage(i) * wt_mature;                        // Old way, correct way to get SSB at start of spawning season would be: Sp_Biom(i) = elem_prod(natage(i),pow(S(i),spawn_fract)) * wt_mature;
  }

  for (i=1978;i<2000;i++) {
    natage(i,1) = mfexp(log_rec_dev(i) + log_mean_rec2);
    natage(i+1)(2,nages_M) = ++elem_prod(natage(i)(1,nages_M-1),S(i)(1,nages_M-1));       // Following year
    natage(i+1,nages_M) += natage(i,nages_M)*S(i,nages_M);
    Sp_Biom(i) = natage(i) * wt_mature;                        // Old way, correct way to get SSB at start of spawning season would be: Sp_Biom(i) = elem_prod(natage(i),pow(S(i),spawn_fract)) * wt_mature;
  }

  for (i=2000;i<endyr;i++) {
    natage(i,1) = mfexp(log_rec_dev(i) + log_mean_rec3);
    natage(i+1)(2,nages_M) = ++elem_prod(natage(i)(1,nages_M-1),S(i)(1,nages_M-1));       // Following year
    natage(i+1,nages_M) += natage(i,nages_M)*S(i,nages_M);
    Sp_Biom(i) = natage(i) * wt_mature;                        // Old way, correct way to get SSB at start of spawning season would be: Sp_Biom(i) = elem_prod(natage(i),pow(S(i),spawn_fract)) * wt_mature;
  }

// End year abundance
  natage(endyr,1) = mfexp(log_rec_dev(endyr) + log_mean_rec3); 
  Sp_Biom(endyr) = elem_prod(natage(endyr),pow(S(endyr),spawn_fract)) * wt_mature;  //Right way, old way was: Sp_Biom(endyr) = natage(endyr)* wt_mature;

// Get recruitment devs for penalty
  for (i=styr;i<1978;i++)
  	rec_devs_early(i) = log_rec_dev(i);
  for (i=1978;i<2000;i++)
  	rec_devs_mid(i) = log_rec_dev(i);
  for (i=2000;i<=endyr;i++)
  	rec_devs_late(i) = log_rec_dev(i);

//==============================================================================================================================
FUNCTION Get_Catch_at_Age
//==============================================================================================================================

  pred_catch_early.initialize();
  pred_catch_later.initialize();

// Early years up to 1977
  for (iyr=styr;iyr<=yr_catchwt;iyr++) {
    catage(iyr) = elem_div(elem_prod(elem_prod(natage(iyr),F(iyr)),(1.-S(iyr))),Z(iyr));
    pred_catch_early(iyr) = catage(iyr)*wt;
  }

// Later years after 1977
  for (iyr=yr_catchwt+1;iyr<=endyr;iyr++) {
    catage(iyr) = elem_div(elem_prod(elem_prod(natage(iyr),F(iyr)),(1.-S(iyr))),Z(iyr));
    pred_catch_later(iyr) = catage(iyr)*wt;
  }

//==============================================================================================================================
FUNCTION Get_Predicted_Values
//==============================================================================================================================

  offset.initialize();

// Calculate predicted data values
  q_srv1         = exp(log_q_srv1);                            // Survey catchability at arithmetic scale
  q_srv2         = exp(log_q_srv2);                            // Survey catchability at arithmetic scale
  for (i=1;i<=nyrs_srv1;i++)
    pred_srv1(i) = q_srv1 * (natage(yrs_srv1(i))*elem_prod(srv1_sel,wt));   // Predicted Survey biomass
  if(nyrs_srv2>0) { for (i=1;i<=nyrs_srv2;i++)
    pred_srv2(i) = q_srv2 * (natage(yrs_srv2(i))*elem_prod(srv2_sel,wt));   // Predicted Survey biomass
  }

// Predicted Fishery age comps, N, effn, sdnr, and offset 
  for (i=1;i<=nyrs_fish_age;i++) {
   eac_fish(i)  = catage(yrs_fish_age(i))/sum(catage(yrs_fish_age(i))) * ageage(age_age_ind_fsh(i));
   nmulti_fish_age(i) = sqrt(nsamples_fish_age(i));
   effn_fish_age(i) = (1-eac_fish(i))*eac_fish(i)/norm2(oac_fish(i)-eac_fish(i));
   sdnr_fish_age(i) = sdnr(eac_fish(i),oac_fish(i),double(nmulti_fish_age(i)));
  }
  for (i=1; i<=nyrs_fish_age; i++) {
   oac_fish(i)/=sum(oac_fish(i));
   offset(1) -= nmulti_fish_age(i) *((oac_fish(i) + 0.00001)*log(oac_fish(i) + 0.00001)); 
  }

// Predicted Survey1 age comps, N, effn, sdnr, and offset       
  for (i=1;i<=nyrs_srv1_age;i++) {
   eac_srv1(i)  = elem_prod(srv1_sel,natage(yrs_srv1_age(i)))/(natage(yrs_srv1_age(i)) * srv1_sel)* ageage(age_age_ind_srv(i));
   nmulti_srv1_age(i) = sqrt(nsamples_srv1_age(i));
   effn_srv1_age(i) = (1-eac_srv1(i))*eac_srv1(i)/norm2(oac_srv1(i)-eac_srv1(i));
   sdnr_srv1_age(i) = sdnr(eac_srv1(i),oac_srv1(i),double(nmulti_srv1_age(i)));
  }
  for (i=1; i<=nyrs_srv1_age; i++) {
   oac_srv1(i)/=sum(oac_srv1(i));
   offset(2) -= nmulti_srv1_age(i)*((oac_srv1(i) + 0.00001)*log(oac_srv1(i) + 0.00001));
  }

// Predicted Fishery size comps, N, effn, sdnr, and offset
  for (i=1;i<=nyrs_fish_size;i++) {
   esc_fish(i)  = catage(yrs_fish_size(i))/sum(catage(yrs_fish_size(i))) * sizeage(siz_age_ind_fsh(i));
   nmulti_fish_size(i) = nhauls_fish_size(i);
   effn_fish_size(i) = (1-esc_fish(i))*esc_fish(i)/norm2(osc_fish(i)-esc_fish(i));
   sdnr_fish_size(i) = sdnr(esc_fish(i),osc_fish(i),double(nmulti_fish_size(i)));
  }
  nmulti_fish_size = nmulti_fish_size/max(nmulti_fish_size)*100;
  for (i=1; i<=nyrs_fish_size; i++) {
   osc_fish(i)/=sum(osc_fish(i));
   offset(3) -= nmulti_fish_size(i)*((osc_fish(i) + 0.00001)*log(osc_fish(i) + 0.00001));
  }

// Predicted Survey1 size comps, N, effn, sdnr, and offset
  for (i=1;i<=nyrs_srv1_size;i++) {
   esc_srv1(i)  = elem_prod(srv1_sel,natage(yrs_srv1_size(i))) /(natage(yrs_srv1_size(i)) * srv1_sel)* sizeage(siz_age_ind_srv1(i));
   effn_srv1_size(i) = (1-esc_srv1(i))*esc_srv1(i)/norm2(osc_srv1(i)-esc_srv1(i));
   //sdnr_srv1_size(i) = sdnr(esc_srv1(i),osc_srv1(i),double(effn_srv1_size(i)));
  }
  for (i=1; i<=nyrs_srv1_size; i++) {
   osc_srv1(i)/=sum(osc_srv1(i));
   offset(4) -= effn_srv1_size(i)*((osc_srv1(i) + 0.00001)*log(osc_srv1(i) + 0.00001));
  }

// Predicted Survey2 size comps, N, effn, sdnr                         
// Need if statement to avoid errors in model when solving without srv2 data
  if(nyrs_srv2>0) {
   for ( i=1;i<=nyrs_srv2_size;i++) {
    esc_srv2(i)  = elem_prod(srv2_sel,natage(yrs_srv2_size(i))) /(natage(yrs_srv2_size(i)) * srv2_sel)* sizeage(siz_age_ind_srv2(i));
    effn_srv2_size(i) = (1-esc_srv2(i))*esc_srv2(i)/norm2(osc_srv2(i)-esc_srv2(i));
    //sdnr_srv2_size(i) = sdnr(esc_srv2(i),osc_srv2(i),double(effn_srv2_size(i)));
   }
   for (i=1; i<=nyrs_srv2_size; i++) {
    osc_srv2(i)/=sum(osc_srv2(i));
    offset(5) -= effn_srv2_size(i)*((osc_srv2(i) + 0.00001)*log(osc_srv2(i) + 0.00001));
   }
  }

// Predicted CPUE  
  if (nyrs_cpue>0)
  {
    int yy;
    for (i=1;i<=nyrs_cpue;i++) 
    {
      yy = yrs_cpue(i);
      pred_cpue(i) = wt*elem_div(elem_prod(natage(yy),fish_sel4),Z(yy)); 
    } 
    q_cpue = mean_obs_cpue/mfexp(mean(log(pred_cpue)));
    pred_cpue *=  q_cpue;
  } 

// Predicted catch
  pred_catch(styr,yr_catchwt) = pred_catch_early;
  pred_catch(yr_catchwt+1,endyr) = pred_catch_later;
  obs_catch(styr,yr_catchwt) = obs_catch_early;
  obs_catch(yr_catchwt+1,endyr) = obs_catch_later;
  
// set up some sdreport numbers
  if(ph_q_srv2>0) q2=mfexp(log_q_srv2); else q2=mfexp(log_q_srv1);
  cigar= sigr1;
  LMR = log_mean_rec3;

//==============================================================================================================================
FUNCTION Get_Dependent_Vars
//==============================================================================================================================

  for (i=styr;i<=endyr;i++) {
    pred_rec(i) = natage(i,1);                                 // Setting up results based on estimated paramters
    tot_biom(i) = wt * natage(i);                              // Total biomass results
    expl_rate(i) = pred_catch(i)/tot_biom(i);                  // Setting up results based on estimated paramters
    spawn_biom(i) = Sp_Biom(i) ;                               // Spawning biomass result
  }
  avg_rec        = mean(pred_rec);
  Depletion      = spawn_biom(endyr)/spawn_biom(styr);         // 1-Depletion
  spbiom_trend   = spawn_biom(endyr)/spawn_biom(endyr-1);

//==============================================================================================================================
FUNCTION Compute_SPR_Rates
//==============================================================================================================================

  SB0=0.;
  SBF50=0.;
  SBF40=0.;
  SBF35=0.;

  // Scale F-spr rates to be on full-selected values
  F50  = mF50*max(fish_sel4);
  F40  = mF40*max(fish_sel4);
  F35  = mF35*max(fish_sel4);
  for (i=1;i<=4;i++)
    Nspr(i,1)=1.;
  
  for (j=2;j<nages_M;j++) {
    Nspr(1,j)=Nspr(1,j-1)*mfexp(-1.*natmort);
    Nspr(2,j)=Nspr(2,j-1)*mfexp(-1.*(natmort+mF50*fish_sel4(j-1)));
    Nspr(3,j)=Nspr(3,j-1)*mfexp(-1.*(natmort+mF40*fish_sel4(j-1)));
    Nspr(4,j)=Nspr(4,j-1)*mfexp(-1.*(natmort+mF35*fish_sel4(j-1)));
  }

  Nspr(1,nages_M)=Nspr(1,nages_M-1)*mfexp(-1.*natmort)/(1.-mfexp(-1.*natmort));
  Nspr(2,nages_M)=Nspr(2,nages_M-1)*mfexp(-1.* (natmort+mF50*fish_sel4(nages_M-1)))/(1.-mfexp(-1.*(natmort+mF50*fish_sel4(nages_M))));
  Nspr(3,nages_M)=Nspr(3,nages_M-1)*mfexp(-1.* (natmort+mF40*fish_sel4(nages_M-1)))/ (1.-mfexp(-1.*(natmort+mF40*fish_sel4(nages_M))));
  Nspr(4,nages_M)=Nspr(4,nages_M-1)*mfexp(-1.* (natmort+mF35*fish_sel4(nages_M-1)))/ (1.-mfexp(-1.*(natmort+mF35*fish_sel4(nages_M))));
  
  for (j=1;j<=nages_M;j++) {
   // Kill them off till (spawn_fract)
    SB0    += Nspr(1,j)*wt_mature(j)*mfexp(-spawn_fract*natmort);
    SBF50  += Nspr(2,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+mF50*fish_sel4(j)));
    SBF40  += Nspr(3,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+mF40*fish_sel4(j)));
    SBF35  += Nspr(4,j)*wt_mature(j)*mfexp(-spawn_fract*(natmort+mF35*fish_sel4(j)));
  }
  
  sprpen    = 100.*square(SBF50/SB0-0.5);
  sprpen   += 100.*square(SBF40/SB0-0.4);
  sprpen   += 100.*square(SBF35/SB0-0.35);
  
  B40= SBF40*mean(pred_rec(1979,endyr-recage));

//==============================================================================================================================
FUNCTION Get_Population_Projection
//==============================================================================================================================

//  Abundance at start of first projection year
  int k;

// Recruitment in endyr+1
  if(mceval_phase()) {
    stdev_rec = sqrt(norm2(value(log_rec_dev(1977 + recage,endyr - recage))-mean(value(log_rec_dev(1977 + recage,endyr - recage))))/(size_count(value(log_rec_dev(1977 + recage,endyr - recage))) - 1));
    k=round(value(stdev_rec) * 10000);
    N_proj(endyr+1,1) = mfexp(value(log(mean(value(pred_rec(1977+recage,endyr-recage))))-square(stdev_rec)/2+stdev_rec*randn(k+l)));
    cout<<stdev_rec<<" "<<k<<" "<<l<<" "<<endl;
  }
  else {
    N_proj(endyr+1,1)= value(mean(pred_rec(1977+recage,endyr-recage))); }

// Abundance for remaining age classes in endyr+1
  for (j=1;j<nages_M-1;j++)
    N_proj(endyr+1,j+1) = natage(endyr,j) * S(endyr,j);
  N_proj(endyr+1,nages_M) = natage(endyr,nages_M-1) * S(endyr,nages_M-1) + natage(endyr,nages_M) * S(endyr,nages_M);
  tot_biom_proj(endyr+1) = N_proj(endyr+1) * wt;
  spawn_biom_proj(endyr+1) = elem_prod(N_proj(endyr+1),pow(mfexp(-yieldratio * FABC_tot_proj-natmort),spawn_fract)) * wt_mature;

// Now loop through to endyr+15
  for (i=endyr+1;i<=endyr+15;i++) {

   // F ABC 
    if (spawn_biom_proj(i)/B40 > 1.) {
      FABC_proj = F40;
      FOFL_proj = F35; }
    else {
      FABC_proj = F40 * (spawn_biom_proj(i)/B40 - 0.05)/(1 - 0.05); 
      FOFL_proj = F35 * (spawn_biom_proj(i)/B40 - 0.05)/(1 - 0.05); }
    for (j=1;j<=nages_M;j++) {  
      FOFL_tot_proj(j) = fish_sel4(j) * FOFL_proj;
      FABC_tot_proj(j) = fish_sel4(j) * FABC_proj;
      Z_proj(j) = FABC_tot_proj(j) + natmort;
      ZOFL_proj(j) = FOFL_tot_proj(j) + natmort;
      S_proj(j) = mfexp(-1.0 * Z_proj(j));
    }

   // Catch 
    for (j=1;j<=nages_M;j++) { 
      catage_proj(i,j) = yieldratio * N_proj(i,j) * FABC_tot_proj(j) / Z_proj(j) * (1.-S_proj(j));
      catage_proj_OFL(i,j) = yieldratio * N_proj(i,j) * FOFL_tot_proj(j) / ZOFL_proj(j) * (1. - mfexp(-ZOFL_proj(j)));
    }
    pred_catch_proj(i) = catage_proj(i) * wt / yieldratio;
    pred_catch_proj_OFL(i) = catage_proj_OFL(i) * wt / yieldratio;
   
   // Next year's abundance
    if (i < endyr + 15) {
     if (mceval_phase()) {
       stdev_rec = sqrt(norm2(value(log_rec_dev(1977+recage,endyr-recage))-mean(value(log_rec_dev(1977+recage,endyr-recage))))/(size_count(value(log_rec_dev(1977+recage,endyr-recage)))-1));
       k=round(value(spawn_biom(endyr)*10000))+i;
       k=k+i;
       N_proj(i+1,1)= mfexp((log(mean(value(pred_rec(1977+recage,endyr-recage))))-square(stdev_rec)/2+stdev_rec*randn(k+l))); }
     else {
       N_proj(i+1,1)= value(mean(pred_rec(1979,endyr-recage))); }
     for (j=1; j<nages_M-1;j++) {
       N_proj(i+1,j+1) = N_proj(i,j) * mfexp(-yieldratio*FABC_tot_proj(j)-natmort); }
     N_proj(i+1,nages_M) = N_proj(i,nages_M-1) * mfexp(-yieldratio*FABC_tot_proj(nages_M-1)-natmort)+ N_proj(i,nages_M) * mfexp(-yieldratio*FABC_tot_proj(nages_M)-natmort);
     spawn_biom_proj(i+1) = elem_prod(N_proj(i+1),pow(mfexp(-yieldratio*FABC_tot_proj-natmort),spawn_fract)) * wt_mature;
     tot_biom_proj(i+1) = N_proj(i+1)*wt;
     }
    }

// Set up control rules for harvest rates
  if (spawn_biom_proj(endyr+1)/B40 > 1.) {
      FABC = F40;
      FOFL = F35; 
      FABC2 = F40;
      FOFL2 = F35;
  }
  else {
      FABC = F40 * (spawn_biom_proj(endyr+1)/B40 - 0.05)/(1 - 0.05); 
      FOFL = F35 * (spawn_biom_proj(endyr+1)/B40 - 0.05)/(1 - 0.05);  
      FABC2 = F40 * (spawn_biom_proj(endyr+2)/B40 - 0.05)/(1 - 0.05); 
      FOFL2 = F35 * (spawn_biom_proj(endyr+2)/B40 - 0.05)/(1 - 0.05);
  }

// Projected ABC and OFL
  OFL = pred_catch_proj_OFL(endyr+1);
  ABC = pred_catch_proj(endyr+1);

//==============================================================================================================================
FUNCTION Evaluate_Objective_Function 
//==============================================================================================================================

  Like.initialize();

// Call functions to compute data likelihood
  Catch_Like();                                                // Catch biomass likelihood (lognormal)
  Surv_Like();                                                 // Trawl survey biomass likelihood (lognormal)
  Size_Age_Like();                                             // Age/Size composition likelihood (multinomial)
  Maturity_Like();                                             // Maturity proportion likelihood (binomial)

// Call functions to compute prior penalties
  Calc_Priors();                                               // Prior penalties for estimated parameters
  Rec_Like();                                                  // Penalty function for recruitment
  F_Like();                                                    // Penalty function for fishing mortality deviations

// Sum objective function
  Like              += ssqcatch ;
  Like              += sum(surv_like);
  Like              += cpue_like;
  Like              += sum(age_like);
  obj_fun           += Like;
  obj_fun           += Like_L;
  obj_fun           += Like_C;
  obj_fun           += sum(priors);
  obj_fun           += wt_rec_var * rec_like;
  if(active(log_F_devs))
    obj_fun         += F_mort_regularity;
  if (current_phase()<3)
      obj_fun       += norm2(F);   
  if (active(mF50)&&last_phase())
    obj_fun         += sprpen;                                 // To solve for the F40 etc.     

//==============================================================================================================================
FUNCTION Catch_Like
//==============================================================================================================================

  ssqcatch.initialize();

// Calculate likelihood for catch biomass
  ssqcatch  +=  wt_ssqcatch *norm2(log(obs_catch_early+.00001)-log(pred_catch_early+.00001));
  ssqcatch  +=  wt_ssqcatch2 *norm2(log(obs_catch_later+.00001)-log(pred_catch_later+.00001));

//==============================================================================================================================
FUNCTION Surv_Like
//==============================================================================================================================

  surv_like.initialize();
  cpue_like.initialize();

// Calculate likelihood for trawl survey biomass
  for (i=1; i<=nyrs_srv1; i++)
   //surv_like(1) += square(obs_srv1_biom(i)-pred_srv1(i) )/ (2.*square(obs_srv1_se(i)));
   surv_like(1) += square((log(obs_srv1_biom(i))-log(pred_srv1(i)) ))/ (2.*square(obs_srv1_se(i)/obs_srv1_biom(i)));

// Calculate likelihood for alternate survey index
  if(nyrs_srv2>0) { 
    for (i=1; i<=nyrs_srv2; i++)
     surv_like(2) += square((log(obs_srv2_biom(i))-log(pred_srv2(i)) ))/ (2.*square(obs_srv2_se(i)/obs_srv2_biom(i)));
  }

// Multiply survey index likelihoods by weights
  surv_like(1) *= wt_srv1 ;  
  surv_like(2) *= wt_srv2 ;  

// Calculate likelihood for CPUE index
  if (nyrs_cpue>0)
    cpue_like  = norm2(log(obs_cpue)-log(pred_cpue)) / (2.*cv_cpue*cv_cpue); // likelihood for fishery cpue

//==============================================================================================================================
FUNCTION Size_Age_Like
//==============================================================================================================================

  age_like.initialize();

// Calculate multinomial likelihoods for survey age, fishery size, and survey size and subtract "offset"
  for (i=1; i <= nyrs_fish_age; i++)
    age_like(1) -= nmulti_fish_age(i) * ((oac_fish(i) + 0.00001) * log(eac_fish(i) + 0.00001));
  age_like(1)   -= offset(1);

  for (i=1; i <= nyrs_srv1_age; i++)
    age_like(2) -= nmulti_srv1_age(i) * ((oac_srv1(i) + 0.00001) * log(eac_srv1(i) + 0.00001));
  age_like(2)   -= offset(2);

  for (i=1; i <= nyrs_fish_size; i++)
    age_like(3) -= nmulti_fish_size(i) * ((osc_fish(i) + 0.00001) * log(esc_fish(i) + 0.00001));
  age_like(3)   -= offset(3);

  for (i=1; i <= nyrs_srv1_size; i++)
    age_like(4) -= effn_srv1_size(i) * ((osc_srv1(i) + 0.00001) * log(esc_srv1(i) + 0.00001));
  age_like(4)   -= offset(4);

 if(nyrs_srv2>0) {
  for (i=1; i <= nyrs_srv2_size; i++)
    age_like(5) -= effn_srv2_size(i) * ((osc_srv2(i) + 0.00001) * log(esc_srv2(i) + 0.00001));
  age_like(5)   -= offset(5);
 }

// Multiple each likelihood by their weights from .ctl file
  age_like(1) *= wt_fish_age;
  age_like(2) *= wt_srv1_age;
  age_like(3) *= wt_fish_size;
  age_like(4) *= wt_srv1_size;
  age_like(5) *= wt_srv2_size;

//==============================================================================================================================
FUNCTION Maturity_Like
//==============================================================================================================================

  Like_L.initialize();
  Like_C.initialize();

// Calculate likelihood for maturity proportions
  for (int i=1;i<=nages_mat;i++){
   if(L_tot_na(i)>0){
    Like_L_vec(i) = (log(L_mat_na(i)+k)+gammln(L_mat_na(i)+k)+log((L_tot_na(i)-L_mat_na(i))+k)+gammln((L_tot_na(i)-L_mat_na(i))+k)-log(L_tot_na(i)+k)-gammln(L_tot_na(i)+k)-(L_mat_na(i)+k)*log(Pred_pmat(i)+k)-((L_tot_na(i)-L_mat_na(i))+k)*log((1-Pred_pmat(i))+k));}
   if(C_tot_na(i)>0){
    Like_C_vec(i) = (log(C_mat_na(i)+k)+gammln(C_mat_na(i)+k)+log((C_tot_na(i)-C_mat_na(i))+k)+gammln((C_tot_na(i)-C_mat_na(i))+k)-log(C_tot_na(i)+k)-gammln(C_tot_na(i)+k)-(C_mat_na(i)+k)*log(Pred_pmat(i)+k)-((C_tot_na(i)-C_mat_na(i))+k)*log((1-Pred_pmat(i))+k));}}
  zero_pen_mat = 1/(1+exp(-mat_delta*(0-mat_a50)));
  Like_L = sum(Like_L_vec);
  Like_C = sum(Like_C_vec) + 1000*zero_pen_mat;

//==============================================================================================================================
FUNCTION Calc_Priors
//==============================================================================================================================

// Calculate prior penalties
    priors.initialize();
    if (active(sigr1))
      priors(1)    = square(log(sigr1/sigrprior))/(2.*square(cvsigrprior)) + square(log(sigr2/sigrprior))/(2.*square(cvsigrprior)) + square(log(sigr3/sigrprior))/(2.*square(cvsigrprior));
    if (active(log_q_srv1))
      priors(2)    = square(log_q_srv1-log_q_srv1prior)/(2.*square(cvq_srv1prior));
    if (active(logm))
      priors(3)    = square(logm-log(mprior))/(2.*square(cvmprior));
    if (active(log_q_srv2))
      priors(4)    = square(log_q_srv2-log_q_srv2prior)/(2.*square(cvq_srv2prior));

//==============================================================================================================================
FUNCTION Rec_Like
//==============================================================================================================================

  rec_like.initialize();

  rec_like = norm2(rec_devs_early)/(2*square(sigr1)) + (size_count(rec_devs_early)*log(sigr1)) + norm2(rec_devs_mid)/(2*square(sigr2)) + (size_count(rec_devs_mid)*log(sigr2)) + norm2(rec_devs_late)/(2*square(sigr3)) + (size_count(rec_devs_late)*log(sigr3));

//==============================================================================================================================
FUNCTION F_Like
//==============================================================================================================================

  F_mort_regularity.initialize();

  if(active(log_F_devs))
    F_mort_regularity  = wt_fmort_reg * norm2(log_F_devs);

//==============================================================================================================================
FUNCTION double round(double r) 
//==============================================================================================================================

    return double((r > 0.0) ? floor(r + 0.5) : ceil(r - 0.5)); 

//==============================================================================================================================
REPORT_SECTION
//==============================================================================================================================

  cout<<"-------------Finished: "<<current_phase()<<" "<<Like<<" "<<age_like<<endl;

// Beginning of all outputting
// Goes to routine that automatically creates input file for projection model
  if (last_phase())
    write_proj();

// Output file (.rep) which is loaded into R for data outputs
  report<<"~~~~~~~ Executive Summary Material ~~~~~~~"<<endl;report<<endl;
  report<<"     Model name"<<endl;
  report<<model_name<<endl;
  report<<"     .dat file"<<endl;
  report<<data_file<<endl;
  report<<"     Number parameters estimated"<<endl;
  report<<initial_params::nvarcalc()<<endl;
  report<<"     TotalBiomass for "<<endyr+1<<endl;
  report<<N_proj(endyr+1)*wt<<endl;
  report<<"     TotalBiomass for "<<endyr+2<<endl;
  report<<N_proj(endyr+2)*wt<<endl;
  report<<"     Female_Spawning Biomass for "<<endyr+1<<endl;
  report<<spawn_biom_proj(endyr+1)<<endl;
  report<<"     Female_Spawning_Biomass for "<<endyr+2<<endl;
  report<<spawn_biom_proj(endyr+2)<<endl;
  report<<"     B_zero"     <<endl;
  report<<SB0*mean(pred_rec(1977+recage,endyr-recage))<<endl;
  report<<"     B_40"<<endl;
  report<<B40<<endl;
  report<<"     B_35"<<endl;
  report<<SBF35*mean(pred_rec(1977+recage,endyr-recage))<<endl;
  report<<"     F_40"<<endl;
  report<<F40<<endl;
  report<<"     F_35"<<endl;
  report<<F35<<endl;
  report<<"     F_ABC for "<<endyr+1<<endl;
  report<<FABC<<endl;
  report<<"     F_ABC for "<<endyr+2<<endl;
  report<<FABC2<<endl;
  report<<"     ABC for "<<endyr+1<<endl;
  report<<pred_catch_proj(endyr+1)<<endl;
  report<<"     ABC for "<<endyr+2<<endl;
  report<<pred_catch_proj(endyr+2)<<endl;
  report<<"     F_OFL for "<<endyr+1<<endl;
  report<<FOFL<<endl;
  report<<"     F_OFL for "<<endyr+2<<endl;
  report<<FOFL2<<endl;
  report<<"     OFL for "<<endyr+1<<endl;
  report<<OFL<<endl; 
  report<<"     OFL for "<<endyr+2<<endl;
  report<<pred_catch_proj_OFL(endyr+2)<<endl; 
  report<<"     Total likelihood"<<endl;
  report<<obj_fun<<endl;
  report<<"     Data likelihood"<<endl;
  report<<Like<<endl;report<<endl;
  
  report<<"~~~~~~~ Some more key parameter estimates ~~~~~~~"<<endl;report<<endl;
  report<<"   q_trawl   "<<endl;
  report<<q_srv1<<endl;
  report<<"   nat_mort  "<<endl;
  report<<natmort<<endl;
  report<<"  sigr (styr-1977)  "<<endl;  
  report<<sigr1<<endl;  
  report<<"  sigr (1978-1999)  "<<endl;  
  report<<sigr2<<endl;  
  report<<"  sigr (2000-endyr)  "<<endl;  
  report<<sigr3<<endl;  
  report<<"   log_mean_rec1 (styr-1977)"<<endl;
  report<<log_mean_rec1<<endl;
  report<<"   log_mean_rec2 (1978-1999)"<<endl;
  report<<log_mean_rec2<<endl;
  report<<"   log_mean_rec3 (2000-endyr)"<<endl;
  report<<log_mean_rec3<<endl;
  report<<"   q_alt  "<<endl;
  report<<q_srv2<<endl;report<<endl;

  report<<"~~~~~~~ Rest of model/data output ~~~~~~~"<<endl;report<<endl;
  report << "Year "<< yy <<endl;
  report << "Pred_Catch "<< pred_catch_early<<pred_catch_later <<endl;
  report << "Obs_Catch "<< obs_catch_early<<obs_catch_later <<endl;report<<endl;
  report << "Catch_at_age "<<aa <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<catage(i) <<endl; report<<endl;
  report << "Numbers "<<aa <<endl;
  for (i=styr;i<=endyr;i++) report << i<<" "<<natage(i) <<endl; report<<endl;
  if (nyrs_cpue>0) {
    report <<"Years_CPUE: "     <<yrs_cpue <<endl; 
    report <<"Predicted_CPUE: " <<pred_cpue<<endl; 
    report <<"observed_CPUE: "  <<obs_cpue <<endl; 
    report <<"q_CPUE: "         <<q_cpue   <<endl; 
  }
  report << "Obs_P_fish_age"<<aa_D <<endl;
  for (i=1;i<=nyrs_fish_age;i++) report << yrs_fish_age(i)<<" "<<oac_fish(i) <<endl; report<<endl;
  report << "Pred_P_fish_age"<<aa_D <<endl;
  for (i=1;i<=nyrs_fish_age;i++) report << yrs_fish_age(i)<<" "<<eac_fish(i) <<endl; report<<endl;
  report << "yrs_fish_age nmulti_fish_age effn_fish_age sdnr_fish_age" <<endl;
  for (i=1;i<=nyrs_fish_age;i++) report <<yrs_fish_age(i)<<" "<<nmulti_fish_age(i)<<" "<<effn_fish_age(i)<<" "<<sdnr_fish_age(i)<<endl; report<<endl;
  report << "Obs_P_fish_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish_size;i++) report << yrs_fish_size(i)<<" "<<osc_fish(i) <<endl; report<<endl;
  report << "Pred_P_fish_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_fish_size;i++) report << yrs_fish_size(i)<<" "<<esc_fish(i) <<endl; report<<endl;
  report << "yrs_fish_size nmulti_fish_size effn_fish_size sdnr_fish_size" <<endl;
  for (i=1;i<=nyrs_fish_size;i++) report <<yrs_fish_size(i)<<" "<<nmulti_fish_size(i)<<" "<<effn_fish_size(i)<<" "<<sdnr_fish_size(i)<<endl; report<<endl;
  report << "Obs_P_srv1_age"<<aa_D <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) report << yrs_srv1_age(i)<<" "<<oac_srv1(i) <<endl; report<<endl;
  report << "Pred_P_srv1_age"<<aa_D <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) report << yrs_srv1_age(i)<<" "<<eac_srv1(i) <<endl; report<<endl;
  report << "yrs_srv1_age nmulti_srv1_age effn_srv1_age sdnr_srv1_age" <<endl;
  for (i=1;i<=nyrs_srv1_age;i++) report <<yrs_srv1_age(i)<<" "<<nmulti_srv1_age(i)<<" "<<effn_srv1_age(i)<<" "<<sdnr_srv1_age(i)<<endl; report<<endl;
  report << "Obs_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" "<<osc_srv1(i) <<endl; report<<endl;
  report << "Pred_P_srv1_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv1_size;i++) report << yrs_srv1_size(i)<<" "<<esc_srv1(i) <<endl; report<<endl;
  report << "Obs_P_srv2_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "<<osc_srv2(i) <<endl; report<<endl;
  report << "Pred_P_srv2_size"<<len_bin_labels <<endl;
  for (i=1;i<=nyrs_srv2_size;i++) report << yrs_srv2_size(i)<<" "<<esc_srv2(i) <<endl; report<<endl;
  report << "Bottom Trawl Survey Biomass " <<endl;
  report << "Year:     " << yrs_srv1  <<endl;
  report << "Predicted:   " << pred_srv1  <<endl;
  report << "Observed:   " << obs_srv1_biom  <<endl;
  report << "Observed_SE:   " << obs_srv1_se  <<endl<< endl;
  report << "Alternative Survey Biomass " <<endl;
  report << "Year:     " << yrs_srv2  <<endl;
  report << "Predicted:   " << pred_srv2  <<endl;
  report << "Observed:   " << obs_srv2_biom  <<endl;
  report << "Observed_SE:   " << obs_srv2_se  <<endl<< endl;
  report << "Year " << yy<< endl;
  report << "SpBiom "<< spawn_biom <<endl;
  report << "Tot_biom "<< tot_biom   <<endl;
  report << "Recruitment "<< pred_rec   <<endl;
  report << "Fully_selected_F "<< Fmort*max(fish_sel4) <<endl<<endl;;
  report << "Age  "<<aa<< endl;
  report << "Weight "<< wt << endl;
  report << "Maturity "<<p_mature<<endl;
  report << "Fishery_Selectivity_1967-1976" << fish_sel1  <<endl;
  report << "Fishery_Selectivity_1977-1995" << fish_sel2  <<endl;
  report << "Fishery_Selectivity_1996-2006" << fish_sel3  <<endl;
  report << "Fishery_Selectivity_2007-"<<endyr << fish_sel4  <<endl;
  report << "Bottom_Trawl_Survey_Selectivity " << srv1_sel / max(srv1_sel) <<endl;
  report << "Alternative_Survey_Selectivity " << srv2_sel / max(srv2_sel) <<endl<<endl;
  report << "F35 F40 F50 "<<endl;
  report <<  F35 << " "<< F40 <<" "<<  F50 <<endl;report<<endl;
  report << "~~~~~~~ Wts and Likelihoods ~~~~~~~"<< endl;report<<endl;
  report << wt_ssqcatch <<" "<<ssqcatch     <<" " ; report << "SSQ Catch Likelihood" << endl;
  report << wt_srv1 <<" "<<surv_like(1) <<" " ; report << "Bottom Trawl Survey Likelihood" << endl;
  report << wt_fish_age <<" "<<age_like(1)  <<" " ; report << "Fishery Age Composition Likelihood"  << endl;
  report << wt_srv1_age <<" "<<age_like(2)  <<" " ; report << "Bottom Trawl Survey Age Composition Likelihood" << endl;
  report << wt_fish_size <<" "<<age_like(3)  <<" " ; report << "Fishery Size Composition Likelihood" << endl;
  report << wt_cpue <<" "<<cpue_like    <<" " ; report << "Fishery CPUE Likelihood" << endl;
  report << wt_srv2 <<" "<<surv_like(2) <<" " ; report << "Alternative Survey Abundance Index Likelihood"   << endl;
  report << wt_srv1_size <<" "<<age_like(4)  <<" " ; report << "Bottom Trawl Survey Size Composition Likelihood" << endl;
  report << wt_srv2_size <<" "<<age_like(5)  <<" " ; report << "Alternative Survey Size Composition Likelihood" << endl;
  report << wt_rec_var <<" "<<rec_like     <<" " ; report << "Recruitment Deviations Likelihood" << endl;
  report << wt_fmort_reg <<" "<<F_mort_regularity<<" " ; report << "Fishing Mortality Deviations Penalty" << endl;
  report << 1 <<" "<<priors(1)  <<" " ; report << "Priors SigmaR" <<endl;
  report << 1 <<" "<<priors(2)  <<" " ; report << "Priors q Bottom Trawl Survey" <<endl;
  report << 1 <<" "<<priors(3)  <<" " ; report << "Priors M" <<endl;
  report << 1 <<" "<<priors(4)  <<" " ; report << "Priors q Alternative Survey" <<endl;
  report << " "<<obj_fun    <<" " ; report << "Objective Function" <<endl;
  report << " "<<Like       <<" " ; report << "Data Likelihood" <<endl;report<<endl;
  report<<"~~~~~~~ Projection outputs ~~~~~~~"<<endl;report<<endl;
  report << "N_at_age projected "<<endl<<N_proj<<endl<<" spawn_bio projected"<<endl<<spawn_biom_proj<<endl;

//==============================================================================================================================
FUNCTION double sdnr(const dvar_vector& pred,const dvector& obs,double m)
//==============================================================================================================================

  RETURN_ARRAYS_INCREMENT();
  double sdnr;
  dvector pp = value(pred);
  int ntmp = -obs.indexmin()+obs.indexmax();
  sdnr = std_dev(elem_div(obs-pp,sqrt(elem_prod(pp,(1.-pp))/m)));
  RETURN_ARRAYS_DECREMENT();
  return sdnr;

//==============================================================================================================================
FUNCTION write_proj
//==============================================================================================================================

 ofstream newproj("proj.dat");
// Function to write out data file for new Ianelli 2005 projection model....
 newproj <<"#Species name here:"<<endl;
 newproj <<model_name+"_"+data_file<<endl;
 newproj <<"#SSL Species?"<<endl;
 newproj <<"0"<<endl;
 newproj <<"#Constant buffer of Dorn?"<<endl;
 newproj <<"0"<<endl;
 newproj <<"#Number of fisheries?"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#Number of sexes?"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#5year_Average_F(endyr-4,endyr_as_estimated_by_ADmodel)"<<endl;
 newproj << mean(Fmort(endyr-4,endyr))<<endl;
 newproj <<"#_Author_F_as_fraction_F_40%"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#ABC SPR" <<endl;
 newproj <<"0.4"<<endl;
 newproj <<"#MSY SPR" <<endl;
 newproj <<"0.35"<<endl;
 newproj <<"#_Spawn_month"<<endl;
 newproj << spawn_fract*12+1<<endl;
 newproj <<"#_Number_of_ages"<<endl;
 newproj <<nages_M<<endl;
 newproj <<"#_F_ratio(must_sum_to_one_only_one_fishery)"<<endl;
 newproj <<"1"<<endl;
 for (j=1;j<=nages_M;j++) natmortv = natmort; 
 newproj <<"#_Natural_Mortality" << aa << endl;
 newproj <<natmortv<<endl;
 newproj <<"#_Maturity_divided_by_2(projection_program_uses_to_get_female_spawning_biomass_if_divide_by_2"<<aa<<endl<<p_mature<< endl;
 newproj <<"#_Wt_at_age_spawners"<<aa<<endl<<wt<< endl;
 newproj <<"#_Wt_at_age_fishery" <<aa<<endl<<wt<< endl;
 newproj <<"#_Selectivity_fishery_scaled_to_max_at_one"<<aa<<endl<<fish_sel4/max(fish_sel4)<< endl;
 newproj <<"#_Numbers_at_age_end_year"<<aa<<endl<<natage(endyr)<< endl;
 newproj <<"#_N_recruitment_years"<<endl<<endyr-recage-1979+1<< endl;
 newproj <<"#_Recruitment_start_at_1977_yearclass=1979_for_age_2_recruits"<<yy(1979,endyr-recage)<<endl<<pred_rec(1979,endyr-recage)<< endl;
 newproj <<"#_Spawners per recruitment (starting at 1977)"<<endl<<spawn_biom(1977,endyr-recage)/1000<< endl;
 newproj.close();

//==============================================================================================================================
GLOBALS_SECTION
//==============================================================================================================================

 # include "admodel.h"
  adstring model_name;
  adstring data_file;
TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(100000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(500);
