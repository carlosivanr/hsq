/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

PH Cox Survival models
HSQ - Bivariate associations between baseline predictors and time to event
outcomes by the quarterly time points. The objective is to determine which
predictors might be included in adjusted models to be reported in a primary
outcomes manuscript.

The following script is designed to perform bivariate, unadjusted models using
the time to event variables as dependent variables.

The purpose of this script is to perform bi-variate associations between the
outcomes:
      surv1_week_jy
      surv11_week_jy

and all the baseline predictor variables.

libname = “Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Datasets”

Predictor file = bldat_0726
Outcome file = outcome_analysis100826

*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* Map a library to a directory name ----------------------------------------*/
/* Sets the path to the directory containing the baseline data for covariates
and the outcomes data. */
libname jun "Z:/Shared/DFM/HSQ_Shared/Analysis/Stat Working/Datasets";

/* ///////////////////////////////////////////////////////////////////////// */
/* /////////////////////////// Unadjusted Models /////////////////////////// */
/* ///////////////////////////////////////////////////////////////////////// */

/* Create tables where the number of characters is predefined. These tables are
created to collect output from a for loop. Since output from continuous and 
categorical variables differ, two tables are created to collect each output 
type. */
data num_unadjusted;
    length Parameter $20
           DV $16           
           HazardRatio 8
           HRLowerCL 8
           HRUpperCL 8
           ProbChiSq 8;
    stop;
run;

data cat_unadjusted;
    length Effect $20
           DV $16
           ProbChiSq 8;
    stop;
run;

/* List all of the predictor variables.*/
%let pvars = 
    anx_score
    dep_score
    tsrq_amot
    density
    n_components
    btw_centr
    deg_centr
    homophily_gender
    homophily_age_cat
    perc_supp_quit
    rate_complete
    rate_complete_SMS
    rate_complete_MSurvey
    insurance6_jy
    age3_jy
    gender3_jy
    race7_jy
    HISP3_jy
    Education3_jy
    Cigarettes_PDay10_jy
    Special_Program2_jy
    NRT2_jy
    BUP_Varenicline2_jy
    eCigarette2_SI5F_jy
    Smoke3_SI6a_jy
    menthol2_OI7a1_jy
    Wake4_SI9_jy
    ProtocolType2_jy
    Age_Tobacco4_jy
    se1c_jy
    se2c_jy
    se3c_jy
    se4c_jy
    se5c_jy
    cd_attraction_jy
    cd_smoker_jy
    cd_vulnerable_jy
    SE6_jy
    SE7_jy
    SE8_jy
    SE9_jy
    SE10_jy
    SE11_jy
    SE12_jy
    SE_skill_jy
    SE_confi_jy
    SQM1_NRT_jy
    SQM2_Rx_jy
    SQM3_App_jy
    SQM4_Web_jy
    SQM5_Coach_jy
    SQM6_SG_jy
    SQM7_EDU_jy
    SQM8_Ecig_jy
    SQM9_Oth_jy
    SQM10_None_jy
    LGBT_jy
  ;

/* Use during development to test predictor variables. */
/* %let pvar = anx_score; */

/* %let pvars = 
  anx_score
  insurance6_jy
; */

%macro fit_ph_cox_models;
  %do j = 1 %to %sysfunc(countw(&pvars));
    %let pvar = %scan(&pvars, &j);

    /* Display in the log output the variable */
    %put &=pvar;
  

    /* Create a temporary table. The specified categorical variables in the 
    analysis data set were created in a way where missing values are explicitly
    declared as "99: Unknown". Per JY via email communication 2026-10-08, these
    are to be removed from logistic and cox models. This proc sql statement 
    ensures each dataset for each outcome preditor pair are uniquely filtered.*/
    proc sql;
        create table temp as
        select a.*,
               b.*
        from jun.bldat_0726 as a
        left join jun.outcome_analysis100826 as b
        on a.hsqid = b.hsqid;
    quit;


    /* Check if &pvar. is character. If so, a class statement will be needed in
    the model. */
    data _null_;
      set temp(obs=1);
      call symputx('pvar_type', vtype(&pvar.));
    run;

    /* Display the data type of the predictor variable */
    %put &=pvar_type;


    %if &pvar_type. = C %then %do;
  /* ------------------Categorical predictor work flow------------------ */
        
      /* Drop rows where values of "99: Unknown" are present */
      proc sql;
        create table temp as
        select *
        from temp
        where &pvar. ne "99: Unknown";
      quit;

      /* Set the ods output tables to save to extract parameter estimates and
      p values.*/
      ods output
        ModelANOVA=_pe
        ParameterEstimates=_mle
      ;

      /* Categorical model */
      proc phreg data = temp;
          %if &pvar_type. = C %then %do;
            class &pvar.;
          %end;
          
          model surv1_week_jy*event1_jy(0) = &pvar / risklimits=both type3(all);
          
          title "Cox-PH Model (Unadjusted &pvar.)";
      run;

      /* Create a table of the omnibus effect */
      proc sql;
        create table cat_tab_1 as
        select
          Effect,
          "surv1_week_jy" as DV,
          ProbChiSq
        from _PE;
      quit;

      /* If the ProbChiSq values is less than .05, then create a table from _mle
      and add DV and &pvar. */

      /* Retrieve rows where p < 0.05 */
      proc sql noprint;
          select count(*) into :sig_count trimmed
          from cat_tab_1
          where ProbChiSq < 0.05;
      quit;

      /* If the omnibus test for a categorical variable is <0.05, then
      capture the comparisons to the reference level.*/
      %if &sig_count > 0 %then %do;

          proc sql;
              create table cat_tab_1_2 as
              select
                  Parameter,
                  "surv1_week_jy" as DV,
                  *
              from _MLE;
          quit;

          /* Append to master table */
          proc append base=cat_unadjusted_2
                      data=cat_tab_1_2 force;
          run;

      %end;
    

      /* Run the two consecutive relapse */
      ods output
        ModelANOVA=_pe
        ParameterEstimates=_mle
      ;

      proc phreg data = temp;
        %if &pvar_type. = C %then %do;
          class &pvar.;
        %end;
        
        model surv11_week_jy*event11_jy(0) = &pvar / risklimits=both type3(all);
        
        title "Cox-PH Model (Unadjusted &pvar.)";
      run;

      /* Collect tables */
      proc sql;
        create table cat_tab_2 as
        select
          Effect,
          "surv11_week_jy" as DV,
          ProbChiSq
        from _PE;
      quit;

      /* If the ProbChiSq values is less than .05, then create a table from _mle and add DV and &pvar. */
      /* Check if the value is less than 0.05 */
      proc sql noprint;
          select count(*) into :sig_count trimmed
          from cat_tab_2
          where ProbChiSq < 0.05;
      quit;

      /* Execute only if the condition is met */
      %if &sig_count > 0 %then %do;

          proc sql;
              create table cat_tab_2_2 as
              select
                  Parameter,
                  "surv11_week_jy" as DV,
                  *
              from _MLE;
          quit;

          /* Append to master table */
          proc append base=cat_unadjusted_2
                      data=cat_tab_2_2 force;
          run;

      %end;

      /* Append to a master table  */
      proc append base=cat_unadjusted data= cat_tab_1 force;
      run;

      proc append base=cat_unadjusted data= cat_tab_2 force;
      run;

    %end;

    %else %if &pvar_type. = N %then %do;
    /* -------------------Numerical predictor workflow------------------- */
      
      ods output
        ParameterEstimates=_pe
      ;

      proc phreg data = temp;          
          model surv1_week_jy*event1_jy(0) = &pvar / risklimits=both type3(all);
          
          title "Cox-PH Model (Unadjusted &pvar.)";
      run;

      /* Collect tables */
      proc sql;
        create table sub_tab_1 as
        select
          Parameter,
          "surv1_week_jy" as DV,
          HazardRatio,
          HRLowerCL,
          HRUpperCL,
          ProbChiSq
        from _PE; 
      quit;


      /* Run the two consecutive relapse */
      ods output
        ParameterEstimates=_pe
      ;

      proc phreg data = temp;        
        model surv11_week_jy*event11_jy(0) = &pvar / risklimits=both type3(all);
        
        title "Cox-PH Model (Unadjusted &pvar.)";
      run;

      /* Collect tables */
      proc sql;
        create table sub_tab_2 as
        select
          Parameter,
          "surv11_week_jy" as DV,
          HazardRatio,
          HRLowerCL,
          HRUpperCL,
          ProbChiSq
        from _PE; 
      quit;

      /* Append to a master table  */
      proc append base=num_unadjusted data= sub_tab_1 force;
      run;

      proc append base=num_unadjusted data= sub_tab_2 force;
      run;
    %end;  
  %end;
%mend;

%fit_ph_cox_models;


/* Output to .csv files */
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;


proc export data=CAT_UNADJUSTED
    outfile="&root./time_to_relapse/surv1_surv11/cat_unadjusted.csv"
    dbms=csv
    replace;
run;

proc export data=NUM_UNADJUSTED
    outfile="&root./time_to_relapse/surv1_surv11/num_unadjusted.csv"
    dbms=csv
    replace;
run;

proc export data=CAT_UNADJUSTED_2
    outfile="&root./time_to_relapse/surv1_surv11/cat_unadjusted_ref_comps.csv"
    dbms=csv
    replace;
run;

/* Check the reference categories for the categorical variables */
proc freq data=jun.bldat_0726;
    tables
        Cigarettes_PDay10_jy
        eCigarette2_SI5F_jy
        Smoke3_SI6a_jy
        Wake4_SI9_jy
        se2c_jy
        se3c_jy
        se4c_jy
        se5c_jy
        cd_attraction_jy
        cd_smoker_jy
        cd_vulnerable_jy
        LGBT_jy
        / missing;
run;