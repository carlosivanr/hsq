/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

HSQ - Bivariate associations between baseline predictors and relapse outcomes
by the quarterly time points. The objective is to determine which predictors
might be included in adjusted models to be reported in a primary outcomes
manuscript.

The following script is designed to perform bivariate, unadjusted models using
the "by" outcome variables as dependent variables. For example, relapse by 3m
is a binary outcome indicating whether or not someone has relapsed by the 
3month timepoint in the study. These variables are names smoke1_SMS_by_Xmo to
indicate that the variable is created from smoke1 variable in the SMS data.

All predictor variables are from the baseline timepoint. Thus the models test
whether predictor at baseline is associated with outcome within a given time
point in the study.

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

/* Create a table where the number of characters is predefined */
data num_unadjusted;
    length Effect $20;
    stop;
run;

data cat_unadjusted;
    length Effect $20;
    stop;
run;

/* Loop through the outcomes */
%let dvs = 
  smoke1_SMS_by_3mo_jy
  smoke1_SMS_by_6mo_jy
  smoke1_SMS_by_9mo_jy
  smoke1_SMS_by_12mo_jy
;

/* %let dv = smoke1_SMS_by_3mo_jy */

%macro fit_unadjusted_models;
%do i = 1 %to %sysfunc(countw(&dvs));
  %let dv = %scan(&dvs, &i);

  /* Merge the two data sets */
  proc sql;
      create table temp as
      select a.*,
          b.&dv.
      from jun.bldat_0726 as a
      left join jun.outcome_analysis100826 as b
      on a.hsqid = b.hsqid;
  quit;
  
  /* Loop through the variables */
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
  /* %let pvar = Wake4_SI9_jy;
     %let pvar = anx_score;
  */

  %do j = 1 %to %sysfunc(countw(&pvars));
    %let pvar = %scan(&pvars, &j);

    /* Display in the log output the variable */
    %put &=pvar;
    
    /* Need a statment to check if &pvar. is character to fit a class statement */
    data _null_;
      set temp(obs=1);
      call symputx('pvar_type', vtype(&pvar.));
    run;

    /* Display the data type of the predictor variable */
    %put &=pvar_type;

    %if &pvar_type. = C %then %do;
      /* ------------------Categorical predictor work flow------------------ */
      
      /* Drop rows where values of "99: Unknown" are present */
      data temp;
          set temp;
          if vvalue(&pvar.) ne "99: Unknown";
      run;
      
      ods output
        /* ParameterEstimates=_pe */
        ModelANOVA=_pe
      ;

      proc logistic data=temp;
        %if &pvar_type. = C %then %do;
          class &pvar.;
        %end;

        model &dv.(event='1') = &pvar.;
      run;

    /* Add the identifying dependent variable and month */
    /* Sub table 2 if predictor is numeric*/
    proc sql;
      create table sub_tab_1 as
      select
        Effect,
        "&dv." as DV,
        ProbChiSq
      from _PE    
    quit;

    /* Append to a master table  */
    proc append base=cat_unadjusted data= sub_tab_1 force;
    run;

    %end;
    %else %if &pvar_type. = N %then %do;
      /* -------------------Numerical predictor workflow------------------- */

      ods output
        ParameterEstimates=_pe
        OddsRatios=_or
      ;

      proc logistic data=temp;
        model &dv.(event='1') = &pvar.;
      run;

    /* Add the identifying dependent variable and month */
    /* Sub table 2 if predictor is numeric*/
    proc sql;
      create table sub_tab_2 as
      select
        Variable as Effect,
        "&dv." as DV,
        ProbChiSq
      from _PE 
      where Variable = "&pvar.";
    quit;


    /* Combine the sub tables */
    proc sql;
        create table sub_tab_2 as
        select a.*,
               b.OddsRatioEst,
               b.LowerCL,
               b.UpperCL
        from sub_tab_2 as a

        left join _OR as b
            on a.Effect = b.Effect
        ;
    quit;

    %end;

  /* Append to a master table  */
    proc append base=num_unadjusted data= sub_tab_2 force;
    run;

  %end;

%end;

%mend;

%fit_unadjusted_models;

/* Output to .csv files */
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;

proc export data=CAT_UNADJUSTED
    outfile="&root./odds_of_smoking/odds_of_smoking_by/cat_unadjusted.csv"
    dbms=csv
    replace;
run;

proc export data=NUM_UNADJUSTED
    outfile="&root./odds_of_smoking/odds_of_smoking_by/num_unadjusted.csv"
    dbms=csv
    replace;
run;


/* ///////////////////////////////////////////////////////////////////////// */
/* ///////////////      Number of Helping Conversations      /////////////// */
/* ///////////////////////////////////////////////////////////////////////// */

/* Set a variable to a the root data directory */
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;


/* Import data ------------------------------------------------------------- */
/* Import the quarterly number of helping conversations and cessation fatigue
  data file */
proc import datafile = "&root./data/quarterly_hcs_cf.csv"
    out = n_help_conv_sms
    dbms = csv
    replace;
run;


/* Loop through the outcomes */
%let dvs = 
  smoke1_SMS_by_3mo_jy
  smoke1_SMS_by_6mo_jy
  smoke1_SMS_by_9mo_jy
  smoke1_SMS_by_12mo_jy
;

%let events = 
  3mo
  6mo
  9mo
  12mo
;

 /* 
%let dv = smoke1_SMS_by_3mo_jy;
%let event = 3mo;
*/

%macro fit_unadjusted_n_hcs_models;
%do i = 1 %to %sysfunc(countw(&dvs));
  %let dv = %scan(&dvs, &i);
  %let event = %scan(&events, &i);

  %put &=dv;
  %put &=event;

  /* Merge the two data sets */
  proc sql;
    create table n_hcs as
      select a.*
    from n_help_conv_sms as a
    where event_name = "&event.";
  quit;

  proc sql;
    create table temp as
    select a.*,
        b.sum_hc
    from jun.outcome_analysis100826 as a
    left join n_hcs as b
    on a.hsqid = b.hsqid;
  quit;

  ods output
  ParameterEstimates=_pe
  OddsRatios=_or
  ;

  proc logistic data=temp;
    model &dv.(event='1') = sum_hc;
  run;

  /* Add the identifying dependent variable and month */
  /* Sub table 2 if predictor is numeric*/
  proc sql;
    create table sub_tab_2 as
    select
      Variable as Effect,
      "&dv." as DV,
      ProbChiSq
    from _PE 
    where Variable = "sum_hc";
  quit;


  /* Combine the sub tables */
  proc sql;
    create table sub_tab_2 as
    select a.*,
            b.OddsRatioEst,
            b.LowerCL,
            b.UpperCL
    from sub_tab_2 as a

    left join _OR as b
        on a.Effect = b.Effect
    ;
  quit;

  /* Append to a master table  */
  proc append base=n_hcs_results data= sub_tab_2 force;
  run;
  
  %end;

%mend fit_unadjusted_n_hcs_models;

%fit_unadjusted_n_hcs_models;

proc export data=n_hcs_results
    outfile="&root./odds_of_smoking/odds_of_smoking_by/n_hcs_unadjusted.csv"
    dbms=csv
    replace;
run;