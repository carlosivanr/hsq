/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

libname = “Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Datasets”

Predictor file = bldat_0726
Outcome file = outcome_analysis100826

*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* Map a library to a directory name ----------------------------------------*/
/* Sets the path to the directory containing the baseline data for covariates
and the outcomes data. */
libname jun "Z:/Shared/DFM/HSQ_Shared/Analysis/Stat Working/Datasets";

/* Due to a coding bug, tsrq_amot is invalid in the BLDAT_0726 file. This code
chunk loads the correct values as tsrq_amot_score*/
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;

proc import datafile="&root./odds_of_smoking/odds_of_smoking_by/tsrq_amot_score_CR.csv"
    out = tsrq_amot_score
    dbms=csv
    replace;
run;

/* ///////////////////////////////////////////////////////////////////////// */
/* ////////////////////////////////  Models //////////////////////////////// */
/* ///////////////////////////////////////////////////////////////////////// */

/* Create a table where the number of characters is predefined */
data num_w_int;
    length PV $20
           DV $20
           Effect $20
           ProbChiSq 8;
    format ProbChiSq pvalue6.4;
    stop;
run;

data cat_w_int;
    length PV $20
           DV $20
           Effect $20
           ProbChiSq 8;
    format ProbChiSq pvalue6.4;
    stop;
run;

/* Loop through the outcomes */
%let dvs = 
  smoke1_SMS_by_3mo_jy
  smoke1_SMS_by_6mo_jy
  smoke1_SMS_by_9mo_jy
  smoke1_SMS_by_12mo_jy
;

%macro fit_models_w_int_all;
%do i = 1 %to %sysfunc(countw(&dvs));
  %let dv = %scan(&dvs, &i);

  /* %let dv = smoke1_SMS_by_3mo_jy; */
  /* Merge the two data sets */
  proc sql;
      create table temp as
      select a.*,
             b.&dv.
      from jun.bldat_0726 as a
      left join jun.outcome_analysis100826 as b
      on a.hsqid = b.hsqid;
  quit;

  /* Merge in the correct tsrq_amot_score */
  proc sql;
  create table temp as
      select a.*,
             b.tsrq_amot_score
      from temp as a
      left join tsrq_amot_score as b
      on a.hsqid = b.hsqid;
  quit;

  
  /* Loop through the variables */
  /* tsrq_amot_score is loaded from a separate file */
  %let pvars = 
    anx_score
    dep_score
    tsrq_amot_score
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

  %do j = 1 %to %sysfunc(countw(&pvars));
    %let pvar = %scan(&pvars, &j);

    /* %let pvar = anx_score; */
    /* Display in the log output the variable */
    %put &=pvar;
    
    /* Need a statment to check if &pvar. is character to fit a class statement */
    data _null_;
      set temp(obs=1);
      call symputx('pvar_type', vtype(&pvar.));
    run;

    /* Display the data type of the predictor variable */
    %put &=pvar_type;
    /* %let pvar_type = C; */

    %if &pvar_type. = C %then %do;
      /* ------------------Categorical predictor work flow------------------ */

      /* Model setting 99: Unknown to missing */
      /* Jun asked to change 99:Unknown to "" to see if there were any
      differences between modifying the data and using the where statement
      within proc logistic but there were none.*/
      /* data temp;
        set temp;
        if &pvar. = "99: Unknown" then &pvar. = "";
      run; */

      ods output
        /* ParameterEstimates=_pe */
        ModelANOVA=_pe
      ;

      /* Drop rows where values of "99: Unknown" are present. This needs to 
      occur on each iteration of a categorical variable. */
      proc logistic data=temp;
        where &pvar. ne "99: Unknown";

        %if &pvar_type. = C %then %do;
          class &pvar. arm(ref = "Control") / param = ref;
        %end;

        model &dv.(event='1') = arm &pvar. arm * &pvar.;
      run;

      /* Create sub table */
      proc sql;
        create table sub_tab_1 as
        select
          "&pvar." as PV,
          "&dv." as DV,
          Effect,
          ProbChiSq
        from _PE    
      quit;

      /* Append to a master table  */
      proc append base=cat_w_int data= sub_tab_1 force;
      run;

    %end;
    %else %if &pvar_type. = N %then %do;
      /* -------------------Numerical predictor workflow------------------- */
      ods output
        ModelANOVA=_pe
      ;

      proc logistic data=temp;
        class arm(ref = "Control");
        model &dv.(event='1') = arm &pvar. arm*&pvar.;
      run;

    /* Create sub table*/
    proc sql;
      create table sub_tab_2 as
      select
        "&pvar." as PV,
        "&dv." as DV,
        Effect,
        ProbChiSq
      from _PE;
    quit;

    /* Append to a master table  */
    proc append base=num_w_int data= sub_tab_2 force;
    run;

    %end;



  %end;

%end;

%mend;

%fit_models_w_int_all;

/* Output to .csv files */
proc export data=cat_w_int
    outfile="&root./odds_of_smoking/odds_of_smoking_w_int/cat_w_int_all.csv"
    dbms=csv
    replace;
run;

proc export data=num_w_int
    outfile="&root./odds_of_smoking/odds_of_smoking_w_int/num_w_int_all.csv"
    dbms=csv
    replace;
run;


