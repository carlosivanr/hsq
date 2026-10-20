/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

The objective of this sas program is to output two .csv files listing the
results of a series of models.

The first file contains model results of a series of models where the 
dependent variable is smoking status and the predictor is the score of one of
the psychological instruments captured at the baseline timepoint. There are 13
psychological instruments and there are 4 timepoints 3,6,9, and 12 months. This
output thus reports 52 different models in total. The outcome is smoking status
so all output is from a logistic regression model.

The second files contains model results from a series of models where the 
variables are swapped. The psychological instrument scores from the baseline 
timepoint become the dependent variables and the smoking status at 3, 6, 9, and 
12 months become the independent variables. Since two of the psychological 
instrument outcomes are binary, the results are the same as those in the first
series of model for cd_attraction and cd_vulnerable. Thus there are 11 psych
instruments modeled for a given timepoint. Thus a total of 44 model are fitted.
All psychological instruments modeled as continuous outcomes. t-test results
modeled using proc reg.

Psychological Instruments
anx_score - anxiety (PHQ4 sub-domain),
dep_score - depression (PHQ4 sub-domain),
psy_score - psychological distress (PHQ4),
arme_scor - Abstinence Related Motivational Engagement,
se_conf_score - Self Efficacy Confidence,
se_skill_score - Self Efficacy Skills,
pcs_score - Proactive Coping Scale,
cd_attraction - COGDIS residual attraction to smoking,
cd_vulnerable - COGDIS vulnerability to relapse,

Treatment Self Regulation Questionnaire 
tsrq_auto_score - Autonomous regulation,
tsrq_intr_score - Introjected regulation,
tsrq_extr_score - External regulation,
tsrq_amot_score - Amotivation


*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* Map a library to a directory name ----------------------------------------*/
/* Set a variable to a the root data directory */
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;


/* Import data ------------------------------------------------------------- */
* Import 0 month (baseline) survey data file;
proc import datafile = "&root./data/hsq_data_0mo.csv"
    out = hsq_0mo
    dbms = csv
    replace;
run;

* Import 6 month survey data file;
proc import datafile = "&root./data/hsq_data_6mo.csv"
    out = hsq_6mo
    dbms = csv
    replace;
run;

* Import 12 month survey data file;
proc import datafile = "&root./data/hsq_data_12mo.csv"
    out = hsq_12mo
    dbms = csv
    replace;
run;

* Import 3 month survey data file;
proc import datafile = "&root./data/hsq_data_3mo.csv"
    out = hsq_3mo
    dbms = csv
    replace;
run;

* Import 9 month survey data file;
proc import datafile = "&root./data/hsq_data_9mo.csv"
    out = hsq_9mo
    dbms = csv
    replace;
run;

/* Create a table called psy_vars with the variables of interest
for merging into a temporary table in the logistic macro*/
proc sql;
    create table psy_vars as
       select 
           hsqid,
           anx_score,
           dep_score,
           psy_score,
           arme_score,
           se_conf_score,
           se_skill_score,
           pcs_score,
           cd_attraction,
           cd_vulnerable,
           tsrq_auto_score,
           tsrq_intr_score,
           tsrq_extr_score,
           tsrq_amot_score
    from hsq_0mo;
quit;

/* ///////////////////////////////////////////////////////////////////////// */
/*                 Smoking Status Predicted by Psych Variables               */
/* ///////////////////////////////////////////////////////////////////////// */

/* Model fitting macro for the smoking status outcome */
%macro logistic_unadjusted(data=, predictor=, month=);
    
    /* Get the outcome and hsqid from the input data set */
    proc sql;
        create table temp0 as
        select hsqid, 
               fu_smoke1
        from &data.
    quit;

    /* Get the predictor variable from the baseline psy_vars table */
    proc sql;
        create table temp as
        select a.*,
               b.*
        from temp0 as a
        left join psy_vars as b
            on a.hsqid = b.hsqid;
    quit;
    
    ods output
        ParameterEstimates = _mle
        OddsRatios = _or
    ;

    proc logistic data=temp;
        model fu_smoke1(event="Yes") = &predictor.;
    run;

    ods output close;

    /* Drop the intercept row */
    data _mle;
        set _mle;
        where Variable ne "Intercept";
        rename Variable = Effect;
    run;

    /* Merge the MLE and OR tables */
    data _results;
        merge _mle _or;
        by Effect;    

        /* Add identifying variable for month */
        month = &month.;
    run;

    /* Append to a master table  */
    proc append base=all_results data=_results force;
    run;

%mend logistic_unadjusted;


/* /////////////////////////////////////////////////////// */
%macro run_models(timepoint=);

%let predictors =
    anx_score
    dep_score
    psy_score
    arme_score
    se_conf_score
    se_skill_score
    pcs_score
    cd_attraction
    cd_vulnerable
    tsrq_auto_score
    tsrq_intr_score
    tsrq_extr_score
    tsrq_amot_score
;

%do i = 1 %to %sysfunc(countw(&predictors));

    %let predictor = %scan(&predictors, &i);

    title "30-day Odds of reporting smoking at &timepoint months by &predictor";

    %logistic_unadjusted(
        data = hsq_&timepoint.mo,
        predictor = &predictor,
        month = &timepoint
    );

    title;

%end;

%mend;

/* Run this friggin bitch */
%run_models(timepoint=3);
%run_models(timepoint=6);
%run_models(timepoint=9);
%run_models(timepoint=12);


/* Output ALL_RESULTS TO .CSV */
data all_results;
    retain month;
    set all_results;
run;

proc export data=all_results
    outfile="&root./psych_dvs_by_smoking_status/smoking_status_by_psych_ivs.csv"
    dbms=csv
    replace;
run;

/* ///////////////////////////////////////////////////////////////////////// */
/*       Continuous Psych Outcomes Predicted by Smoking Status               */
/* ///////////////////////////////////////////////////////////////////////// */
/* Swap the dependent and independent variables */

/* Macro function to perform t-test as linear model ---------------- */
%macro t_test(data=, outcome=, month=);

    /* Get the predictor and hsqid from the input data set */
    proc sql;
        create table temp0 as
        select hsqid, 
               fu_smoke1
        from &data.
    quit;

    /* Get the outcome variable from the baseline psy_vars table  */
    proc sql;
        create table temp as
        select a.*,
               b.&outcome
        from temp0 as a
        left join psy_vars as b
            on a.hsqid = b.hsqid;
    quit;

    proc means data=temp;
      class fu_smoke1;
      var &outcome ;
    run;

    /* create a new variable smoke to use as the IV */
    data temp;
        set temp;
        if fu_smoke1 = "No" then smoke = 0;
        else if fu_smoke1 = "Yes" then smoke = 1;
        else smoke = .;
    run;
                
    ods output
        ParameterEstimates = _pe
    ;

    proc reg data=temp;
        model &outcome = smoke;
    run;

    ods output close;

    /* Drop the intercept row and add a month variable */
    data _pe;
        set _pe;
        where Variable ne "Intercept";
        month = &month.;
    run;

    /* Append to a master table  */
    proc append base=all_t_test_results data=_pe force;
    run;

%mend t_test;

/* Test the macro */
/* %t_test(data=hsq_6mo, outcome=dep_score, month=6); */


%macro run_t_test_models(timepoint=);
%let outcomes =
    anx_score
    dep_score
    psy_score
    arme_score
    se_conf_score
    se_skill_score
    pcs_score
    tsrq_auto_score
    tsrq_intr_score
    tsrq_extr_score
    tsrq_amot_score
;

%do i = 1 %to %sysfunc(countw(&outcomes));

    %let outcome = %scan(&outcomes, &i);

    title "Mean differences of &outcome by smoking status at &timepoint months";

    %t_test(
        data = hsq_&timepoint.mo,
        outcome = &outcome,
        month = &timepoint
    );

    title;

%end;

%mend;

/* Run this friggin bitch */
%run_t_test_models(timepoint=3);
%run_t_test_models(timepoint=6);
%run_t_test_models(timepoint=9);
%run_t_test_models(timepoint=12);

/* Output ALL_RESULTS TO .CSV */
data all_t_test_results;
    retain month;
    set all_t_test_results;
run;

proc export data=all_t_test_results
    outfile="&root./psych_dvs_by_smoking_status/psych_ivs_by_smoking_status_tst.csv"
    dbms=csv
    replace;
run;