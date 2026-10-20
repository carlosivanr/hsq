/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

Aim: Are participants who receive the HSQ intervention more likely to be
abstinent from smoking combustible tobacco (7-day and 30-day abstinence) than
participants who do not receive the HSQ intervention at 6 months and 12 months?
(point assessment of self-reported smoking status using self-reported data
collected at 6 and 12 months post-randomization)

Aim: Are participants who receive the HSQ intervention more likely to be
abstinent from smoking combustible tobacco (7-day and 30-day abstinence) than
participants who do not receive the HSQ intervention at 3 months and 9 months?
(point assessment of self-reported smoking status using self-reported data
collected at 3 and 9 months post-randomization)

Dependent variables: 
1. fu_smoke1 30-day - Did the participant smoke cigarettes in the past 30 days?

2. fu_smoke5 7-day - Did the participant smoke cigarettes the past 7 days,
   branches only to those that responded "Yes" to fu_smoke1

Predictor variables:
1. arm - Intervention or Control

*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* Map a library to a directory name ----------------------------------------*/
/* Set a variable to a the root data directory */
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;


/* Import data ------------------------------------------------------------- */
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

* Import 12 month survey data file;
proc import datafile = "&root./data/hsq_data_9mo.csv"
    out = hsq_9mo
    dbms = csv
    replace;
run;

/* Import covariates from Jun's dataset */
/* libname jun "&root./Jun/To Carlos 042026";  */ /* path to an older version on 4TB external HD */
libname jun "Z:/Shared/DFM/HSQ_Shared/Analysis/Stat Working/Datasets";


/* ///////////////////////////////////////////////////////////////////////// */
/* /////////////////////////// Unadjusted Models /////////////////////////// */
/* ///////////////////////////////////////////////////////////////////////// */

/* Macro function to perform unadjusted logistic regression ---------------- */
%macro logistic_unadjusted(data=, outcome=, where=);
    /*  @data: one of the separate 0,3,6,9,or 12 month data sets
        @outcome: one of the fu_smoke1 or fu_smoke5 variables
        @where: optional where statement to be used with the fu_smoke5 outcome
            since its a branching question. 
        
        @example: 
        %logistic_unadjusted(
            data= hsq_6mo, 
            outcome= fu_smoke5, 
            where= fu_smoke1 = "Yes")                                          */

    proc freq data=&data.;
        %if %length(&where.) > 0 %then %do;
            where &where.;
        %end;

        tables &outcome.*arm / norow nocol;
    run;

    /* Set missing to smoking status as Yes/No */
    /* This code chunk was placed here to be able to modify how a missing value
    is handled. It can be either left as missing, set to No, or set to Yes.*/
    data temp;
        set &data;
        if missing(&outcome) then
        &outcome = "No";
    run;
                
    /* This code chunk was modified to use a temporary data frame */
    /* &where represent fu_smoke5 = "Yes" */
    /* proc logistic data=&data.; */
    proc logistic data=temp;
        %if %length(&where.) > 0 %then %do;
            where &where.;
        %end;

        class arm (ref="Control") / param=ref;
        model &outcome.(event="Yes") = arm;
    run;

%mend logistic_unadjusted;


/* --------------------------- BEGIN WORD OUTPUT --------------------------- */
ods word file = "&root./odds_of_smoking/missing_set_to_no/odds_of_smoking_unadjusted.docx";

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Primary Aims %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* ------------------------------- 6-months -------------------------------- */

/* 30-day at 6 months    --------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 6 months";
%logistic_unadjusted(data = hsq_6mo, outcome = fu_smoke1);
title;



/* ------------------------------- 12-months ------------------------------- */

/* 30-day at 12 months    -------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 12 months";
%logistic_unadjusted(data = hsq_12mo, outcome = fu_smoke1);
title;


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%% Secondary Aims %%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* ------------------------------- 3-months -------------------------------- */

/* 30-day at 6 months    --------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 3 months";
%logistic_unadjusted(data = hsq_3mo, outcome = fu_smoke1);
title;



/* ------------------------------- 9-months -------------------------------- */

/* 30-day at 12 months    -------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 9 months";
%logistic_unadjusted(data = hsq_9mo, outcome = fu_smoke1);
title;



ods word close;
/* ---------------------------- END WORD OUTPUT ---------------------------- */
