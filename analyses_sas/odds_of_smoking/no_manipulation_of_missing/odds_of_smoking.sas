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

* Import 9 month survey data file;
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

                
    /* This code chunk was modified to use a temporary data frame */
    /* &where represent fu_smoke5 = "Yes" */
    proc logistic data=&data.;
        %if %length(&where.) > 0 %then %do;
            where &where.;
        %end;

        class arm (ref="Control") / param=ref;
        model &outcome.(event="Yes") = arm;
    run;

%mend logistic_unadjusted;


/* --------------------------- BEGIN WORD OUTPUT --------------------------- */
ods word file = "&root./odds_of_smoking/odds_of_smoking_unadjusted.docx";

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Primary Aims %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* ------------------------------- 6-months -------------------------------- */

/* 30-day at 6 months    --------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 6 months";
%logistic_unadjusted(data = hsq_6mo, outcome = fu_smoke1);
title;

/* 7-day at 6 months    ---------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 6 months";
%logistic_unadjusted(
    data= hsq_6mo, 
    outcome= fu_smoke5, 
    where = fu_smoke1 = "Yes");
title;


/* ------------------------------- 12-months ------------------------------- */

/* 30-day at 12 months    -------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 12 months";
%logistic_unadjusted(data = hsq_12mo, outcome = fu_smoke1);
title;

/* 7-day at 12 months    --------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 12 months";
%logistic_unadjusted(data= hsq_12mo, outcome= fu_smoke5, where= fu_smoke1 = "Yes");
title;

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%% Secondary Aims %%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* ------------------------------- 3-months -------------------------------- */

/* 30-day at 6 months    --------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 3 months";
%logistic_unadjusted(data = hsq_3mo, outcome = fu_smoke1);
title;

/* 7-day at 6 months    ---------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 3 months";
%logistic_unadjusted(data= hsq_3mo, outcome= fu_smoke5, where= fu_smoke1 = "Yes");
title;


/* ------------------------------- 9-months -------------------------------- */

/* 30-day at 12 months    -------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 9 months";
%logistic_unadjusted(data = hsq_9mo, outcome = fu_smoke1);
title;

/* 7-day at 12 months    ---------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 9 months";
%logistic_unadjusted(data= hsq_9mo, outcome= fu_smoke5, where= fu_smoke1 = "Yes");
title;


ods word close;
/* ---------------------------- END WORD OUTPUT ---------------------------- */

/* ///////////////////////////////////////////////////////////////////////// */
/* //////////////////////////// Adjusted Models //////////////////////////// */
/* ///////////////////////////////////////////////////////////////////////// */

/* Select the columns of interest */
proc sql;
    create table covariates as
    select 
        hsqid,
        age_jy, age3_jy, Age2_0326, Age4_0326, age_gp0426, age50, Age_Tobacco4_jy, Age_Started_Tobacco_jy,
        gender2_0326, gender3_jy, gender2_jy,
        race_jy, race_eth_jy, race3_0326, race_eth4_0326, race7_jy, race4_jy, race_eth4_jy, hisp3_jy,
        insurance4_0326, insurance6_jy
    from jun.bldat_0726;
quit;


/* Proc freq for the covariates to determine which ones have 99: Unknown and
    set the reference categories */
proc freq data=covariates;
    tables Age3_jy gender3_jy race4_jy HISP3_jy insurance6_jy;
run;


/* Macro function to perform adjusted logistic regression ------------------ */
%macro logistic_adjusted(data=, outcome=, where=);

    proc sql;
        create table temp as
        select *
        from &data. as a
        left join covariates as b
            on a.hsqid = b.hsqid;
    quit;

    /* Drop the unknowns 99:s */
    proc sql;
        create table temp as
        select *
        from temp
        where gender3_jy ne "99: Unknown"
        and race4_jy ne "99: Unknown"
        and HISP3_jy ne "99: Unknown"
        and insurance6_jy ne "99: Unknown";
    quit;


    proc logistic data=temp;
        %if %length(&where.) > 0 %then %do;
            where &where.;
        %end;

        class 
            arm (ref="Control") 
            Age3_jy (ref="2: 18-49") 
            gender3_jy (ref="1: Male")
            race4_jy (ref="1: White/Caucasian")
            HISP3_jy (ref ="1: Hispanic")
            insurance6_jy (ref= "1: Commercial/Privat") / param=ref;
        model &outcome.(event="Yes") = arm Age3_jy gender3_jy race4_jy HISP3_jy insurance6_jy; 
    run;

%mend logistic_adjusted;

/* --------------------------- BEGIN WORD OUTPUT --------------------------- */
ods word file = "&root./odds_of_smoking/odds_of_smoking_adjusted.docx";

/* ------------------------------- 6-months -------------------------------- */

/* 30-day at 6 months    --------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 6 months";
%logistic_adjusted(data = hsq_6mo, outcome = fu_smoke1);
title;

/* 7-day at 6 months    ---------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 6 months";
%logistic_adjusted(
    data= hsq_6mo, 
    outcome= fu_smoke5, 
    where = fu_smoke1 = "Yes");
title;

/* ------------------------------- 12-months ------------------------------- */

/* 30-day at 12 months    -------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 12 months";
%logistic_adjusted(data = hsq_12mo, outcome = fu_smoke1);
title;

/* 7-day at 12 months    --------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 12 months";
%logistic_adjusted(data= hsq_12mo, outcome= fu_smoke5, where= fu_smoke1 = "Yes");
title;

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%% Secondary Aims %%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* ------------------------------- 3-months -------------------------------- */

/* 30-day at 6 months    --------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 3 months";
%logistic_adjusted(data = hsq_3mo, outcome = fu_smoke1);
title;

/* 7-day at 6 months    ---------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 3 months";
%logistic_adjusted(data= hsq_3mo, outcome= fu_smoke5, where= fu_smoke1 = "Yes");
title;


/* ------------------------------- 9-months -------------------------------- */

/* 30-day at 12 months    -------------------------------------------------- */
/* fu_smoke1 */
title "30-day Odds of reporting smoking at 9 months";
%logistic_adjusted(data = hsq_9mo, outcome = fu_smoke1);
title;

/* 7-day at 12 months    ---------------------------------------------------- */
/* fu_smoke5, where fu_smoke1 == "Yes" */
title "7-day Odds of reporting smoking at 9 months";
%logistic_adjusted(data= hsq_9mo, outcome= fu_smoke5, where= fu_smoke1 = "Yes");
title;

ods word close;
/* ---------------------------- END WORD OUTPUT ---------------------------- */


/* %%%%%%%%%%%%%%%%%%%%%%%%%%% Exploratory Aim %%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/*  */


/* ------------------------------------------------------------------------- */
proc import datafile = "&root./data/combined_sms_quarterly_status.csv"
    out = combined
    dbms = csv
    replace;
run;


%macro combined_logistic(where=);

    proc sql;
        create table temp1 as
        select *
        from combined
        where event_name = &where.;
        /* where event_name = "03mo"; */
    quit;


    proc sql;
        create table temp2 as
        select hsqid, arm
        from hsq_3mo;
    quit;

    proc sql;
        create table final as
        select a.*,
            b.arm
        from temp1 as a
        left join temp2 as b
            on a.hsqid = b.hsqid;
    quit;

    proc logistic data=final;
        class 
            arm (ref="Control");
        model relapse = arm; 
    run;

%mend combined_logistic;

/* --------------------------- BEGIN WORD OUTPUT --------------------------- */
ods word file = "&root./odds_of_smoking/odds_of_smoking_combined.docx";

/* 3mo */
title "Odds of reporting smoking at 3 months";
%combined_logistic(where= event_name = "03mo");
title;

/* 6mo */
title "Odds of reporting smoking at 6 months";
%combined_logistic(where= event_name = "06mo");
title;

/* 9mo */
title "Odds of reporting smoking at 9 months";
%combined_logistic(where= event_name = "09mo");
title;

/* 12mo */
title "Odds of reporting smoking at 12 months";
%combined_logistic(where= event_name = "12mo");
title;

ods word close;
/* ---------------------------- END WORD OUTPUT ---------------------------- */
