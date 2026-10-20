/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
This SAS program fits a series of models using dependent variables from the 
SMS surveys and the hsq quarterly surveys.

At each quarterly timepoint 3mo, 6mo, 9mo, and 12mo, (16 models total)

1. average cessation fatigue ~ smoking status
2. number of helping conversations ~ smoking status 
3. smoking status ~ average cessation fatigue
4. smoking status ~ number of helping conversatoin

Average cessation fatigue and number of helping conversations are calculated
in intervals, for example, number of helping conversations for the 3month time
point are calculated from the week 1 to week 12 sms messages. The number of 
helping conversations for the 6month time point are calculated as the sum
of values between week 13 and week 24.

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

/* Import  */
proc import datafile = "&root./data/quarterly_hcs_cf.csv"
  out = sms_vars
  dbms = csv
  replace;
run;


/* Macro function to perform t-test as linear model ---------------- */
%macro t_test(data=, outcome=, where=);

    proc sql;
        create table temp as
        select *
        from &data. as a
        left join sms_vars as b
            on a.hsqid = b.hsqid
            and a.event_name = b.event_name;
    quit;

    proc means data=temp;
      class fu_smoke1;
      var &outcome ;
    run;

    data temp;
        set temp;
        if fu_smoke1 = "No" then smoke = 0;
        else if fu_smoke1 = "Yes" then smoke = 1;
        else smoke = .;
    run;

                
    /* This code chunk was modified to use a temporary data frame */
    /* &where represent fu_smoke5 = "Yes" */
    proc reg data=temp;
        %if %length(&where.) > 0 %then %do;
            where &where.;
        %end;

        model &outcome = smoke;
    run;

%mend t_test;

/* --------------------------- BEGIN WORD OUTPUT --------------------------- */
ods word file = "&root./sms_dv_by_quarterly_hsq/sms_dvs_by_smoking_status.docx";

/* ------------------------------- 6-months -------------------------------- */

/* fu_smoke1 */
title "Mean Number of Helping Conversations by Smoking Status at 6mo";
%t_test(data = hsq_6mo, outcome = sum_hc);
title;

title "Average Cessation fatigue by Smoking Status at 6mo";
%t_test(data = hsq_6mo, outcome = avg_cf);
title;



/* ------------------------------- 12-months ------------------------------- */
/* fu_smoke1 */
title "Mean Number of Helping Conversations by Smoking Status at 12mo";
%t_test(data = hsq_12mo, outcome = sum_hc);
title;

title "Average Cessation fatigue by Smoking Status at 6mo";
%t_test(data = hsq_12mo, outcome = avg_cf);
title;


/* ------------------------------- 3-months -------------------------------- */
/* fu_smoke1 */
title "Mean Number of Helping Conversations by Smoking Status at 3mo";
%t_test(data = hsq_3mo, outcome = sum_hc);
title;

title "Average Cessation fatigue by Smoking Status at 3mo";
%t_test(data = hsq_3mo, outcome = avg_cf);
title;


/* ------------------------------- 9-months -------------------------------- */
/* fu_smoke1 */
title "Mean Number of Helping Conversations by Smoking Status at 9mo";
%t_test(data = hsq_9mo, outcome = sum_hc);
title;

title "Average Cessation fatigue by Smoking Status at 9mo";
%t_test(data = hsq_9mo, outcome = avg_cf);
title;


/* ///////////////////////////////////////////////////////////////////////// */
/* Macro function to perform unadjusted logistic regression ---------------- */
%macro logistic_unadjusted(data=, predictor=, where=);

    proc sql;
        create table temp as
        select *
        from &data. as a
        left join sms_vars as b
            on a.hsqid = b.hsqid
            and a.event_name = b.event_name;
    quit;
    /* This code chunk was modified to use a temporary data frame */
    /* &where represent fu_smoke5 = "Yes" */
    proc logistic data=temp;
        %if %length(&where.) > 0 %then %do;
            where &where.;
        %end;

        model fu_smoke1(event="Yes") = &predictor.;
    run;

%mend logistic_unadjusted;


/* ------------------------------- 6-months -------------------------------- */
title "30-day Odds of reporting smoking predicted by sum_hc at 6 months";
%logistic_unadjusted(data = hsq_6mo, predictor = sum_hc);
title;


title "30-day Odds of reporting smoking predicted by avg_cf at 6 months";
%logistic_unadjusted(data = hsq_6mo, predictor = avg_cf);
title;


/* ------------------------------- 12-months ------------------------------- */
title "30-day Odds of reporting smoking predicted by sum_hc at 12 months";
%logistic_unadjusted(data = hsq_12mo, predictor = sum_hc);
title;

title "30-day Odds of reporting smoking predicted by avg_cf at 12 months";
%logistic_unadjusted(data = hsq_12mo, predictor = avg_cf);
title;


/* ------------------------------- 3-months -------------------------------- */
title "30-day Odds of reporting smoking predicted by sum_hc at 3 months";
%logistic_unadjusted(data = hsq_3mo, predictor = sum_hc);
title;

title "30-day Odds of reporting smoking predicted by avg_cf at 3 months";
%logistic_unadjusted(data = hsq_3mo, predictor = avg_cf);
title;


/* ------------------------------- 9-months -------------------------------- */
title "30-day Odds of reporting smoking predicted by sum_hc at 9 months";
%logistic_unadjusted(data = hsq_9mo, predictor = sum_hc);
title;

title "30-day Odds of reporting smoking predicted by avg_cf at 9 months";
%logistic_unadjusted(data = hsq_9mo, predictor = avg_cf);
title;


ods word close;
/* ---------------------------- END WORD OUTPUT ---------------------------- */