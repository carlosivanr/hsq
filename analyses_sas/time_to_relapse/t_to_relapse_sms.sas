/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
HSQ - Time to first relapse analysis


*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/* Map a library to a directory name ----------------------------------------*/
/* Set a variable to a the root data directory */
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;


* Import time to first relapse file;
/* First relapse data come from the sms text message data. There were 12
participants that did not submit any SMS text messages and therefore were not
able to determine if they stayed quit or relapsed*/
proc import datafile = "&root./data/first_relapse.csv"
    out = first_relapse
    dbms = csv
    replace;
run;


* Import covariate data from the baseline quarterly survey;
/* Covariate data such as arm, gender, age, etc. are in the baseline (0month) quarterly survey data */
proc import datafile = "&root./data/hsq_data_0mo.csv"
    out = hsq_data
    dbms = csv
    replace;
run;


/* Sort data sets by hsqid before merging */
proc sort data=first_relapse;
    by hsqid;
run;

proc sort data=hsq_data;
    by hsqid;
run;

* Merge the two data sources;
data first_relapse;
    merge first_relapse hsq_data;
    by hsqid;
run;


/* /////////////////////////// BEGIN WORD OUTPUT /////////////////////////// */
ods word file = "&root./time_to_relapse/t_to_relapse.docx";

/* Time to first relapse /////////////////////////////////////////////////// */
/* Frequencies & proportions ------------------------------------------------*/
title "Event by Arm (event = 1 indicates relapse)";
proc freq data = first_relapse;
    table event * arm;
run;
title;


/* Kaplan-Meier Analysis ----------------------------------------------------*/
proc lifetest data = first_relapse plot=(s) notable;
    time time*event(0);
    strata arm;
    title "Kaplan-Meier Analysis";
run;


/* Unadjusted Model ---------------------------------------------------------*/
proc phreg data = first_relapse;
    class arm;
    model time*event(0) = arm / risklimits=both type3(all);
    title "Cox-PH Model (Unadjusted)";
run;


/* Time to first consecutive relapse ////////////////////////////////////// */
/* Frequencies & proportions ------------------------------------------------*/

title "Consecutive event by Arm (consecutive event = 1 indicates 2 consecutive indicators of relapse)";
proc freq data = first_relapse;
    table consecutive_event * arm;
run;
title;


/* Kaplan-Meier Analysis ----------------------------------------------------*/
proc lifetest data = first_relapse plot=(s) notable;
    time time_to_consecutive_event*consecutive_event(0);
    strata arm;
    title "Kaplan-Meier Analysis";
run;


/* Unadjusted Model ---------------------------------------------------------*/
proc phreg data = first_relapse;
    class arm;
    model time_to_consecutive_event*consecutive_event(0) = arm / risklimits=both type3(all);
    title "Cox-PH Model (Unadjusted)";
run;


ods word close;
/* ///////////////////////////// END WORD OUTPUT //////////////////////////// */


/* Chi-square to compare the proportion of relapse by intervention
title "Chi-square test by arm where relapse = 1";
proc freq data = first_relapse;
    where censor = 1;
    tables arm / chisq;
run; */


/* proc freq data = first_relapse;
    table gender;
run;

proc phreg data=hsq_data.first_relapse;
   class arm gender / param=GLM;
   model time*censor(0) = arm|gender;
   lsmeans arm*gender/diff;
run; */


/*  Number of helping conversations ---------------------------------------- */
ods word file = "&root./time_to_relapse/t_to_relapse.docx";
proc sort data=first_relapse;
    by arm;
run;

proc means data=first_relapse;
    by arm;
    var n_hcs;
run;

/* Poisson model to test if the number of HCs are different between the two
treatment groups. */
proc genmod data=first_relapse;
    class arm(ref='Control');
    model n_hcs = arm / dist=poisson link=log;
run;
ods word close;
