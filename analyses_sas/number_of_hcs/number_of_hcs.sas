/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
HSQ - Number of helping conversation
Tests the overall number of helping conversations 

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


/*  Number of helping conversations ---------------------------------------- */
ods word file = "&root./number_of_hcs/n_hcs.docx";
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
