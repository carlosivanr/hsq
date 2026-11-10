/* ///////////////////////////////////////////////////////////////////////// */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

Step 3 of 3

The purpose of this script is to fit the models where a significant interaction
was observed.

In a previous step, bivariate logistic regression models were fitted between a
set of categorical and numerical predictors and 4 dependent variables that
indicate whether or not a participant reported smoking by a certain time point.
All predictors were captured from the baseline time point. For models where the
predictor was observed to be statistically significant, a subsequent model that
included an interaction term was fitted. 

These models were fitted using a high-through-put style analysis macro specified
in odds_of_smoking_w_int.sas.

The results of those analyses are saved in ./analyses_sas/odds_of_smoking \
/odds_of_smoking_by/odds_of_smoking_w_int/sas_output_tables.

The compiled results are in ./analyses_sas/odds_of_smoking/ \
odds_of_smoking_w_int/organized_results/results_2026-10-28.xlsx.

A total of 5 models were fitted where a significant interaction was observed. 
The predictors included two categorical and three numerical predictors.

The following scripts fits the models for these predictors and the 
corresponding dependent variables to generate a more complete set of results
using oddsratio statements. All predictors are from the baseline timepoint and 
all include an interaction term between the IV and the HSQ intervention 
variable.

1. smoke1_SMS_by_3mo_jy ~ cd_vulnerable
2. smoke1_SMS_by_9mo_jy ~ cd_vulnerable

Additional models were fitted per JY's request 10/29/2026. Bivariate
associations were not significant for these two models. As a result, the
interaction models were not specified. However, considering that 3mo and 6mo
demonstrated significant interaction terms, these were of further interest.
a. smoke1_SMS_by_6mo_jy ~ cd_vulnerable
b. smoke1_SMS_by_12mo_jy ~ cd_vulnerable

3. smoke1_SMS_by_6mo_jy ~ tsrq_amot !!! This was a fluke due to the wrong variable
4. smoke1_SMS_by_12mo_jy ~ rate_complete
5. smoke1_SMS_by_12mo_jy ~ rate_complete_SMS

The 5 initial models were based on filtering where the predictor was significant
in a model fitting the out come and predictor only.

The remaining models were all discovered from fitting all predictors with interactions
6. smoke1_SMS_by_3mo_jy ~ perc_supp_quit
7. smoke1_SMS_by_3mo_jy ~ rate_complete_MSurve

8. smoke1_SMS_by_6mo_jy ~ perc_supp_quit
9. smoke1_SMS_by_6mo_jy ~ rate_complete

10. smoke1_SMS_by_6mo_jy ~ rate_complete_SMS
11. smoke1_SMS_by_9mo_jy ~ rate_complete

12. smoke1_SMS_by_9mo_jy ~ rate_complete_SMS
13. smoke1_SMS_by_9mo_jy ~ rate_complete_MSurve


*/

/* ///////////////////////////////////////////////////////////////////////// */


/* ///////////////////////////////////////////////////////////////////////// */
libname jun "Z:/Shared/DFM/HSQ_Shared/Analysis/Stat Working/Datasets";
%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;


/* Merge the outcome variable to the baseline characteristics */
proc sql;
  create table temp as
      select a.*,
             b.smoke1_SMS_by_3mo_jy,
             b.smoke1_SMS_by_6mo_jy,
             b.smoke1_SMS_by_9mo_jy,
             b.smoke1_SMS_by_12mo_jy
      from jun.bldat_0726 as a
      left join jun.outcome_analysis100826 as b
      on a.hsqid = b.hsqid;
quit;


proc import datafile="&root./odds_of_smoking/odds_of_smoking_by/tsrq_amot_score_CR.csv"
    out = tsrq_amot_score
    dbms=csv
    replace;
run;


proc sql;
  create table temp as
      select a.*,
             b.tsrq_amot_score
      from temp as a
      left join tsrq_amot_score as b
      on a.hsqid = b.hsqid;
quit;



/* smoke1 SMS by 3mo ~ cd_vulnerable_jy  */
proc logistic data=temp;
  where cd_vulnerable_jy ne "99: Unknown";

  class cd_vulnerable_jy(ref = "2: No") arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_3mo_jy(event='1') = arm cd_vulnerable_jy arm * cd_vulnerable_jy;

  oddsratio cd_vulnerable_jy / at(arm = ALL);
run;

proc logistic data=temp;
  where cd_vulnerable_jy ne "99: Unknown";

  class cd_vulnerable_jy(ref = "2: No") arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_6mo_jy(event='1') = arm cd_vulnerable_jy arm * cd_vulnerable_jy;

  oddsratio cd_vulnerable_jy / at(arm = ALL);
run;


/* smoke1 SMS by 9mo ~ cd_vulnerable */
proc logistic data=temp;
  where cd_vulnerable_jy ne "99: Unknown";

  class cd_vulnerable_jy(ref = "2: No") arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_9mo_jy(event='1') = arm cd_vulnerable_jy arm * cd_vulnerable_jy;

  oddsratio cd_vulnerable_jy / at(arm = ALL);

run;

proc logistic data=temp;
  where cd_vulnerable_jy ne "99: Unknown";

  class cd_vulnerable_jy(ref = "2: No") arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_12mo_jy(event='1') = arm cd_vulnerable_jy arm * cd_vulnerable_jy;

  oddsratio cd_vulnerable_jy / at(arm = ALL);

run;



/* ///////////////////////////////////////////////////////////////////////// */

/* tsrq_amot		tsrq_amot*arm	0.0383 */

proc logistic data=temp;
  class arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_6mo_jy(event='1') = arm tsrq_amot arm * tsrq_amot;

  oddsratio tsrq_amot / at(arm = ALL);

run;


proc logistic data=temp;
  class arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_12mo_jy(event='1') = arm rate_complete arm * rate_complete;

  oddsratio rate_complete / at(arm = ALL);

run;


proc logistic data=temp;
  class arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_12mo_jy(event='1') = arm rate_complete_SMS arm * rate_complete_SMS;

  oddsratio rate_complete_SMS / at(arm = ALL);
run;


/* Check tsrq_amot at baseline */
/* proc sql;
  create table tsrq_amot as
  select a.hsqid, a.tsrq_amot
  from temp as a;
quit; */

/* proc export data=tsrq_amot
    outfile="&root./odds_of_smoking/odds_of_smoking_by/tsrq_amot_.csv"
    dbms=csv
    replace;
run; */


/* ///////////////////////////////////////////////////////////////////////// */
proc logistic data=temp;
  class arm(ref = "Control") / param=ref;

  model smoke1_SMS_by_6mo_jy(event='1') = arm tsrq_amot_score arm * tsrq_amot_score;

  oddsratio tsrq_amot_score / at(arm = ALL);

run;

/* //////////////////////////////////////////////////////////////////////// */
	/* 
  smoke1_SMS_by_3mo_jy ~ perc_supp_quit
	smoke1_SMS_by_3mo_jy ~ rate_complete_MSurve

	smoke1_SMS_by_6mo_jy ~ perc_supp_quit
	smoke1_SMS_by_6mo_jy ~ rate_complete
	smoke1_SMS_by_6mo_jy ~ rate_complete_SMS
	
  smoke1_SMS_by_9mo_jy ~ rate_complete
	smoke1_SMS_by_9mo_jy ~ rate_complete_SMS
	smoke1_SMS_by_9mo_jy ~ rate_complete_MSurve */


/* 3 month */
proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_3mo_jy(event='1') = arm perc_supp_quit arm * perc_supp_quit;
  oddsratio perc_supp_quit / at(arm = ALL);
run;

proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_3mo_jy(event='1') = arm rate_complete_MSurvey arm * rate_complete_MSurvey;
  oddsratio rate_complete_MSurvey / at(arm = ALL);
run;

/* 6 month */
proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_6mo_jy(event='1') = arm perc_supp_quit arm * perc_supp_quit;
  oddsratio perc_supp_quit / at(arm = ALL);
run;

proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_6mo_jy(event='1') = arm rate_complete arm * rate_complete;
  oddsratio rate_complete / at(arm = ALL);
run;

proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_6mo_jy(event='1') = arm rate_complete_SMS arm * rate_complete_SMS;
  oddsratio rate_complete_SMS / at(arm = ALL);
run;

/* 9 month */
proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_9mo_jy(event='1') = arm rate_complete arm * rate_complete;
  oddsratio rate_complete / at(arm = ALL);
run;

proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_9mo_jy(event='1') = arm rate_complete_SMS arm * rate_complete_SMS;
  oddsratio rate_complete_SMS / at(arm = ALL);
run;

proc logistic data=temp;
  class arm(ref = "Control") / param=ref;
  model smoke1_SMS_by_9mo_jy(event='1') = arm rate_complete_MSurvey arm * rate_complete_MSurvey;
  oddsratio rate_complete_MSurvey / at(arm = ALL);
run;