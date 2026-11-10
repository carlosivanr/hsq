/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine

Step 2 of 3

  - After running create_lookup_tables_for_significant_effects.R (Step 1), this
    program will loop through all combinations of DVs and PVs to fit interaction
    models.
  - Creates two files cat_w_int.csv and num_w_int.csv
  - 2026-11-04, TSRQ amot score was not copied correctly in Jun's data, had
    to update code and re-run analyses. After re-running analyses, there were
    some differences in the output file of num_w-int, because results with 
    tsrq_amot were no longer significant in step 1. So there were less rows.

*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


/* Interaction models for odds of smoking . For those that were significant,
see if there's an interaction with the Treatment effect.*/

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

/* Merge in the correct tsrq_amot_score */
/* proc sql;
create table temp as
    select a.*,
        b.tsrq_amot_score
    from temp as a
    left join tsrq_amot_score as b
    on a.hsqid = b.hsqid;
quit; */



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

%let root = C:/Users/rodrica2/OneDrive - The University of Colorado Denver/Documents/DFM/projects/hsq/analyses_sas;

proc import datafile = "&root./odds_of_smoking/odds_of_smoking_w_int/cat_sig_effects_lookup.csv"
  out = cat_sig_fx
  dbms = csv
  replace;
  guessingrows = max;
run;

data _null_;
    set cat_sig_fx;
    call symputx(cats('pv', _n_), pv);
    call symputx(cats('dv', _n_), dv);
    call symputx('n_vars', _n_);  /* store count for looping */
run;

%put Number of pairs: &n_vars.;
%put PV1 = &pv1., DV1 = &dv1.;

%macro fit_cat_models_w_int;
    %do i = 1 %to &n_vars.;
      
      /* Diagnostic — check what's actually stored */
      %put PV&i. = &&pv&i.;
      %put DV&i. = &&dv&i.;


      proc sql;
        create table temp as
            select a.*,
                   b.&&dv&i.
            from jun.bldat_0726 as a
            left join jun.outcome_analysis100826 as b
            on a.hsqid = b.hsqid;
        quit;
      
      ods output
      ModelANOVA=_pe
      ;
      
      proc logistic data=temp;
          where &&pv&i. ne "99: Unknown";

          class &&pv&i. arm(ref = "Control");

          model &&dv&i.(event='1') = arm &&pv&i. arm * &&pv&i.;
      run;

      /* Add the identifying variables */
      proc sql;
        create table sub_tab_1 as
        select
          "&&pv&i." as PV,
          "&&dv&i." as DV,
          Effect,
          ProbChiSq
        from _PE    
      quit;

      /* Append to a master table  */
      proc append base=cat_w_int data= sub_tab_1 force;
      run;


    %end;
%mend fit_cat_models_w_int;

%fit_cat_models_w_int;

/* ///////////////////////////////////////////////////////////////////////// */
/* ///////////////////// Models with continuous predictors ///////////////// */
proc import datafile = "&root./odds_of_smoking/odds_of_smoking_w_int/num_sig_effects_lookup.csv"
    out = num_sig_fx
    dbms = csv
    replace;
    guessingrows = max;
run;

data _null_;
    set num_sig_fx;
    call symputx(cats('pv', _n_), pv);
    call symputx(cats('dv', _n_), dv);
    call symputx('n_vars', _n_);  /* store count for looping */
run;

/* Troubleshoot the loop */
/* %let i = 1; */

%macro fit_num_models_w_int;
    %do i = 1 %to &n_vars.;
      
      /* Diagnostic — check what's actually stored */
      %put PV&i. = &&pv&i.;
      %put DV&i. = &&dv&i.;

      /* Merge the outcome variable to the baseline characteristics */
      proc sql;
        create table temp as
            select a.*,
                  b.&&dv&i.
            from jun.bldat_0726 as a
            left join jun.outcome_analysis100826 as b
            on a.hsqid = b.hsqid;
      quit;
      
      ods output
      ModelANOVA=_pe
      ;
      
      proc logistic data=temp;
          class arm(ref = "Control");

          model &&dv&i.(event='1') = arm &&pv&i. arm * &&pv&i.;
      run;

      /* Add the identifying variables */
      proc sql;
        create table sub_tab_2 as
        select
          "&&pv&i." as PV,
          "&&dv&i." as DV,
          Effect,
          ProbChiSq
        from _PE    
      quit;

      /* Append to a master table  */
      proc append base=num_w_int data= sub_tab_2 force;
      run;


    %end;
%mend fit_num_models_w_int;

%fit_num_models_w_int;


/* ///////////////////////////////////////////////////////////////////////// */
/* Output to for-loop output to .csv files. These can then be reviewed to
determine which models can be re-fit along with comparisons between groups for
further interpretation. */

proc export data=cat_w_int
    outfile="&root./odds_of_smoking/odds_of_smoking_w_int/cat_w_int.csv"
    dbms=csv
    replace;
run;

proc export data=num_w_int
    outfile="&root./odds_of_smoking/odds_of_smoking_w_int/num_w_int.csv"
    dbms=csv
    replace;
run;