/* --------------------------------------------------------------------------
Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine.
04/20/2026
The following code is based off Jun Ying's SAS macros to analyze the baseline
HSQ variables. 

Modifications were placed in the code to create two definitions of dependence
using the NJH data question "How soon after you wake up do you smoke your first
cigarette?"

The difference between the two definitions rests on whether the respondents in
the 6 to 30 minutes group are placed in the low group or become their own 
group.

---------------------------------------------------------------------------*/

/* Set the file library to the Egnyte folder */
libname mydata "Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Jun\To Carlos 042026";

/* Define dependence ----------------------------------------------------------*/

/* Definition 1: This definition creates a 3-level measure of dependence high, medium, and low */
data mydata.BLdat_0326;
    set mydata.BLdat_0326;
    length dependence1 $20;
    if SI_9 in ("Within five minutes") then dependence1 = "1: High";
    else if SI_9 in ("6 to 30 minutes") then dependence1 = "2: Mid";
    else if SI_9 in ("31 to 60 minutes", "More than 60 minutes") then dependence1 = "3: Low";
    else dependence1 = "99: Unknown";
run;

/* Definition 2: This definition creates a 2-level measure of dependent high and low */
data mydata.BLdat_0326;
    set mydata.BLdat_0326;
    length dependence2 $20;
    if SI_9 in ("Within five minutes") then dependence2 = "1: High";
    else if SI_9 in ("6 to 30 minutes", "31 to 60 minutes", "More than 60 minutes") then dependence2 = "2: Low";
    else dependence2 = "99: Unknown";
run;

/* MACRO VERSION --------------------------------------------------------------- */
%macro analyze_dependence (dependence);
	%macro freqfn_dependence (factor);
	proc freq data=mydata.BLdat_0326;
	where &dependence not in ("99: Unknown");
	tables &factor._0326 * &dependence /chisq;
	ods output crosstabfreqs =freqout;
	ods output chisq =cout;
	run;

	data freqout_&factor;
	set freqout;
	if _TYPE_ = "11";
	format factor $20.;
	factor = "&factor";
	format gp $20.;
	gp = &factor._0326;
	keep factor gp &dependence Frequency RowPercent;
	run;

	data cout_&factor;
	set cout;
	if Statistic = "Chi-Square";
	format factor $20.;
	factor = "&factor";
	keep factor Prob;
	run;
	%mend;
	%freqfn_dependence (gender2);
	%freqfn_dependence (race_eth4);
	%freqfn_dependence (Age4);
	%freqfn_dependence (insurance4);
	%freqfn_dependence (Education3_SI16);
	%freqfn_dependence (Cigarettes_PDay10);
	%freqfn_dependence (NRT2);
	%freqfn_dependence (BUP_Varenicline2);
	%freqfn_dependence (eCigarette2_SI5F);
	%freqfn_dependence (Smoke3_SI6a);
	%freqfn_dependence (menthol2_OI7a1);
	%freqfn_dependence (Wake4_SI9);
	%freqfn_dependence (Age_Tobacco4);
	%freqfn_dependence (se6);
	%freqfn_dependence (se7);
	%freqfn_dependence (se8);
	%freqfn_dependence (se9);
	%freqfn_dependence (se10);
	%freqfn_dependence (se11);
	%freqfn_dependence (se12);
	%freqfn_dependence (cd_attraction);
	%freqfn_dependence (cd_smoker);
	%freqfn_dependence (cd_vulnerable);

	data table_freq;
	set
	freqout_gender2
	freqout_race_eth4
	freqout_Age4
	freqout_insurance4
	freqout_Education3_SI16
	freqout_Cigarettes_PDay10
	freqout_NRT2
	freqout_BUP_Varenicline2
	freqout_eCigarette2_SI5F
	freqout_Smoke3_SI6a
	freqout_menthol2_OI7a1
	freqout_Wake4_SI9
	freqout_Age_Tobacco4
	freqout_se6
	freqout_se7
	freqout_se8
	freqout_se9
	freqout_se10
	freqout_se11
	freqout_se12
	freqout_cd_attraction
	freqout_cd_smoker
	freqout_cd_vulnerable
	;
	run;

	data table_chisq;
	set
	cout_gender2
	cout_race_eth4
	cout_Age4
	cout_insurance4
	cout_Education3_SI16
	cout_Cigarettes_PDay10
	cout_NRT2
	cout_BUP_Varenicline2
	cout_eCigarette2_SI5F
	cout_Smoke3_SI6a
	cout_menthol2_OI7a1
	cout_Wake4_SI9
	cout_Age_Tobacco4
	cout_se6
	cout_se7
	cout_se8
	cout_se9
	cout_se10
	cout_se11
	cout_se12
	cout_cd_attraction
	cout_cd_smoker
	cout_cd_vulnerable
	;
	run;


	%macro mixedfn_dependence (var);
	proc mixed data=mydata.BLdat_0326;
	class &dependence;
	where &dependence not in ("99: Unknown");
	model &var = &dependence/s;
	lsmeans &dependence/diff adjust=tukey;
	ods output lsmeans=lsm;
	ods output diffs=diff;
	ods output tests3=t3;
	run;

	data lsm_&var;
	set lsm;
	format var $20.;
	var = "&var";
	run;

	data diff_&var;
	set diff;
	format var $20.;
	var = "&var";
	run;

	data t3_&var;
	set t3;
	format var $20.;
	var = "&var";
	run;
	%mend;

	%mixedfn_dependence(anx_score);
	%mixedfn_dependence(dep_score);
	%mixedfn_dependence(psy_score);
	%mixedfn_dependence(arme_score);
	%mixedfn_dependence(pcs_score);
	%mixedfn_dependence(tsrq_auto);
	%mixedfn_dependence(tsrq_intr);
	%mixedfn_dependence(tsrq_extr);
	%mixedfn_dependence(tsrq_amot);
	%mixedfn_dependence(se_confidence);
	%mixedfn_dependence(density);
	%mixedfn_dependence(n_components);
	%mixedfn_dependence(btw_centr);
	%mixedfn_dependence(deg_centr);
	%mixedfn_dependence(homophily_gender);
	%mixedfn_dependence(homophily_age_cat);
	%mixedfn_dependence(perc_supp_quit);

	data table_lsm;
	set
	lsm_anx_score
	lsm_dep_score
	lsm_psy_score
	lsm_arme_score
	lsm_pcs_score
	lsm_tsrq_auto
	lsm_tsrq_intr
	lsm_tsrq_extr
	lsm_tsrq_amot
	lsm_se_confidence
	lsm_density
	lsm_n_components
	lsm_btw_centr
	lsm_deg_centr
	lsm_homophily_gender
	lsm_homophily_age_cat
	lsm_perc_supp_quit
	;
	run;

	data table_diff;
	set
	diff_anx_score
	diff_dep_score
	diff_psy_score
	diff_arme_score
	diff_pcs_score
	diff_tsrq_auto
	diff_tsrq_intr
	diff_tsrq_extr
	diff_tsrq_amot
	diff_se_confidence
	diff_density
	diff_n_components
	diff_btw_centr
	diff_deg_centr
	diff_homophily_gender
	diff_homophily_age_cat
	diff_perc_supp_quit
	;
	run;

	data table_t3;
	set
	t3_anx_score
	t3_dep_score
	t3_psy_score
	t3_arme_score
	t3_pcs_score
	t3_tsrq_auto
	t3_tsrq_intr
	t3_tsrq_extr
	t3_tsrq_amot
	t3_se_confidence
	t3_density
	t3_n_components
	t3_btw_centr
	t3_deg_centr
	t3_homophily_gender
	t3_homophily_age_cat
	t3_perc_supp_quit
	;
	run;


	PROC EXPORT DATA=table_lsm
				OUTFILE="Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Jun\To Carlos 042026\Analysis &dependence 042026.xlsx"
				DBMS=XLSX REPLACE;
				SHEET="lsm";
	RUN;

	PROC EXPORT DATA=table_diff
				OUTFILE="Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Jun\To Carlos 042026\Analysis &dependence 042026.xlsx"
				DBMS=XLSX REPLACE;
				SHEET="diff";
	RUN;

	PROC EXPORT DATA=table_t3
				OUTFILE="Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Jun\To Carlos 042026\Analysis &dependence 042026.xlsx"
				DBMS=XLSX REPLACE;
				SHEET="t3";
	RUN;

	PROC EXPORT DATA=table_freq
				OUTFILE="Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Jun\To Carlos 042026\Analysis &dependence 042026.xlsx"
				DBMS=XLSX REPLACE;
				SHEET="freq";
	RUN;

	PROC EXPORT DATA=table_chisq
				OUTFILE="Z:\Shared\DFM\HSQ_Shared\Analysis\Stat Working\Jun\To Carlos 042026\Analysis &dependence 042026.xlsx"
				DBMS=XLSX REPLACE;
				SHEET="chisq";
	RUN;
%mend;


%analyze_dependence(dependence1);
%analyze_dependence(dependence2);