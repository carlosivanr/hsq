libname mydata 'C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\From Egnyte\Stat Working\Jun Work\SAS\Data';
libname respdata 'C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\Jun\Response Rate\Data';

%macro freqfn_protocol_type (factor);
proc freq data=mydata.BLdat_0326;
where &factor._0326 not in ("99: Unknown");
tables &factor._0326 * ProtocolType2_0326 /chisq;
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
keep factor gp ProtocolType2_0326 Frequency RowPercent;
run;

data cout_&factor;
set cout;
if Statistic = "Chi-Square";
format factor $20.;
factor = "&factor";
keep factor Prob;
run;
%mend;
%freqfn_protocol_type (gender2);
%freqfn_protocol_type (race_eth4);
%freqfn_protocol_type (Age4);
%freqfn_protocol_type (insurance4);
%freqfn_protocol_type (Education3_SI16);
%freqfn_protocol_type (Cigarettes_PDay10);
%freqfn_protocol_type (NRT2);
%freqfn_protocol_type (BUP_Varenicline2);
%freqfn_protocol_type (eCigarette2_SI5F);
%freqfn_protocol_type (Smoke3_SI6a);
%freqfn_protocol_type (menthol2_OI7a1);
%freqfn_protocol_type (Wake4_SI9);
%freqfn_protocol_type (Age_Tobacco4);
%freqfn_protocol_type (se6);
%freqfn_protocol_type (se7);
%freqfn_protocol_type (se8);
%freqfn_protocol_type (se9);
%freqfn_protocol_type (se10);
%freqfn_protocol_type (se11);
%freqfn_protocol_type (se12);
%freqfn_protocol_type (cd_attraction);
%freqfn_protocol_type (cd_smoker);
%freqfn_protocol_type (cd_vulnerable);

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


%macro mixedfn_protocol_type (var);
proc mixed data=mydata.BLdat_0326;
class ProtocolType2_0326;
model &var = ProtocolType2_0326/s;
lsmeans ProtocolType2_0326/diff adjust=turkey;
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

%mixedfn_protocol_type(anx_score);
%mixedfn_protocol_type(dep_score);
%mixedfn_protocol_type(psy_score);
%mixedfn_protocol_type(arme_score);
%mixedfn_protocol_type(pcs_score);
%mixedfn_protocol_type(tsrq_auto);
%mixedfn_protocol_type(tsrq_intr);
%mixedfn_protocol_type(tsrq_extr);
%mixedfn_protocol_type(tsrq_amot);
%mixedfn_protocol_type(se_confidence);
%mixedfn_protocol_type(density);
%mixedfn_protocol_type(n_components);
%mixedfn_protocol_type(btw_centr);
%mixedfn_protocol_type(deg_centr);
%mixedfn_protocol_type(homophily_gender);
%mixedfn_protocol_type(homophily_age_cat);
%mixedfn_protocol_type(perc_supp_quit);

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
			OUTFILE="C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\From Egnyte\Stat Working\Jun Work\SAS\Out\Analysis Protocol Type 040126.xlsx"
			DBMS=XLSX REPLACE;
			SHEET="lsm";
RUN;

PROC EXPORT DATA=table_diff
			OUTFILE="C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\From Egnyte\Stat Working\Jun Work\SAS\Out\Analysis Protocol Type 040126.xlsx"
			DBMS=XLSX REPLACE;
			SHEET="diff";
RUN;

PROC EXPORT DATA=table_t3
			OUTFILE="C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\From Egnyte\Stat Working\Jun Work\SAS\Out\Analysis Protocol Type 040126.xlsx"
			DBMS=XLSX REPLACE;
			SHEET="t3";
RUN;

PROC EXPORT DATA=table_freq
			OUTFILE="C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\From Egnyte\Stat Working\Jun Work\SAS\Out\Analysis Protocol Type 040126.xlsx"
			DBMS=XLSX REPLACE;
			SHEET="freq";
RUN;

PROC EXPORT DATA=table_chisq
			OUTFILE="C:\Users\yingj\OneDrive - The University of Colorado Denver\Jun\CUANSCHUTZ\DFM\Research\Myra Muramoto\HSQ\From Egnyte\Stat Working\Jun Work\SAS\Out\Analysis Protocol Type 040126.xlsx"
			DBMS=XLSX REPLACE;
			SHEET="chisq";
RUN;

