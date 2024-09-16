# REDCap Processing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_labels_factors <- function(data){
  library(Hmisc)
  ## Setting Labels ----
  label(data$record_id)="Record ID"
  label(data$redcap_event_name)="Event Name"
  #label(data$redcap_survey_identifier)="Survey Identifier"
  #label(data$pathweigh_practice_member_survey_timestamp)="Survey Timestamp"
  #label(data$consent)="This study is designed to learn more about weight management practices for overweight and obese patients who qualify in primary care. You are being asked to be in this research study because you are a staff member or provider in a UCHealth system primary care practice that is participating in this study. If you choose to join the study, you will complete this online survey. Clicking on the I agree button at the bottom of this page will take you to a survey that will take 10-15 minutes to complete. You will receive a $10 gift card for completing the survey. There are no costs to you to participate in this study. Possible discomforts or risks include a minimal risk of loss of confidentiality. There may be risks the researchers have not thought of at this time. Every effort will be made to protect your privacy and confidentiality by using unique identifying codes for each completed survey. These codes will be accessible and used only by the research team. Practice survey data will be completely confidential. To mitigate potential risks, we will only present results in aggregate at the practice-level so that it is not possible to identify you or any individuals. All data will be stored on a secure research server maintained by the University of Colorado Anschutz. This research is being paid for by a grant from the National Institute of Diabetes and Digestive and Kidney Diseases (NIDDK) of the National Institutes of Health. Your participation in this research is voluntary. You do not have to be in this study if you do not want to be. You may choose not to answer certain questions if you are uncomfortable. You may choose to withdraw at any time without penalty. Declining participation will not penalize you in any way from your employment. Your participation or non-participation in this study will not be shared with your employer. If you have questions, you can call the study manager, Johnny Williams, at (303) 724-9668. You can call and ask questions at any time. You may have questions about your rights as someone in this study. If you have questions, you can call the COMIRB (the responsible Institutional Review Board). Their number is (303) 724-1055. By completing the practice survey, you are agreeing to participate in this research study. "
  label(data$decline)="Please confirm you are declining to participate."
  label(data$pm_role)="Please select the appropriate option to describe your role in the practice."
  label(data$pm_oth_text)="Please describe."
  label(data$pm_gender)="What is your gender?"
  label(data$pm_age)="What is your age range?"
  label(data$pm_yrs_exper)="How many years have you been working in health care?"
  label(data$pm_ptft)="What is your percent full-time equivalent (FTE) in the clinic?"
  label(data$pm_strggle)="Have you ever personally wanted to and/or tried to lose weight? "
  label(data$wls_satis_prac)="How satisfied are you with this practices ability to help patients manage their weight/address weight loss and maintenance? "
  label(data$wls_appropriate)="To what extent do you agree with this statement: Weight loss for patients with overweight or obesity is something that a primary care practice should provide to their patients as part of comprehensive primary care. "
  label(data$wls_concern___1)="I/our providers dont have time in their schedule)"
  label(data$wls_concern___2)="We wont get paid / will lose money)"
  label(data$wls_concern___3)="I/our providers dont know how)"
  label(data$wls_concern___4)="I/our providers dont feel like patients will be able to successfully change and lose weight)"
  label(data$wls_concern___5)="I/our providers are not interested in providing weight management)"
  label(data$wls_concern___6)="We dont have weight management workflows set up in our practice)"
  label(data$wls_concern___7)="Our patients will not want their providers to bring up weight loss with them)"
  label(data$wls_concern___8)="We dont have time to set up a new program)"
  label(data$wls_concern___99)="Other (please describe))"
  label(data$wls_concern___10)="None of the above are concerns)"
  label(data$wls_oth_con_text)="Please describe."
  label(data$wls_help_text)="What do you think, if in place, would help weight management go better? "
  label(data$provide_wls)="I am expected to provide (or assist in providing if not a clinical role) weight loss assistance to a certain number of patients."
  label(data$help_wls)="I am expected to help my practice with providing weight loss assistance to patients."
  label(data$support_to_id)="I get the support I need to identify patients (or assist in identifying patients) who might need weight loss assistance."
  label(data$support_to_asst)="I get the support I need to do my part with assisting patients with weight loss."
  label(data$recognition)="I receive recognition when I provide (or help provide) weight loss assistance to patients."
  label(data$appreciation)="I receive appreciation when I provide (or help provide) weight loss assistance to patients."
  label(data$confidence)="Overall, how confident are you with your ability to provide high-quality weight management assistance to your patients? "
  label(data$effective)="Overall, how effective do you believe you are with helping your overweight and obese patients to lose weight and keep it off? "
  label(data$certification)="Do you have obesity treatment certification?"
  label(data$cert_type_text)="What type of certification?"
  label(data$cme)="Have you gotten additional CME on how to provide obesity treatment or weight loss?"
  label(data$any_assistance)="Do you provide some assistance to patients related to weight loss or maintenance?"
  label(data$ext_wtloss)="Identify patients for whom weight loss might be needed"
  label(data$ext_id)="Educate patients about the effects of excess weight on health"
  label(data$ext_edu)="Offer to work with them on weight loss"
  label(data$ext_offer)="Provide ongoing visits (at least monthly) to assist pts. with weight loss"
  label(data$ext_ongoing)="Track patients weight and/or weight loss"
  label(data$ext_track)="Show a patient his/her weight loss improvement over time"
  label(data$ext_show)="Set and track patient-specific weight loss goals"
  label(data$ext_emr)="Use EMR templates for weight loss goal setting and tracking"
  label(data$ext_motiv)="Discuss motivational/change strategies for weight loss with patients"
  label(data$ext_challenges)="Track patients challenges/barriers"
  label(data$ext_therapy)="Refer patients to counseling/therapy for their weight"
  label(data$ext_nutrtion)="Refer patients to nutrition counseling"
  label(data$ext_bariatric)="Refer overweight/obese patients to bariatric surgery"
  label(data$ext_programs)="Refer patients to weight loss programs"
  label(data$ext_meds)="Provide medications for weight loss"
  label(data$ext_med_impl)="Consider weight implications of medication prescribed for other reasons"
  label(data$ext_meals)="Provide supplements or meal replacements for weight loss"
  label(data$ext_clin_mgmt)="Provide other clinical management for weight loss"
  label(data$ext_code_bill)="Complete coding and/or billing to get paid for weight-prioritized visits"
  label(data$ext_other)="Other"
  label(data$ext_oth_text)="Please describe other weight loss services, if applicable. "
  label(data$level_of_assistance)="Which of the following best describes the weight management assistance you usually provide to patients?"
  label(data$how_help_text)="When you have a patient who asks you for help with weight loss, what do you do now to help them?"
  label(data$learn_more___1)="How to prescribe/appropriate medications for weight management"
  label(data$learn_more___2)="Which medications cause weight gain and how to find ones that dont"
  label(data$learn_more___3)="How to bring up the subject of weight without upsetting the patient"
  label(data$learn_more___4)="How to organize workflows to accommodate weight management"
  label(data$learn_more___5)="Options for how to organize weight management in a busy practice"
  label(data$learn_more___6)="Recommendations around specific diet and eating plans, as well as which ones work and do not work"
  label(data$learn_more___7)="Helping patients who are struggling with weight management"
  label(data$learn_more___8)="Resources to refer patients to for weight loss"
  label(data$learn_more___9)="Apps, tools, and other materials to use with patients for weight loss"
  label(data$learn_more___10)="How to bill for/get paid for weight management"
  label(data$learn_more___11)="How to organize and track weight management with patients over time in the EMR"
  label(data$learn_more___12)="None of the above"
  label(data$learn_more___99)="Other"
  label(data$learn_oth_text)="Please describe."
  label(data$learn_ways___1)="One-two hour e-learning module on providing weight management to patients where I get CME credit"
  label(data$learn_ways___2)="Having someone come to our practice and provide information on how to do weight management"
  label(data$learn_ways___3)="Being able to call another provider experienced in weight management to ask questions"
  label(data$learn_ways___4)="Have resources in a program like UpToDate (or other)"
  label(data$learn_ways___5)="None of the above"
  label(data$learn_ways___99)="Other"
  label(data$ways_learn_text)="Please describe."
  label(data$learn_commt)="If you have any other comments about weight loss for patients in the practice, please describe."
  label(data$pc_discussion)="After making a change, we discuss what worked and what didnt."
  label(data$pc_opinion)="My opinion is valued by others in this practice."
  label(data$pc_jobs_fit)="People in this practice understand how their jobs fit into the rest of this practice."
  label(data$pc_chaos)="This practice is almost always in chaos."
  label(data$pc_rely)="I can rely on the other people in this practice to do their jobs well."
  label(data$pc_effort)="This practice puts a great deal of effort into improving the quality of care."
  label(data$pc_input)="This practice encourages everybodys input for making changes."
  label(data$pc_improve)="We regularly take time to consider ways to improve how we do things."
  label(data$pc_time)="The practice leadership makes sure that we have the time and space necessary to discuss changes to improve care."
  label(data$pc_organized)="This practice is very disorganized."
  label(data$pc_tension)="When there is conflict or tension in this practice, those involved are encouraged to talk about it."
  label(data$pc_thoughtful)="People in this practice are thoughtful about how they do their jobs."
  label(data$pc_data)="This practice uses data and information to improve the work of the practice."
  label(data$pc_share)="Our practice encourages people to share their ideas about how to improve things."
  label(data$pc_affects)="People in this practice pay attention to how their actions affect others in the practice."
  label(data$pc_available)="The leadership in this practice is available to discuss work related problems."
  label(data$pc_efforts)="When we experience a problem in the practice we make a serious effort to figure out whats really going on."
  label(data$pc_stable)="Our practice has recently been very stable."
  label(data$pc_change)="Things have been changing so fast in our practice that it is hard to keep up with what is going on."
  label(data$pc_leadership)="The leadership of this practice is good at helping us to make sense of problems or difficult situations."
  label(data$pc_enjoy)="Most of the people who work in our practice seem to enjoy their work."
  label(data$pc_environment)="The practice leadership promotes an environment that is an enjoyable place to work."
  label(data$changesinweight_q1___1)="Served as a practice champion"
  label(data$changesinweight_q1___2)="Participated in a learning community via zoom"
  label(data$changesinweight_q1___3)="Utilized the PATHWEIGH (labeled as ambulatory weight management) tools in EPIC (patient questions, smart set, e-consults, UpToDate instructions)"
  label(data$changesinweight_q1___4)="Scheduled, participated in helping with, or had a patient visit utilizing the weight prioritized visit type)"
  label(data$changesinweight_q1___5)="None of these"
  label(data$changesinweight_q1___6)="Dont know/not sure"
  label(data$changesinweight_q1___7)="Other"
  label(data$explainchange_q1)="Please explain"
  label(data$changesinweight_q2)="Did you take part in any training for weight management over the past year? "
  label(data$explainchange_q2___1)="If yes, what did you do? (choice=Became board certified in Obesity Medicine)"
  label(data$explainchange_q2___2)="If yes, what did you do? (choice=Completed the CU e-learning online course on obesity management)"
  label(data$explainchange_q2___3)="If yes, what did you do? (choice=Completed other training (includingCME) for weight management, please describe)"
  label(data$explainyes_q2)="Please explain"
  label(data$changesinweight_q3___1)="More attention on weight as a health concern"
  label(data$changesinweight_q3___2)="Better tracking of height and weight/calculating of BMI/having BMI recorded"
  label(data$changesinweight_q3___3)="More visits about weight loss specifically"
  label(data$changesinweight_q3___4)="More discussion about weight loss in other types of visits"
  label(data$changesinweight_q3___5)="Better help for patients relating to weight loss (i.e. quality of weight loss help)"
  label(data$changesinweight_q3___6)="Other, please describe"
  label(data$changesinweight_q3___7)="None of these"
  label(data$explainyes_q3)="Please explain"
  label(data$changesinweight_q4___1)="I/our providers dont have time in their schedule"
  label(data$changesinweight_q4___2)="We wont get paid / will lose money"
  label(data$changesinweight_q4___3)="I/our providers dont know how"
  label(data$changesinweight_q4___4)="I/our providers dont feel like patients will be able to successfully change and lose weight"
  label(data$changesinweight_q4___5)="I/our providers are not interested in providing weight management"
  label(data$changesinweight_q4___6)="We dont have weight management workflows set up in our practice"
  label(data$changesinweight_q4___7)="Our patients will not want their providers to bring up weight loss with them"
  label(data$changesinweight_q4___8)="We dont have time to set up a new program"
  label(data$changesinweight_q4___9)="Other (please describe)"
  label(data$changesinweight_q4___10)="None of the above are concerns"
  label(data$explainyes_q4)="Please explain"
  label(data$changesinweight_q5___1)="COVID-19 pandemic effects in general"
  label(data$changesinweight_q5___2)="COVID-19 pandemic effects on staffing availability"
  label(data$changesinweight_q5___3)="COVID-19 pandemic effects on access for weight visits"
  label(data$changesinweight_q5___4)="COVID-19 pandemic effects on ability to implement anything new"
  label(data$changesinweight_q5___5)="Lack of interest in the topic of weight loss"
  label(data$changesinweight_q5___6)="Lack of staffing overall"
  label(data$changesinweight_q5___7)="Other (please describe)"
  label(data$changesinweight_q5___8)="None of the above"
  label(data$explainyes_q5)="Please explain"
  label(data$changesinweight_q6)="Are you interested in more help with PATHWEIGH/weight management in the next year? "
  label(data$explainyes_q6)="Please explain"
  label(data$changesinweight_q7)="What do you think, if in place, would help providing weight management to go better? "
  label(data$changesinweight_q8)="If you have any other comments about PATHWEIGH or weight loss for patients in the practice overall, please describe."
  label(data$burnout)="Using your own definition of burnout, please indicate which statement best describes your situation working at this practice. "
  label(data$ws_rewarding)="I find my current work personally rewarding."
  label(data$ws_satisfied)="Overall, I am satisfied in my current practice."
  label(data$pathweigh_practice_member_survey_complete)="Complete?"
  label(data$email)="Email address"
  label(data$practice)="Practice"
  label(data$role_orig)="Role"
  label(data$send_followup)="Follow-up survey?"
  label(data$contact_info_complete)="Complete?"
  #Setting Units
  
  
  ## Setting Factors(will create new variable for factors) ----
  data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline_arm_1","followup12m_arm_1"))
  data$consent.factor = factor(data$consent,levels=c("1","2"))
  data$decline.factor = factor(data$decline,levels=c("1","0"))
  data$pm_role.factor = factor(data$pm_role,levels=c("1","2","3","4","5","6","7","8","9","99"))
  data$pm_gender.factor = factor(data$pm_gender,levels=c("1","2","3","4"))
  data$pm_age.factor = factor(data$pm_age,levels=c("1","2","3","4","5"))
  data$pm_ptft.factor = factor(data$pm_ptft,levels=c("1","2","3"))
  data$pm_strggle.factor = factor(data$pm_strggle,levels=c("1","0","2"))
  data$wls_satis_prac.factor = factor(data$wls_satis_prac,levels=c("1","2","3","4","5"))
  data$wls_appropriate.factor = factor(data$wls_appropriate,levels=c("1","2","3","4","5"))
  # data$wls_concern___1.factor = factor(data$wls_concern___1,levels=c("0","1"))
  # data$wls_concern___2.factor = factor(data$wls_concern___2,levels=c("0","1"))
  # data$wls_concern___3.factor = factor(data$wls_concern___3,levels=c("0","1"))
  # data$wls_concern___4.factor = factor(data$wls_concern___4,levels=c("0","1"))
  # data$wls_concern___5.factor = factor(data$wls_concern___5,levels=c("0","1"))
  # data$wls_concern___6.factor = factor(data$wls_concern___6,levels=c("0","1"))
  # data$wls_concern___7.factor = factor(data$wls_concern___7,levels=c("0","1"))
  # data$wls_concern___8.factor = factor(data$wls_concern___8,levels=c("0","1"))
  # data$wls_concern___99.factor = factor(data$wls_concern___99,levels=c("0","1"))
  # data$wls_concern___10.factor = factor(data$wls_concern___10,levels=c("0","1"))
  data$provide_wls.factor = factor(data$provide_wls,levels=c("1","2","3","4","5"))
  data$help_wls.factor = factor(data$help_wls,levels=c("1","2","3","4","5"))
  data$support_to_id.factor = factor(data$support_to_id,levels=c("1","2","3","4","5"))
  data$support_to_asst.factor = factor(data$support_to_asst,levels=c("1","2","3","4","5"))
  data$recognition.factor = factor(data$recognition,levels=c("1","2","3","4","5"))
  data$appreciation.factor = factor(data$appreciation,levels=c("1","2","3","4","5"))
  data$confidence.factor = factor(data$confidence,levels=c("1","2","3","4","5","99"))
  data$effective.factor = factor(data$effective,levels=c("1","2","3","4","5","99"))
  data$certification.factor = factor(data$certification,levels=c("0","1","2","99"))
  data$cme.factor = factor(data$cme,levels=c("1","2","3","4","99"))
  data$any_assistance.factor = factor(data$any_assistance,levels=c("1","0"))
  data$ext_wtloss.factor = factor(data$ext_wtloss,levels=c("1","2","3"))
  data$ext_id.factor = factor(data$ext_id,levels=c("1","2","3"))
  data$ext_edu.factor = factor(data$ext_edu,levels=c("1","2","3"))
  data$ext_offer.factor = factor(data$ext_offer,levels=c("1","2","3"))
  data$ext_ongoing.factor = factor(data$ext_ongoing,levels=c("1","2","3"))
  data$ext_track.factor = factor(data$ext_track,levels=c("1","2","3"))
  data$ext_show.factor = factor(data$ext_show,levels=c("1","2","3"))
  data$ext_emr.factor = factor(data$ext_emr,levels=c("1","2","3"))
  data$ext_motiv.factor = factor(data$ext_motiv,levels=c("1","2","3"))
  data$ext_challenges.factor = factor(data$ext_challenges,levels=c("1","2","3"))
  data$ext_therapy.factor = factor(data$ext_therapy,levels=c("1","2","3"))
  data$ext_nutrtion.factor = factor(data$ext_nutrtion,levels=c("1","2","3"))
  data$ext_bariatric.factor = factor(data$ext_bariatric,levels=c("1","2","3"))
  data$ext_programs.factor = factor(data$ext_programs,levels=c("1","2","3"))
  data$ext_meds.factor = factor(data$ext_meds,levels=c("1","2","3"))
  data$ext_med_impl.factor = factor(data$ext_med_impl,levels=c("1","2","3"))
  data$ext_meals.factor = factor(data$ext_meals,levels=c("1","2","3"))
  data$ext_clin_mgmt.factor = factor(data$ext_clin_mgmt,levels=c("1","2","3"))
  data$ext_code_bill.factor = factor(data$ext_code_bill,levels=c("1","2","3"))
  data$ext_other.factor = factor(data$ext_other,levels=c("1","2","3"))
  data$level_of_assistance.factor = factor(data$level_of_assistance,levels=c("1","2","3","4"))
  # data$learn_more___1.factor = factor(data$learn_more___1,levels=c("0","1"))
  # data$learn_more___2.factor = factor(data$learn_more___2,levels=c("0","1"))
  # data$learn_more___3.factor = factor(data$learn_more___3,levels=c("0","1"))
  # data$learn_more___4.factor = factor(data$learn_more___4,levels=c("0","1"))
  # data$learn_more___5.factor = factor(data$learn_more___5,levels=c("0","1"))
  # data$learn_more___6.factor = factor(data$learn_more___6,levels=c("0","1"))
  # data$learn_more___7.factor = factor(data$learn_more___7,levels=c("0","1"))
  # data$learn_more___8.factor = factor(data$learn_more___8,levels=c("0","1"))
  # data$learn_more___9.factor = factor(data$learn_more___9,levels=c("0","1"))
  # data$learn_more___10.factor = factor(data$learn_more___10,levels=c("0","1"))
  # data$learn_more___11.factor = factor(data$learn_more___11,levels=c("0","1"))
  # data$learn_more___12.factor = factor(data$learn_more___12,levels=c("0","1"))
  # data$learn_more___99.factor = factor(data$learn_more___99,levels=c("0","1"))
  # data$learn_ways___1.factor = factor(data$learn_ways___1,levels=c("0","1"))
  # data$learn_ways___2.factor = factor(data$learn_ways___2,levels=c("0","1"))
  # data$learn_ways___3.factor = factor(data$learn_ways___3,levels=c("0","1"))
  # data$learn_ways___4.factor = factor(data$learn_ways___4,levels=c("0","1"))
  # data$learn_ways___5.factor = factor(data$learn_ways___5,levels=c("0","1"))
  # data$learn_ways___99.factor = factor(data$learn_ways___99,levels=c("0","1"))
  data$pc_discussion.factor = factor(data$pc_discussion,levels=c("1","2","3","4","5"))
  data$pc_opinion.factor = factor(data$pc_opinion,levels=c("1","2","3","4","5"))
  data$pc_jobs_fit.factor = factor(data$pc_jobs_fit,levels=c("1","2","3","4","5"))
  data$pc_chaos.factor = factor(data$pc_chaos,levels=c("1","2","3","4","5"))
  data$pc_rely.factor = factor(data$pc_rely,levels=c("1","2","3","4","5"))
  data$pc_effort.factor = factor(data$pc_effort,levels=c("1","2","3","4","5"))
  data$pc_input.factor = factor(data$pc_input,levels=c("1","2","3","4","5"))
  data$pc_improve.factor = factor(data$pc_improve,levels=c("1","2","3","4","5"))
  data$pc_time.factor = factor(data$pc_time,levels=c("1","2","3","4","5"))
  data$pc_organized.factor = factor(data$pc_organized,levels=c("1","2","3","4","5"))
  data$pc_tension.factor = factor(data$pc_tension,levels=c("1","2","3","4","5"))
  data$pc_thoughtful.factor = factor(data$pc_thoughtful,levels=c("1","2","3","4","5"))
  data$pc_data.factor = factor(data$pc_data,levels=c("1","2","3","4","5"))
  data$pc_share.factor = factor(data$pc_share,levels=c("1","2","3","4","5"))
  data$pc_affects.factor = factor(data$pc_affects,levels=c("1","2","3","4","5"))
  data$pc_available.factor = factor(data$pc_available,levels=c("1","2","3","4","5"))
  data$pc_efforts.factor = factor(data$pc_efforts,levels=c("1","2","3","4","5"))
  data$pc_stable.factor = factor(data$pc_stable,levels=c("1","2","3","4","5"))
  data$pc_change.factor = factor(data$pc_change,levels=c("1","2","3","4","5"))
  data$pc_leadership.factor = factor(data$pc_leadership,levels=c("1","2","3","4","5"))
  data$pc_enjoy.factor = factor(data$pc_enjoy,levels=c("1","2","3","4","5"))
  data$pc_environment.factor = factor(data$pc_environment,levels=c("1","2","3","4","5"))
  data$changesinweight_q1___1.factor = factor(data$changesinweight_q1___1,levels=c("0","1"))
  data$changesinweight_q1___2.factor = factor(data$changesinweight_q1___2,levels=c("0","1"))
  data$changesinweight_q1___3.factor = factor(data$changesinweight_q1___3,levels=c("0","1"))
  data$changesinweight_q1___4.factor = factor(data$changesinweight_q1___4,levels=c("0","1"))
  data$changesinweight_q1___5.factor = factor(data$changesinweight_q1___5,levels=c("0","1"))
  data$changesinweight_q1___6.factor = factor(data$changesinweight_q1___6,levels=c("0","1"))
  data$changesinweight_q1___7.factor = factor(data$changesinweight_q1___7,levels=c("0","1"))
  data$changesinweight_q2.factor = factor(data$changesinweight_q2,levels=c("1","0","2"))
  data$explainchange_q2___1.factor = factor(data$explainchange_q2___1,levels=c("0","1"))
  data$explainchange_q2___2.factor = factor(data$explainchange_q2___2,levels=c("0","1"))
  data$explainchange_q2___3.factor = factor(data$explainchange_q2___3,levels=c("0","1"))
  data$changesinweight_q3___1.factor = factor(data$changesinweight_q3___1,levels=c("0","1"))
  data$changesinweight_q3___2.factor = factor(data$changesinweight_q3___2,levels=c("0","1"))
  data$changesinweight_q3___3.factor = factor(data$changesinweight_q3___3,levels=c("0","1"))
  data$changesinweight_q3___4.factor = factor(data$changesinweight_q3___4,levels=c("0","1"))
  data$changesinweight_q3___5.factor = factor(data$changesinweight_q3___5,levels=c("0","1"))
  data$changesinweight_q3___6.factor = factor(data$changesinweight_q3___6,levels=c("0","1"))
  data$changesinweight_q3___7.factor = factor(data$changesinweight_q3___7,levels=c("0","1"))
  data$changesinweight_q4___1.factor = factor(data$changesinweight_q4___1,levels=c("0","1"))
  data$changesinweight_q4___2.factor = factor(data$changesinweight_q4___2,levels=c("0","1"))
  data$changesinweight_q4___3.factor = factor(data$changesinweight_q4___3,levels=c("0","1"))
  data$changesinweight_q4___4.factor = factor(data$changesinweight_q4___4,levels=c("0","1"))
  data$changesinweight_q4___5.factor = factor(data$changesinweight_q4___5,levels=c("0","1"))
  data$changesinweight_q4___6.factor = factor(data$changesinweight_q4___6,levels=c("0","1"))
  data$changesinweight_q4___7.factor = factor(data$changesinweight_q4___7,levels=c("0","1"))
  data$changesinweight_q4___8.factor = factor(data$changesinweight_q4___8,levels=c("0","1"))
  data$changesinweight_q4___9.factor = factor(data$changesinweight_q4___9,levels=c("0","1"))
  data$changesinweight_q4___10.factor = factor(data$changesinweight_q4___10,levels=c("0","1"))
  data$changesinweight_q5___1.factor = factor(data$changesinweight_q5___1,levels=c("0","1"))
  data$changesinweight_q5___2.factor = factor(data$changesinweight_q5___2,levels=c("0","1"))
  data$changesinweight_q5___3.factor = factor(data$changesinweight_q5___3,levels=c("0","1"))
  data$changesinweight_q5___4.factor = factor(data$changesinweight_q5___4,levels=c("0","1"))
  data$changesinweight_q5___5.factor = factor(data$changesinweight_q5___5,levels=c("0","1"))
  data$changesinweight_q5___6.factor = factor(data$changesinweight_q5___6,levels=c("0","1"))
  data$changesinweight_q5___7.factor = factor(data$changesinweight_q5___7,levels=c("0","1"))
  data$changesinweight_q5___8.factor = factor(data$changesinweight_q5___8,levels=c("0","1"))
  data$changesinweight_q6.factor = factor(data$changesinweight_q6,levels=c("1","0","2"))
  data$burnout.factor = factor(data$burnout,levels=c("1","2","3","4","5"))
  data$ws_rewarding.factor = factor(data$ws_rewarding,levels=c("1","2","3","4","5"))
  data$ws_satisfied.factor = factor(data$ws_satisfied,levels=c("1","2","3","4","5"))
  data$pathweigh_practice_member_survey_complete.factor = factor(data$pathweigh_practice_member_survey_complete,levels=c("0","1","2"))
  data$send_followup.factor = factor(data$send_followup,levels=c("1","0"))
  data$contact_info_complete.factor = factor(data$contact_info_complete,levels=c("0","1","2"))
  
  levels(data$redcap_event_name.factor)=c("Baseline","Followup12m")
  levels(data$consent.factor)=c("I agree","I decline")
  levels(data$decline.factor)=c("Yes","No")
  
  # Original values provided by Redcap
  # levels(data$pm_role.factor)=c("Physician",
  #                               "Advanced practice provider (NP, PA)",
  #                               "Nurse/RN",
  #                               "Medical Assistant/LPN",
  #                               "Behavioral health (PhD, MSW)",
  #                               "PAR or front desk/scheduling",
  #                               "Care manager",
  #                               "Other patient care (RDN, PharmD)",
  #                               "Administration",
  #                               "Other")
  
  # Values to integrate into the legacy workflow
  levels(data$pm_role.factor)=c("Physician",
                                "NP/PA",
                                "Nurse/RN",
                                "Medical Assistant/LPN",
                                "Behavioral Health (PhD or MSW)",
                                "PAR or Front Desk/Scheduling",
                                "Care Manager",
                                "Other patient care (RDN or PharmD)",
                                "Administration",
                                "Other")
  
  levels(data$pm_gender.factor)=c("Male","Female","Nonbinary","Prefer to not answer")
  levels(data$pm_age.factor)=c("Under 30","30-45","46-60","Over 60","Prefer not to answer")
  levels(data$pm_ptft.factor)=c("Part-time (less than 90% total)","Full-time (90% or higher)","Prefer not to answer")
  levels(data$pm_strggle.factor)=c("Yes","No","Prefer to not answer")
  levels(data$wls_satis_prac.factor)=c("Not satisfied","Somewhat satisfied","Very satisfied","Depends on the patient","Im not sure/dont know")
  levels(data$wls_appropriate.factor)=c("1 Totally disagree","2","3","4","5	Totally agree")
  
  # These were commented out to prevent the column name prefix wls_concern from 
  # getting modified in a pre-built function.
  # levels(data$wls_concern___1.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___2.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___3.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___4.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___5.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___6.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___7.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___8.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___99.factor)=c("Unchecked","Checked")
  # levels(data$wls_concern___10.factor)=c("Unchecked","Checked")
  levels(data$provide_wls.factor)=c("1 Not at all","2","3 Somewhat","4","5 To a great extent")
  levels(data$help_wls.factor)=c("1 Not at all","2","3 Somewhat","4","5 To a great extent")
  levels(data$support_to_id.factor)=c("1 Not at all","2","3 Somewhat","4","5 To a great extent")
  levels(data$support_to_asst.factor)=c("1 Not at all","2","3 Somewhat","4","5 To a great extent")
  levels(data$recognition.factor)=c("1 Not at all","2","3 Somewhat","4","5 To a great extent")
  levels(data$appreciation.factor)=c("1 Not at all","2","3 Somewhat","4","5 To a great extent")
  levels(data$confidence.factor)=c("1 Not confident","2","3","4","5 Completely confident","Not applicable to my role")
  levels(data$effective.factor)=c("1 Not effective","2","3","4","5 Completely effective","Not applicable to my role")
  levels(data$certification.factor)=c("No","Yes","Dont know/Prefer not to answer","Not applicable to my role")
  levels(data$cme.factor)=c("No, never","Yes, but not for a long time","Yes, within the past 3 years","Dont know/prefer not to answer","Not applicable to my role")
  levels(data$any_assistance.factor)=c("Yes","No")
  levels(data$ext_wtloss.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_id.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_edu.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_offer.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_ongoing.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_track.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_show.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_emr.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_motiv.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_challenges.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_therapy.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_nutrtion.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_bariatric.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_programs.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_meds.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_med_impl.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_meals.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_clin_mgmt.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_code_bill.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$ext_other.factor)=c("Not at all","Sometimes","Very Often")
  levels(data$level_of_assistance.factor)=c("I do not provide weight management assistance to patients.","Category 1: Identification, education and referral. I identify patients who might benefit from weight loss, provide education on how weight affects health, discuss strategies for weight loss and sometimes refer patients to programs for weight loss or other assistance. This does not involve ongoing visits for weight.","Category 2: Category 1 plus provide direct assistance such as prescribing medications for weight loss. May include referral to a consistent and varied group of resources such as bariatric surgery, nutrition specialists, specific weight loss programs or other specialists such as endocrinology. Involves ongoing visits for weight loss.","Category 3: Categories 1 and 2 plus having weight management as an area of expertise. Includes having the means to have ongoing visits for weight that involve tracking and monitoring goals over time and effectively handling motivational issues and barriers. May involve enhanced use of EPIC for use with weight loss.")
  
  # These were commented out to prevent the column name prefix wls_concern from 
  # getting modified in a pre-built function.
  # levels(data$learn_more___1.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___2.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___3.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___4.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___5.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___6.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___7.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___8.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___9.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___10.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___11.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___12.factor)=c("Unchecked","Checked")
  # levels(data$learn_more___99.factor)=c("Unchecked","Checked")
  # levels(data$learn_ways___1.factor)=c("Unchecked","Checked")
  # levels(data$learn_ways___2.factor)=c("Unchecked","Checked")
  # levels(data$learn_ways___3.factor)=c("Unchecked","Checked")
  # levels(data$learn_ways___4.factor)=c("Unchecked","Checked")
  # levels(data$learn_ways___5.factor)=c("Unchecked","Checked")
  # levels(data$learn_ways___99.factor)=c("Unchecked","Checked")
  levels(data$pc_discussion.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_opinion.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_jobs_fit.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_chaos.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_rely.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_effort.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_input.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_improve.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_time.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_organized.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_tension.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_thoughtful.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_data.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_share.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_affects.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_available.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_efforts.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_stable.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_change.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_leadership.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_enjoy.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pc_environment.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$changesinweight_q1___1.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q1___2.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q1___3.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q1___4.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q1___5.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q1___6.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q1___7.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q2.factor)=c("Yes","No","Dont Know/Prefer not to answer")
  levels(data$explainchange_q2___1.factor)=c("Unchecked","Checked")
  levels(data$explainchange_q2___2.factor)=c("Unchecked","Checked")
  levels(data$explainchange_q2___3.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___1.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___2.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___3.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___4.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___5.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___6.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q3___7.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___1.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___2.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___3.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___4.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___5.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___6.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___7.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___8.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___9.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q4___10.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___1.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___2.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___3.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___4.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___5.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___6.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___7.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q5___8.factor)=c("Unchecked","Checked")
  levels(data$changesinweight_q6.factor)=c("Yes, Please Describe","No","Dont Know/Not Sure")
  levels(data$burnout.factor)=c("I enjoy my work. I have no symptoms of burnout.","Occasionally I am under stress, and I dont always have as much energy as I once did, but I dont feel burned out.","I am definitely burning out and have one or more symptoms of burnout, such as physical and emotional exhaustion.","The symptoms of burnout that Im experiencing wont go away. I think about frustrations at work a lot.","I feel completely burned out and often wonder if I can go on. I am at the point where I may need some changes or may need to seek some sort of help.")
  levels(data$ws_rewarding.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$ws_satisfied.factor)=c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")
  levels(data$pathweigh_practice_member_survey_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$send_followup.factor)=c("Yes","No")
  levels(data$contact_info_complete.factor)=c("Incomplete","Unverified","Complete")
  
  return(data)

}