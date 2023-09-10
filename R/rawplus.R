#' @title rawplus_ae
#' @description Adverse Events (ae) data in [ rawplus ] format.
#' @format a data frame with 4854 rows and 127 columns.
#' \describe{
#'   \item{studyid}{[ string ] Study Identifier}
#'   \item{siteid}{[ string ] Site Identifier}
#'   \item{invid}{[ string ] Investigator ID}
#'   \item{scrnid}{[ string ] Screen Identifier}
#'   \item{subjid}{[ string ] Subject Identifier}
#'   \item{subjectid}{[ string ] System Subject Identifier}
#'   \item{datapagename}{[ string ] eCRF Page Name}
#'   \item{datapageid}{[ string ] Internal ID for Data Page}
#'   \item{foldername}{[ string ] Folder Name}
#'   \item{instancename}{[ string ] Instance Name}
#'   \item{recordid}{[ string ] Record Identifier}
#'   \item{record_dt}{[ string ] Record Date}
#'   \item{recordposition}{[ float ] Record Position}
#'   \item{mincreated_dts}{[ string ] Earliest Data Creation Date/Time}
#'   \item{maxupdated_dts}{[ string ] Latest Data Update Date/Time}
#'   \item{aeterm}{[ string ] Reported Term for the Adverse Event}
#'   \item{aeser}{[ string ] Serious Event}
#'   \item{aest_dt}{[ string ] Start Date/Time for Adverse Event}
#'   \item{aeen_dt}{[ string ] End Date/Time for Adverse Event}
#'   \item{aeongo}{[ string ] AE Ongoing}
#'   \item{aerel}{[ string ] Pass through label from RAW}
#'   \item{aerel1}{[ string ] Pass through label from RAW}
#'   \item{aerel2}{[ string ] Pass through label from RAW}
#'   \item{aerel3}{[ string ] Pass through label from RAW}
#'   \item{aerelprc}{[ string ] Related to Study Porcedures}
#'   \item{aeacn}{[ string ] Pass through label from RAW}
#'   \item{aeacn1}{[ string ] Pass through label from RAW}
#'   \item{aeacn2}{[ string ] Pass through label from RAW}
#'   \item{aetoxgr}{[ string ] Standard Toxicity Grade}
#'   \item{caseno}{[ string ] Case ID}
#'   \item{aesdth}{[ string ] Result in Death}
#'   \item{aeslife}{[ string ] Is Life Threatening}
#'   \item{aeshosp}{[ string ] Requires or Prolongs Hospitalization}
#'   \item{aehstdat}{[ string ] If Hospitalized, Date Admitted}
#'   \item{aehendat}{[ string ] If Hospitalized, Date Discharged}
#'   \item{aesdisab}{[ string ] Persis of Signif Disability/Incapacity}
#'   \item{aescong}{[ string ] Congenital Anomaly or Birth Defect}
#'   \item{aesmie}{[ string ] Other Medically Important Serious Event}
#'   \item{aediseas}{[ string ] Study Disease-related}
#'   \item{aeprecon}{[ string ] Pre-existing Condition}
#'   \item{aeinter}{[ string ] Intercurrent Illness}
#'   \item{aecm}{[ string ] Concomitant Medications}
#'   \item{aecmspec}{[ string ] Specify Concomitant Medications}
#'   \item{aeoth}{[ string ] Other Causality}
#'   \item{aeothsp}{[ string ] Specify Other Causality}
#'   \item{aeout}{[ string ] Outcome of Advese Event}
#'   \item{subjid_nsv}{[ string ] SUBJECT IDENTIFIER}
#'   \item{scrnid_nsv}{[ string ] SCREENING IDENTIFIER}
#'   \item{subjinit_nsv}{[ string ] SUBJECT INITIALS}
#'   \item{invid_nsv}{[ string ] INVESTIGATOR ID}
#'   \item{subject_nsv}{[ string ] Subject name or identifier}
#'   \item{instanceid_nsv}{[ double ] Internal id for the instance}
#'   \item{folder_nsv}{[ string ] Folder OID}
#'   \item{folderseq_nsv}{[ double ] Folder sequence number}
#'   \item{aeser_std_nsv}{[ string ] AE serious_Coded Value}
#'   \item{aestdat_nsv}{[ timestamp ] Start Date}
#'   \item{aestdat_yy_nsv}{[ double ] Start Date_Year}
#'   \item{aestdat_mm_nsv}{[ double ] Start Date_Month}
#'   \item{aestdat_dd_nsv}{[ double ] Start Date_Day}
#'   \item{aeendat_nsv}{[ timestamp ] End Date}
#'   \item{aeendat_yy_nsv}{[ double ] End Date_Year}
#'   \item{aeendat_mm_nsv}{[ double ] End Date_Month}
#'   \item{aeendat_dd_nsv}{[ double ] End Date_Day}
#'   \item{aeongo_std_nsv}{[ string ] Check if Ongoing_Coded Value}
#'   \item{aerel_std_nsv}{[ string ] Related to Study Treatment_Coded Value}
#'   \item{aerelprc_std_nsv}{[ string ] Related to Study Procedures_Coded Value}
#'   \item{aeacn_std_nsv}{[ double ] Action Taken with Blinded TAF_Coded Value}
#'   \item{aeacn1_std_nsv}{[ double ] Action Taken with Blinded TDF_Coded Value}
#'   \item{aeacn2_std_nsv}{[ double ] Action Taken with Open Label TAF_Coded Value}
#'   \item{aeacnoth1_nsv}{[ double ] Other Action Taken}
#'   \item{aeacnoth1_raw_nsv}{[ string ] Other Action Taken_(Character)}
#'   \item{aeacnoth2_nsv}{[ double ] Medication Required}
#'   \item{aeacnoth2_raw_nsv}{[ string ] Medication Required_(Character)}
#'   \item{aeacnoth4_nsv}{[ double ] Other Treatment Required}
#'   \item{aeacnoth4_raw_nsv}{[ string ] Other Treatment Required_(Character)}
#'   \item{aesev_std_nsv}{[ double ] Severity_Coded Value}
#'   \item{caseno_std_nsv}{[ string ] CASE ID_Coded Value}
#'   \item{oldcase_nsv}{[ string ] Old Case ID}
#'   \item{aerel1_std_nsv}{[ string ] Related to Blinded TAF_Coded Value}
#'   \item{aerel2_std_nsv}{[ string ] Related to Blinded TDF_Coded Value}
#'   \item{aerel3_std_nsv}{[ string ] Related to Open Label TAF_Coded Value}
#'   \item{aesdth_std_nsv}{[ string ] Seriousness Criteria_Coded Value}
#'   \item{aeslife_std_nsv}{[ string ] Life Threatening_Coded Value}
#'   \item{aeshosp_std_nsv}{[ string ] AE, Initial/Prolonged Hospitalization_Coded Value}
#'   \item{aehstdat_nsv}{[ timestamp ] If Hospitalized, Date Admitted}
#'   \item{aehstdat_yy_nsv}{[ double ] If Hospitalized, Date Admitted_Year}
#'   \item{aehstdat_mm_nsv}{[ double ] If Hospitalized, Date Admitted_Month}
#'   \item{aehstdat_dd_nsv}{[ double ] If Hospitalized, Date Admitted_Day}
#'   \item{aehendat_nsv}{[ timestamp ] If Hospitalized, Date Discharged}
#'   \item{aehendat_yy_nsv}{[ double ] If Hospitalized, Date Discharged_Year}
#'   \item{aehendat_mm_nsv}{[ double ] If Hospitalized, Date Discharged_Month}
#'   \item{aehendat_dd_nsv}{[ double ] If Hospitalized, Date Discharged_Day}
#'   \item{aesdisab_std_nsv}{[ string ] Persistent or significant disability_Coded Value}
#'   \item{aescong_std_nsv}{[ string ] Congenital anomaly/birth defect_Coded Value}
#'   \item{aesmie_std_nsv}{[ string ] Other significant medical event_Coded Value}
#'   \item{aealtcau_nsv}{[ string ] Alternative Causality}
#'   \item{aediseas_nsv}{[ double ] Study Disease-related}
#'   \item{aeprecon_nsv}{[ double ] Pre-existing Condition}
#'   \item{aeinter_nsv}{[ double ] Intercurrent Illness}
#'   \item{aecm_nsv}{[ double ] Concomitant Medications}
#'   \item{aeoth_nsv}{[ double ] Other}
#'   \item{aeout_std_nsv}{[ double ] SAE Outcome_Coded Value}
#'   \item{rep_smtp_nsv}{[ string ] Reporter's E-Mail Address}
#'   \item{diction_nsv}{[ string ] CODING DICTIONARY}
#'   \item{dic_ver_nsv}{[ string ] CODING DICTIONARY VERSION}
#'   \item{mdrlltc_nsv}{[ string ] MEDDRA LOWEST LEVEL TERM CODE}
#'   \item{mdrllt_nsv}{[ string ] MEDDRA LOWEST LEVEL TERM}
#'   \item{mdrsocc_nsv}{[ string ] MEDDRA SYSTEM ORGAN CLASS CODE}
#'   \item{mdrsoc_nsv}{[ string ] MEDDRA SYSTEM ORGAN CLASS}
#'   \item{mdrhlgtc_nsv}{[ string ] MEDDRA HIGH LEVEL GROUP TERM CODE}
#'   \item{mdrhlgt_nsv}{[ string ] MEDDRA HIGH LEVEL GROUP TERM}
#'   \item{mdrhltc_nsv}{[ string ] MEDDRA HIGH LEVEL TERM CODE}
#'   \item{mdrhlt_nsv}{[ string ] MEDDRA HIGH LEVEL TERM}
#'   \item{mdrptc_nsv}{[ string ] MEDDRA PREFERRED TERM CODE}
#'   \item{mdrpt_nsv}{[ string ] MEDDRA PREFERRED TERM}
#'   \item{aesel_nsv}{[ string ] Selected Adverse Event}
#'   \item{aeselcat_nsv}{[ string ] Selected Adverse Event Category}
#'   \item{aeseltyp_nsv}{[ string ] Selected Adverse Event Type}
#'   \item{aebon_nsv}{[ string ] Selected AE Bone Event}
#'   \item{aeboncat_nsv}{[ string ] Selected AE Bone Event Category}
#'   \item{aebontyp_nsv}{[ string ] Selected AE Bone Event Type}
#'   \item{htension_nsv}{[ string ] Hypertension}
#'   \item{diabetes_nsv}{[ string ] Diabetes}
#'   \item{carddis_nsv}{[ string ] Cardiovascular Disease}
#'   \item{hlipdem_nsv}{[ string ] Hyperlipidemia}
#'   \item{uveitis_nsv}{[ string ] Uveitis}
#'   \item{treatmentemergent}{[ character ]}
#' }
#' @source ./data-raw/rawplus/ae.parquet
"rawplus_ae"

#' @title rawplus_dm
#' @description Demographics (dm) data in [ rawplus ] format.
#' @format a data frame with 1301 rows and 48 columns.
#' \describe{
#'   \item{studyid}{[ string ] Study Identifier}
#'   \item{siteid}{[ string ] Site Identifier}
#'   \item{invid}{[ string ] Investigator ID}
#'   \item{scrnid}{[ string ] Screen Identifier}
#'   \item{subjid}{[ string ] Subject Identifier}
#'   \item{subjectid}{[ string ] System Subject Identifier}
#'   \item{datapagename}{[ string ] eCRF Page Name}
#'   \item{datapageid}{[ string ] Internal ID for Data Page}
#'   \item{foldername}{[ string ] Folder Name}
#'   \item{instancename}{[ string ] Instance Name}
#'   \item{recordid}{[ string ] Record Identifier}
#'   \item{record_dt}{[ string ] Record Date}
#'   \item{recordposition}{[ float ] Record Position}
#'   \item{mincreated_dts}{[ string ] Earliest Data Creation Date/Time}
#'   \item{maxupdated_dts}{[ string ] Latest Data Update Date/Time}
#'   \item{brthdat}{[ string ] Date of Birth}
#'   \item{agerep}{[ string ] Reported Age}
#'   \item{sex}{[ string ] Sex}
#'   \item{ethnic}{[ string ] Ethnicity}
#'   \item{race}{[ string ] Race}
#'   \item{raceoth}{[ string ] Race Other}
#'   \item{racescat}{[ string ] Race Sub-Category}
#'   \item{racesoth}{[ string ] Race Sub-Category Other}
#'   \item{subjid_nsv}{[ string ] SUBJECT IDENTIFIER}
#'   \item{scrnid_nsv}{[ string ] SCREENING IDENTIFIER}
#'   \item{subjinit_nsv}{[ string ] SUBJECT INITIALS}
#'   \item{invid_nsv}{[ string ] INVESTIGATOR ID}
#'   \item{subject_nsv}{[ string ] Subject name or identifier}
#'   \item{instanceid_nsv}{[ double ] Internal id for the instance}
#'   \item{folder_nsv}{[ string ] Folder OID}
#'   \item{folderseq_nsv}{[ double ] Folder sequence number}
#'   \item{brthdat_nsv}{[ timestamp ] Date of Birth}
#'   \item{brthdat_yy_nsv}{[ double ] Date of Birth_Year}
#'   \item{brthdat_mm_nsv}{[ double ] Date of Birth_Month}
#'   \item{brthdat_dd_nsv}{[ double ] Date of Birth_Day}
#'   \item{age_nsv}{[ double ] Age (in years)}
#'   \item{sex_std_nsv}{[ string ] Sex at birth_Coded Value}
#'   \item{ethnic_std_nsv}{[ string ] Ethnicity_Coded Value}
#'   \item{race_std_nsv}{[ string ] Race_Coded Value}
#'   \item{racescat_std_nsv}{[ string ] Race Sub-category_Coded Value}
#'   \item{firstdosedate}{[ Date ]}
#'   \item{lastdosedate}{[ Date ]}
#'   \item{lastparticipantdate}{[ Date ]}
#'   \item{firstparticipantdate}{[ Date ]}
#'   \item{timeontreatment}{[ int ] Time on Treatment}
#'   \item{timeonstudy}{[ int ] Time on Study}
#'   \item{country}{[ character ]}
#'   \item{enrollyn}{[ character ]}
#' }
#' @source ./data-raw/rawplus/dm.parquet
"rawplus_dm"

#' @title rawplus_ex
#' @description Exposure (ex) data in [ rawplus ] format.
#' @format a data frame with 4876 rows and 53 columns.
#' \describe{
#'   \item{studyid}{[ string ] Study Identifier}
#'   \item{siteid}{[ string ] Site Identifier}
#'   \item{invid}{[ string ] Investigator ID}
#'   \item{scrnid}{[ string ] Screen Identifier}
#'   \item{subjid}{[ string ] Subject Identifier}
#'   \item{subjectid}{[ string ] System Subject Identifier}
#'   \item{datapagename}{[ string ] eCRF Page Name}
#'   \item{datapageid}{[ string ] Internal ID for Data Page}
#'   \item{foldername}{[ string ] Folder Name}
#'   \item{instancename}{[ string ] Instance Name}
#'   \item{recordid}{[ string ] Record Identifier}
#'   \item{record_dt}{[ string ] Record Date}
#'   \item{recordposition}{[ float ] Record Position}
#'   \item{mincreated_dts}{[ string ] Earliest Data Creation Date/Time}
#'   \item{maxupdated_dts}{[ string ] Latest Data Update Date/Time}
#'   \item{extrt}{[ string ] Treatment Name}
#'   \item{exds}{[ string ] Actual Dose}
#'   \item{exdsu}{[ string ] Acutal Dose Units}
#'   \item{exdsfrmn}{[ int ] Number of Dose Form}
#'   \item{exdsfrm}{[ string ] Dose Form}
#'   \item{exdsfrq}{[ string ] Dosing Frequecy}
#'   \item{exdsfrot}{[ string ] Other Dosing Frequecy}
#'   \item{exrte}{[ string ] Route}
#'   \item{exst_dt}{[ string ] Start Date/Time of Treatment}
#'   \item{exen_dt}{[ string ] End Date/Time of Treatment}
#'   \item{exongo}{[ string ] EX Ongoing}
#'   \item{experm}{[ string ] Permanently Withdrawn}
#'   \item{subjid_nsv}{[ string ] SUBJECT IDENTIFIER}
#'   \item{scrnid_nsv}{[ string ] SCREENING IDENTIFIER}
#'   \item{subjinit_nsv}{[ string ] SUBJECT INITIALS}
#'   \item{invid_nsv}{[ string ] INVESTIGATOR ID}
#'   \item{subject_nsv}{[ string ] Subject name or identifier}
#'   \item{instanceid_nsv}{[ double ] Internal id for the instance}
#'   \item{folder_nsv}{[ string ] Folder OID}
#'   \item{folderseq_nsv}{[ double ] Folder sequence number}
#'   \item{extrt_std_nsv}{[ string ] Drug Name_Coded Value}
#'   \item{exdstxt_nsv}{[ double ] Dose per administration}
#'   \item{exdosu_std_nsv}{[ double ] Dose Units_Coded Value}
#'   \item{exdsfrmn_raw_nsv}{[ string ] Number of insert Dose Form (Tablets)_(Character)}
#'   \item{exdosfrm_std_nsv}{[ double ] Dose Form_Coded Value}
#'   \item{exdosfrq_std_nsv}{[ string ] Dosing Frequency_Coded Value}
#'   \item{exroute_std_nsv}{[ double ] Route_Coded Value}
#'   \item{exrtoth_nsv}{[ string ] EXRTOTH}
#'   \item{exstdat_nsv}{[ timestamp ] Start Date/Dose Date}
#'   \item{exstdat_yy_nsv}{[ double ] Start Date/Dose Date_Year}
#'   \item{exstdat_mm_nsv}{[ double ] Start Date/Dose Date_Month}
#'   \item{exstdat_dd_nsv}{[ double ] Start Date/Dose Date_Day}
#'   \item{exendat_nsv}{[ timestamp ] Stop Date}
#'   \item{exendat_yy_nsv}{[ double ] Stop Date_Year}
#'   \item{exendat_mm_nsv}{[ double ] Stop Date_Month}
#'   \item{exendat_dd_nsv}{[ double ] Stop Date_Day}
#'   \item{exongo_std_nsv}{[ string ] Check if Ongoing_Coded Value}
#'   \item{experm_nsv}{[ string ] if study drug was permanently withdrawn}
#' }
#' @source ./data-raw/rawplus/ex.parquet
"rawplus_ex"

#' @title rawplus_ixrsrand
#' @description Randomization (ixrsrand) data in [ rawplus ] format.
#' @format a data frame with 1301 rows and 18 columns.
#' \describe{
#'   \item{studyid}{[ string ]}
#'   \item{invid}{[ string ]}
#'   \item{invnam}{[ string ]}
#'   \item{oinvid}{[ string ]}
#'   \item{oinvnam}{[ string ]}
#'   \item{country}{[ string ]}
#'   \item{ocountry}{[ string ]}
#'   \item{txtyp_nsv}{[ string ]}
#'   \item{scrnid_nsv}{[ string ]}
#'   \item{subjid}{[ string ]}
#'   \item{subjinit_nsv}{[ string ]}
#'   \item{hbvdna_nsv}{[ string ]}
#'   \item{virtreat_nsv}{[ string ]}
#'   \item{trtgrpc}{[ string ]}
#'   \item{randnum}{[ string ]}
#'   \item{dispid_nsv}{[ double ]}
#'   \item{status}{[ string ]}
#'   \item{rand_dt}{[ string ]}
#' }
#' @source ./data-raw/rawplus/ixrsrand.parquet
"rawplus_ixrsrand"

#' @title rawplus_lb
#' @description Labs (lb) data in [ rawplus ] format.
#' @format a data frame with 1132070 rows and 13 columns.
#' \describe{
#'   \item{subjid}{[ string ] Subject ID or Number}
#'   \item{visnam}{[ string ] Visit Name}
#'   \item{visnum}{[ float ] Visit ID or Number}
#'   \item{lb_dt}{[ string ] Actual Collection Date and Time}
#'   \item{battrnam}{[ string ] Battery Name}
#'   \item{lbtstnam}{[ string ] Test Name ID}
#'   \item{siresn}{[ float ] SI Numeric Result}
#'   \item{sinrlo}{[ string ] SI Reference Range Low}
#'   \item{sinrhi}{[ string ] SI Reference Range High}
#'   \item{toxgrg_nsv}{[ string ] Toxicity Grade}
#'   \item{alrtfl}{[ string ] Alert Flag}
#'   \item{treatmentemergent}{[ character ]}
#'   \item{alertsimplified}{[ character ]}
#' }
#' @source ./data-raw/rawplus/lb.parquet
"rawplus_lb"

#' @title rawplus_sdrgcomp
#' @description Treatment Disposition (sdrgcomp) data in [ rawplus ] format.
#' @format a data frame with 689 rows and 36 columns.
#' \describe{
#'   \item{studyid}{[ string ] Study Identifier}
#'   \item{siteid}{[ string ] Site Identifier}
#'   \item{invid}{[ string ] Investigator ID}
#'   \item{scrnid}{[ string ] Screen Identifier}
#'   \item{subjid}{[ string ] Subject Identifier}
#'   \item{subjectid}{[ string ] System Subject Identifier}
#'   \item{datapagename}{[ string ] eCRF Page Name}
#'   \item{datapageid}{[ string ] Internal ID for Data Page}
#'   \item{foldername}{[ string ] Folder Name}
#'   \item{instancename}{[ string ] Instance Name}
#'   \item{recordid}{[ string ] Record Identifier}
#'   \item{record_dt}{[ string ] Record Date}
#'   \item{recordposition}{[ float ] Record Position}
#'   \item{mincreated_dts}{[ string ] Earliest Data Creation Date/Time}
#'   \item{maxupdated_dts}{[ string ] Latest Data Update Date/Time}
#'   \item{sdrgyn}{[ string ] Did subject complete study drug dosing?}
#'   \item{sdrgreas}{[ string ] Reason for study drug discontinue}
#'   \item{sdrgterm}{[ string ] Discontinuation reasons}
#'   \item{subjid_nsv}{[ string ] SUBJECT IDENTIFIER}
#'   \item{scrnid_nsv}{[ string ] SCREENING IDENTIFIER}
#'   \item{subjinit_nsv}{[ string ] SUBJECT INITIALS}
#'   \item{invid_nsv}{[ string ] INVESTIGATOR ID}
#'   \item{subject_nsv}{[ string ] Subject name or identifier}
#'   \item{instanceid_nsv}{[ double ] Internal id for the instance}
#'   \item{folder_nsv}{[ string ] Folder OID}
#'   \item{folderseq_nsv}{[ double ] Folder sequence number}
#'   \item{sdrgyn_std_nsv}{[ string ] Open label study drug completed_Coded Value}
#'   \item{sdrgreas_std_nsv}{[ string ] Reason for study drug discontinuation_Coded Value}
#'   \item{sdrgol_nsv}{[ string ] Will subject go to Open-Label}
#'   \item{sdrgol_std_nsv}{[ string ] Will subject go to Open-Label_Coded Value}
#'   \item{sdrgfu_nsv}{[ string ] Will subject enter 24-week trt free fup}
#'   \item{sdrgfu_std_nsv}{[ string ] Will subject enter 24-week trt free fup_Coded Value}
#'   \item{sdrgtrt_nsv}{[ string ] Did subject start another HBV therapy}
#'   \item{sdrgtrt_std_nsv}{[ string ] Did subject start another HBV therapy_Coded Value}
#'   \item{phase}{[ character ]}
#'   \item{extrt}{[ character ]}
#' }
#' @source ./data-raw/rawplus/sdrgcomp.parquet
"rawplus_sdrgcomp"

#' @title rawplus_studcomp
#' @description Study Disposition (studcomp) data in [ rawplus ] format.
#' @format a data frame with 133 rows and 31 columns.
#' \describe{
#'   \item{studyid}{[ string ] Study Identifier}
#'   \item{siteid}{[ string ] Site Identifier}
#'   \item{invid}{[ string ] Investigator ID}
#'   \item{scrnid}{[ string ] Screen Identifier}
#'   \item{subjid}{[ string ] Subject Identifier}
#'   \item{subjectid}{[ string ] System Subject Identifier}
#'   \item{datapagename}{[ string ] eCRF Page Name}
#'   \item{datapageid}{[ string ] Internal ID for Data Page}
#'   \item{foldername}{[ string ] Folder Name}
#'   \item{instancename}{[ string ] Instance Name}
#'   \item{recordid}{[ string ] Record Identifier}
#'   \item{record_dt}{[ string ] Record Date}
#'   \item{recordposition}{[ float ] Record Position}
#'   \item{mincreated_dts}{[ string ] Earliest Data Creation Date/Time}
#'   \item{maxupdated_dts}{[ string ] Latest Data Update Date/Time}
#'   \item{compyn}{[ string ] Subject complete duration of the study?}
#'   \item{compreas}{[ string ] Reason for study discontinue}
#'   \item{subjid_nsv}{[ string ] SUBJECT IDENTIFIER}
#'   \item{scrnid_nsv}{[ string ] SCREENING IDENTIFIER}
#'   \item{subjinit_nsv}{[ string ] SUBJECT INITIALS}
#'   \item{invid_nsv}{[ string ] INVESTIGATOR ID}
#'   \item{subject_nsv}{[ string ] Subject name or identifier}
#'   \item{instanceid_nsv}{[ double ] Internal id for the instance}
#'   \item{folder_nsv}{[ string ] Folder OID}
#'   \item{folderseq_nsv}{[ double ] Folder sequence number}
#'   \item{compyn_std_nsv}{[ string ] Subject complete duration of the study?_Coded Value}
#'   \item{compreas_std_nsv}{[ string ] If No, reason for study discontinuation_Coded Value}
#'   \item{compfu_nsv}{[ string ] 24-week trt free fup completed}
#'   \item{compfu_std_nsv}{[ string ] 24-week trt free fup completed_Coded Value}
#'   \item{comptrt_nsv}{[ string ] Did subject start another HBV therapy}
#'   \item{comptrt_std_nsv}{[ string ] Did subject start another HBV therapy_Coded Value}
#' }
#' @source ./data-raw/rawplus/studcomp.parquet
"rawplus_studcomp"

#' @title rawplus_visdt
#' @description Visits (visdt) data in [ rawplus ] format.
#' @format a data frame with 28093 rows and 48 columns.
#' \describe{
#'   \item{studyid}{[ string ] Study Identifier}
#'   \item{siteid}{[ string ] Site Identifier}
#'   \item{invid}{[ string ] Investigator ID}
#'   \item{scrnid}{[ string ] Screen Identifier}
#'   \item{subjid}{[ string ] Subject Identifier}
#'   \item{subjectid}{[ string ] System Subject Identifier}
#'   \item{datapagename}{[ string ] eCRF Page Name}
#'   \item{datapageid}{[ string ] Internal ID for Data Page}
#'   \item{foldername}{[ string ] Folder Name}
#'   \item{instancename}{[ string ] Instance Name}
#'   \item{recordid}{[ string ] Record Identifier}
#'   \item{record_dt}{[ string ] Record Date}
#'   \item{recordposition}{[ float ] Record Position}
#'   \item{mincreated_dts}{[ string ] Earliest Data Creation Date/Time}
#'   \item{maxupdated_dts}{[ string ] Latest Data Update Date/Time}
#'   \item{visit_dt}{[ string ] Visit Date}
#'   \item{peperf}{[ string ] Complete Physical Exam Performed}
#'   \item{pereasnd}{[ string ] Reason Physical Exam Not Performed}
#'   \item{svvs}{[ string ] Vital Signs}
#'   \item{svaecm}{[ string ] Evaluation of AE and/or CM}
#'   \item{svecg}{[ string ] ECG}
#'   \item{svpk}{[ string ] PK and Associated Dose}
#'   \item{svlab}{[ string ] Lab Evaluations}
#'   \item{svpregtest}{[ string ] Pregnancy Test}
#'   \item{svsd}{[ string ] Study Drug Adjustment or Dispensation}
#'   \item{svothsp}{[ string ] Other Specify}
#'   \item{subjid_nsv}{[ string ] SUBJECT IDENTIFIER}
#'   \item{scrnid_nsv}{[ string ] SCREENING IDENTIFIER}
#'   \item{subjinit_nsv}{[ string ] SUBJECT INITIALS}
#'   \item{invid_nsv}{[ string ] INVESTIGATOR ID}
#'   \item{subject_nsv}{[ string ] Subject name or identifier}
#'   \item{instanceid_nsv}{[ double ] Internal id for the instance}
#'   \item{folder_nsv}{[ string ] Folder OID}
#'   \item{folderseq_nsv}{[ double ] Folder sequence number}
#'   \item{visitdat_nsv}{[ timestamp ] Visit Date}
#'   \item{visitdat_yy_nsv}{[ double ] Visit Date_Year}
#'   \item{visitdat_mm_nsv}{[ double ] Visit Date_Month}
#'   \item{visitdat_dd_nsv}{[ double ] Visit Date_Day}
#'   \item{peperf_std_nsv}{[ string ] Complete physical examination performed? _Coded Value_}
#'   \item{svvs_nsv}{[ double ] Vital Signs}
#'   \item{svaecm_nsv}{[ double ] Concomitant Medications}
#'   \item{svecg_nsv}{[ double ] ECG}
#'   \item{svpk_nsv}{[ double ] PK and Associated Dose}
#'   \item{svlab_nsv}{[ double ] Lab Evaluations}
#'   \item{svpregtest_nsv}{[ double ] Urine Pregnancy Test}
#'   \item{svsd_nsv}{[ double ] Study Drug Adjustment or Dispensation}
#'   \item{svhcc_nsv}{[ double ] HCC Surveillance}
#'   \item{svhcc_raw_nsv}{[ string ] HCC Surveillance_(Character)}
#' }
#' @source ./data-raw/rawplus/visdt.parquet
"rawplus_visdt"
