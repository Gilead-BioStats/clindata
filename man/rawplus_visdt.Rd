% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rawplus.R
\docType{data}
\name{rawplus_visdt}
\alias{rawplus_visdt}
\title{rawplus_visdt}
\format{
a data frame with 28093 rows and 48 columns.
\describe{
\item{studyid}{\link{ string } Study Identifier}
\item{siteid}{\link{ string } Site Identifier}
\item{invid}{\link{ string } Investigator ID}
\item{scrnid}{\link{ string } Screen Identifier}
\item{subjid}{\link{ string } Subject Identifier}
\item{subjectid}{\link{ string } System Subject Identifier}
\item{datapagename}{\link{ string } eCRF Page Name}
\item{datapageid}{\link{ string } Internal ID for Data Page}
\item{foldername}{\link{ string } Folder Name}
\item{instancename}{\link{ string } Instance Name}
\item{recordid}{\link{ string } Record Identifier}
\item{record_dt}{\link{ string } Record Date}
\item{recordposition}{\link{ float } Record Position}
\item{mincreated_dts}{\link{ string } Earliest Data Creation Date/Time}
\item{maxupdated_dts}{\link{ string } Latest Data Update Date/Time}
\item{visit_dt}{\link{ string } Visit Date}
\item{peperf}{\link{ string } Complete Physical Exam Performed}
\item{pereasnd}{\link{ string } Reason Physical Exam Not Performed}
\item{svvs}{\link{ string } Vital Signs}
\item{svaecm}{\link{ string } Evaluation of AE and/or CM}
\item{svecg}{\link{ string } ECG}
\item{svpk}{\link{ string } PK and Associated Dose}
\item{svlab}{\link{ string } Lab Evaluations}
\item{svpregtest}{\link{ string } Pregnancy Test}
\item{svsd}{\link{ string } Study Drug Adjustment or Dispensation}
\item{svothsp}{\link{ string } Other Specify}
\item{subjid_nsv}{\link{ string } SUBJECT IDENTIFIER}
\item{scrnid_nsv}{\link{ string } SCREENING IDENTIFIER}
\item{subjinit_nsv}{\link{ string } SUBJECT INITIALS}
\item{invid_nsv}{\link{ string } INVESTIGATOR ID}
\item{subject_nsv}{\link{ string } Subject name or identifier}
\item{instanceid_nsv}{\link{ double } Internal id for the instance}
\item{folder_nsv}{\link{ string } Folder OID}
\item{folderseq_nsv}{\link{ double } Folder sequence number}
\item{visitdat_nsv}{\link{ timestamp } Visit Date}
\item{visitdat_yy_nsv}{\link{ double } Visit Date_Year}
\item{visitdat_mm_nsv}{\link{ double } Visit Date_Month}
\item{visitdat_dd_nsv}{\link{ double } Visit Date_Day}
\item{peperf_std_nsv}{\link{ string } Complete physical examination performed? \emph{Coded Value}}
\item{svvs_nsv}{\link{ double } Vital Signs}
\item{svaecm_nsv}{\link{ double } Concomitant Medications}
\item{svecg_nsv}{\link{ double } ECG}
\item{svpk_nsv}{\link{ double } PK and Associated Dose}
\item{svlab_nsv}{\link{ double } Lab Evaluations}
\item{svpregtest_nsv}{\link{ double } Urine Pregnancy Test}
\item{svsd_nsv}{\link{ double } Study Drug Adjustment or Dispensation}
\item{svhcc_nsv}{\link{ double } HCC Surveillance}
\item{svhcc_raw_nsv}{\link{ string } HCC Surveillance_(Character)}
}
}
\source{
./data-raw/rawplus/visdt.parquet
}
\usage{
rawplus_visdt
}
\description{
Visits (visdt) data in \link{ rawplus } format.
}
\keyword{datasets}
