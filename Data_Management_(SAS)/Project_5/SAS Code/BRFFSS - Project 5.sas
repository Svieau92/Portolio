*************	P	R	O	G	R	A	M		H	E	A	D	E	R	*****************
*****************************************************************************************
*																						*
*	PROGRAM:	Project 5 - Creating a statistical report.sas                   		*
*	PURPOSE:	Perform Data Analysis for Final Project			                        *
*	AUTHOR:		Sean Vieau																*
*	CREATED:	04 Dec 2024															*
*	                                                                                    *
*	COURSE:		BIOS 6680 - Data Management Using SAS                                   *
*	DATA USED:	BRFSS_Final                                                         *
*****************************************************************************************
***********************************************************************************; RUN;

*****************************************************************************************
RESEARCH QUESTIONS
*****************************************************************************************;
/*
1. Is there a significant difference in BMI between California and North Carolina?
2. Is Marijuana usage a significant predictor of cancer, while controlling for years 
   of cigarette smoking, alcohol use, and age?*/


*****************************************************************************************
MACROS
*****************************************************************************************;
* Set Courseroot;
%LET CourseRoot = /home/u63376223/sasuser.v94/Data Management Using SAS;

*****************************************************************************************
LIBRARIES
*****************************************************************************************;
* Create libraries;
LIBNAME Source "&CourseRoot/BRFSS/Data/1_Source";
LIBNAME Impt "&CourseRoot/BRFSS/Data/2_Import";

*****************************************************************************************
SYSTEM OPTIONS
*****************************************************************************************;
OPTIONS NOFMTERR;

*****************************************************************************************
FORMATS
*****************************************************************************************;
*How do we want the variable names to display??;
PROC FORMAT;
	VALUE $Varnames
	"BMI" = "BMI"
	"SmokeTime" = "Years Smoked"
	"Age" = "Age";
	RUN;

*****************************************************************************************
CREATE VARIABLES
*****************************************************************************************;


*****************************************************************************************
CREATE TABLE 1 A and B: DESCRIPTIVE STATISTICS
*****************************************************************************************;

*Use ODS output to make it beautiful;

******************************
TABLE 1A
******************************;
TITLE "Table 1A. Means and Standard Deviation";
PROC MEANS DATA = IMPT.BRFSS_FINAL MEAN STDDEV MAXDEC = 2;
	VAR BMI SmokeTime Age;
	RUN;
	
ods output Summary=MeansOutput; /* Create a dataset named MeansOutput */
proc means data=IMPT.BRFSS_FINAL mean stddev;
    var BMI SmokeTime Age;
run;

proc print data=MeansOutput; /* Print the resulting dataset */
run;


*Step 1- Use ODS TRACE to identify name of Table;
*Step 2- Use ODS OUTPUT to create a dataset;
/*	ods trace on;*/
ODS OUTPUT Table = Table1_pre;
PROC TABULATE DATA = IMPT.BRFSS_FINAL MISSING;
	VAR BMI SmokeTime Age;
	TABLE BMI SmokeTime Age, mean*f=comma9.2 std*f=comma9.2/nocellmerge;
	RUN;
/*	ods trace off;*/

* Step 3- Examine table;
PROC PRINT DATA = Table1_pre;
	RUN;
	
* Step 4-  Clean the dataset with the output to look pretty;
DATA Table1_pre2;
	SET Table1_pre;
		BMI = strip(put(BMI_mean,8.2))||" ("||strip(put(BMI_std, 8.2))||")";
		SmokeTime = strip(put(SmokeTime_mean,8.2))||" ("||strip(put(SmokeTime_std, 8.2))||")";
		Age = strip(put(Age_mean,8.2))||" ("||strip(put(Age_std, 8.2))||")";
		RUN;

* Step 5- Examine table;
PROC PRINT DATA = table1_pre2;
	VAR BMI SmokeTime Age;
	RUN;
	
*Step 5. Tranpose the data;
PROC TRANSPOSE DATA = Table1_Pre2 OUT = Table1_Clean(RENAME = (_NAME_ = Variable Col1 = MeanSD));
	VAR BMI SmokeTime Age;
	RUN;
	
* Reapply Labels;
DATA Table1_Clean;
	SET Table1_Clean;
	LABEL Variable = "Variable" MeanSd = "Mean(SD)";
	FORMAT Variable $Varnames.;
	RUN;

* Examine Final clean table 1a;
PROC PRINT DATA = Table1_Clean LABEL NOOBS;
	RUN;
TITLE;

******************************
TABLE 1B
******************************;

*Step 1- Use ODS TRACE to identify name of Table;
*Step 2- Use OUT to create a dataset;
*ODS TRACE ON;
TITLE "Table 1B. Frequency and Proportion";
PROC FREQ DATA = IMPT.BRFSS_FINAL;
	TABLES Marijuana / OUT = Marijuana_Freq;
	TABLES Cancer / OUT = Cancer_Freq;
	TABLES Alcohol / OUT = Alcohol_Freq;
	RUN;
*ODS TRACE OFF;

DATA WORK.TEST;
	SET IMPT.BRFSS_FINAL;
	RUN;
* Step 3- Examine data set;
* Handle Missing;
DATA Marijuana_Freq;
	SET Marijuana_Freq;
	IF Marijuana = . THEN DELETE;
	ELSE IF Marijuana = 1 THEN Marijuana_Status = "Marijuana Usage: Yes";
	ELSE IF Marijuana = 2 THEN Marijuana_Status = "Marijuana Usage: No";
	DROP Marijuana;
	RENAME Marijuana_Status = Marijuana;
	RUN;
	
PROC PRINT DATA = Marijuana_Freq;
	RUN;
	
* Handle Missing;
DATA Cancer_Freq;
	SET Cancer_Freq;
	IF Cancer = . THEN DELETE;
	ELSE IF Cancer = 1 THEN Cancer_Status = "Had Cancer: Yes";
	ELSE IF Cancer = 2 THEN Cancer_Status = "Had Cancer: No";
	DROP Cancer;
	RENAME Cancer_Status = Cancer;
	RUN;

PROC PRINT DATA = Cancer_Freq;
	RUN;
	
* Handle Missing;
DATA Alcohol_Freq;
	SET Alcohol_Freq;
	IF Alcohol = . THEN DELETE;
	ELSE IF Alcohol = 1 THEN Alcohol_Status = "Drink Alcohol: Yes";
	ELSE IF Alcohol = 2 THEN Alcohol_Status = "Drink Alcohol: No";
	DROP Alcohol;
	RENAME Alcohol_Status = Alcohol;
	RUN;
	
PROC PRINT DATA = Alcohol_Freq;
	RUN;
	
*Step 4- Concatanate the data;
DATA Table1b_Combine;
	SET Marijuana_Freq(Rename = (Marijuana = Variable))
		Cancer_Freq(Rename = (Cancer = Variable))
		Alcohol_Freq(Rename = (Alcohol = Variable));
	N_PERCENT = strip(put(count,8.2))||" ("||strip(put(round(percent,0.2),8.2))||"%)";
	LABEL N_PERCENT = "N(%)" Variable = "Variable";
	DROP COUNT PERCENT;
	RUN;
	
PROC PRINT DATA = Table1b_Combine LABEL NOOBS;
	RUN;
TITLE;

*****************************************************************************************
CREATE TABLE 2: TESTS BY CATEGORIES
*****************************************************************************************;



*Step 1- Use ODS TRACE to identify name of Table;
*Step 2- Use ODS OUTPUT to create a dataset;
/*	ods trace on;*/
	ods output Table=Table2_pre;
	proc tabulate data=CanAnl.cananalysis1 missing;
	class TxNm;
	var LesionSumCmMon0 LesionSumCmMon6 LesionRatio AgeAtEnroll;
	table LesionSumCmMon0 LesionSumCmMon6 LesionRatio AgeAtEnroll,(TxNm)*(mean*f=comma9.1 std*f=comma9.1)/nocellmerge;
	run;
/*	ods trace off;*/


***************;
PROC PRINT DATA = IMPT.BRFSS_final (OBS = 10);
RUN;

