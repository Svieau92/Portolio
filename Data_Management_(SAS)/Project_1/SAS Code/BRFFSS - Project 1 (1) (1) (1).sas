*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    0.1 - BRFSS Project Part 1 - Read in Data                               *
*   PURPOSE:    Creates the folder structure used for the BRFSS Project                 *
*   AUTHOR:     Sean Vieau	                                                            *
*   CREATED:    2024-09-23                                                              *
*                                                                                       *
*   COURSE:     BIOS 6680 - Data Management Using SAS                                   *
*   DATA USED:  BRFSS                                                                  *
*   SOFTWARE:   SAS (r) Proprietary Software 9.4 (TS1M4)                                *
*   MODIFIED:   DATE        BY  REASON                                                  *
*               ----------  --- ------------------------------------------------------- *
*                																        *
*                                                                                       *
*****************************************************************************************
***********************************************************************************; RUN;


*****************************************************************************************
* Step 1: Create Project Folder
*****************************************************************************************;
* Note: Code modified from the code from class to create BIOS 6680 Course Folder;

* Set Courseroot;
%LET CourseRoot = /home/u63376223/sasuser.v94/Data Management Using SAS;

* Create Folders;
OPTIONS DLCREATEDIR;

LIBNAME Create "&CourseRoot/BRFSS";
LIBNAME Create "&CourseRoot/BRFSS/Data";
LIBNAME Create "&CourseRoot/BRFSS/Data/1_Source";
LIBNAME Create "&CourseRoot/BRFSS/Data/2_Import";
LIBNAME Create "&CourseRoot/BRFSS/Data/3_Tabulations";
LIBNAME Create "&CourseRoot/BRFSS/Data/4_Analysis";
LIBNAME Create "&CourseRoot/BRFSS/Data/5_Results";
LIBNAME Create "&CourseRoot/BRFSS/Documents";
LIBNAME Create "&CourseRoot/BRFSS/Programs";

LIBNAME Create CLEAR;

OPTIONS NODLCREATEDIR;

RUN;

;   *'; *"; */; QUIT;   RUN;
*   End of Program   *; RUN;

*****************************************************************************************
* Step 2: Create Libraries
*****************************************************************************************;

LIBNAME Source "&CourseRoot/BRFSS/Data/1_Source";
LIBNAME Impt "&CourseRoot/BRFSS/Data/2_Import";

*****************************************************************************************
* Step 3: Read in Demographics Data
*****************************************************************************************;

*** Step 3.1 Race and Ethnicity;

PROC IMPORT 
		DATAFILE =  "&CourseRoot/BRFSS/Data/1_Source/race_eth.xlsx"
		OUT 	= 	Impt.race_eth
		DBMS 	= 	XLSX
		REPLACE ;
	RUN;
	
*** Step 3.2 General Demographics;
DATA Impt.gen_dem;
	INFILE "&CourseRoot/BRFSS/Data/1_Source/gen_dem.csv" MISSOVER DSD FIRSTOBS = 2;
	INPUT 	marital			
			trnsgndr
			_sex
			_age5year
			id				:$13.		;
	RUN;

PROC PRINT DATA = Impt.gen_dem (OBS = 6);
	RUN;
	
* Check that we successfully have trnsgender values imported. 
  This was rarer in the data set so we will sort by it. Looks good;
PROC SORT DATA = Impt.gen_dem
	OUT = sorted_data;
	BY DESCENDING trnsgndr;
	RUN;

*** Step 3.3 Social and Economic Status;
DATA Impt.socioec;
	INFILE "&CourseRoot/BRFSS/Data/1_Source/socioec.csv" MISSOVER DSD FIRSTOBS = 2;
	INPUT	educa		
			employ1
			income3
			seqno		:$13.		;
	RUN;

*** Step 3.4 Insurance;
DATA Impt.insurance;
	INFILE "&CourseRoot/BRFSS/Data/1_Source/insurance.txt" DELIMITER = " " MISSOVER DSD FIRSTOBS = 2;
	INPUT 	priminisr		
			persdoc3
			medcost1
			checkup1
			seqno			;
	RUN;

*** Step 3.5 Disabilites;
DATA Impt.disability;
	INFILE "&CourseRoot/BRFSS/Data/1_Source/disability.txt" DELIMITER = "|" MISSOVER DSD FIRSTOBS = 2;
	INPUT 	deaf
			blind
			decide
			diffwalk
			diffdres
			diffalon
			id			:$13.	;
	RUN;

*****************************************************************************************
* Step 4: Read in Outcomes Data
*****************************************************************************************;

* Step 4.1 Chronic Health Conditions;
DATA 	Impt.chronic;
	INFILE 	"&CourseRoot/BRFSS/Data/1_Source/chronic.dat" DLMSTR = "::" DSD;
	INPUT 	seqno			
			asthma3
			asthnow
			chccopd3
			diabetes4
			cvdcrhd4
			cvdinfr4		@@ ;
	RUN;

* Step 4.2: Mental Health;
DATA 	Impt.mentalhealth;
	INFILE 	"&CourseRoot/BRFSS/Data/1_Source/mentalhealth.csv" MISSOVER DSD FIRSTOBS=2;
	INPUT 	addepev3			
			lsatisfy
			emtsuprt
			sdhisoft
			id				:$13.	;
	RUN;

* Step 4.3 Cancer;
PROC IMPORT
		DATAFILE 	= "&CourseRoot/BRFSS/Data/1_Source/cancer.xls"
		OUT 		= Impt.cancer
		DBMS 		= XLS
		REPLACE;
	RUN;

* Step 4.4 Obesity;
DATA 	Impt.obesity;
	INFILE 	"&CourseRoot/BRFSS/Data/1_Source/obesity.txt" DELIMITER = "*" MISSOVER DSD FIRSTOBS = 2;
	INPUT 	seqno		:$13.			
			weight		
			height		;
	RUN;
	
* Step 4.5 COVID;
PROC IMPORT	
		DATAFILE 	= "&CourseRoot/BRFSS/Data/1_Source/covid.xlsx"
		OUT 		= Impt.covid
		DBMS 		= XLSX
		REPLACE;
	RUN;
	
	
*****************************************************************************************
* Step 5: Read in Risk Factors Data
*****************************************************************************************;

* Step 5.1 General Health;
DATA Impt.gen_health;
	INFILE 	"&CourseRoot/BRFSS/Data/1_Source/gen_health.txt";
	INPUT 	seqno			$1-11
			gnhlth			12-14
			menthlth		15-17
			physhlth		18-20
			exerany2		21-23
			sleptim1				;
	RUN;
	
* Step 5.2 Marijuana Use;
PROC IMPORT
	DATAFILE 	=  "&CourseRoot/BRFSS/Data/1_Source/marijuana.xlsx"
	OUT 		= 	Impt.marijuana
	DBMS 		= 	XLSX
	REPLACE;
RUN;

* Step 5.3 Smoking Status;
PROC IMPORT 
	DATAFILE 	= "&CourseRoot/BRFSS/Data/1_Source/smoke.xlsx"
	OUT			= impt.smoke
	DBMS		= XLSX
	REPLACE;
RUN;

* Step 5.4 Alcohol Use;
DATA Impt.alcohol;
	INFILE 	"&CourseRoot/BRFSS/Data/1_Source/alcohol.txt" MISSOVER DSD FIRSTOBS = 2;
	INPUT 	alcday4			
			avedrnk3
			drnk3ge5
			drnkaby6
			maxdrnks
			id			:$13.	;
	RUN;

* Step 5.5 COVID Vaccination;
DATA Impt.covid_vax;
	INFILE 	"&CourseRoot/BRFSS/Data/1_Source/covid_vax.csv" MISSOVER DSD FIRSTOBS = 2;
	INPUT 	covidva1
			covidnu1
			covidfs1
			covidse1
			seqno		;
	RUN;
	
*****************************************************************************************
* Step 6: Read in Reference Table
*****************************************************************************************;
* Import the reference table and save it in import folder
DATA Impt.ref_table;
	SET "&CourseRoot/BRFSS/Documents/ref_table.sas7bdat";
RUN;

* Transpose the data out of wide format into long format (excludes the FORMER LABEL column);
PROC TRANSPOSE DATA = Impt.ref_table
	OUT = Impt.ref_table_long;
	VAR COL1--COL29803;
	DROP _NAME_;
	RUN;

* Process the transposed data (drop out the _NAME_ coulumn);
DATA Impt.ref_table_long;
	SET Impt.ref_table_long;
	DROP _NAME_;
RUN;




QUIT;