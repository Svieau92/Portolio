*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    0.1 - BRFSS Project Part 3 - Create Analysis Data Set                   *
*   PURPOSE:    Create the Analysis Data Set Requested by the Investigator              *
*   AUTHOR:     Sean Vieau	                                                            *
*   CREATED:    2024-10-28                                                              *
*                                                                                       *
*   COURSE:     BIOS 6680 - Data Management Using SAS                                   *
*   DATA USED:  BRFSS                                                                   *
*   SOFTWARE:   SAS (r) Proprietary Software 9.4 (TS1M4)                                *
*   MODIFIED:   DATE        BY  REASON                                                  *
*               ----------  --- ------------------------------------------------------- *
*                																        *
*                                                                                       *
*****************************************************************************************
***********************************************************************************; RUN;

*****************************************************************************************
* Step 1: Create Libraries
*****************************************************************************************;
* Set Courseroot;
%LET CourseRoot = /home/u63376223/sasuser.v94/Data Management Using SAS;

* Create libraries;
LIBNAME Source "&CourseRoot/BRFSS/Data/1_Source";
LIBNAME Impt "&CourseRoot/BRFSS/Data/2_Import";

*****************************************************************************************
* Step 2: Read in Data Sets;
*****************************************************************************************;

***** Step 2.1: Read in Reference Table *****:

* Import the reference table;
TITLE "Reference Table";
DATA Impt.ref_table;
    SET "&CourseRoot/BRFSS/Data/1_Source/ref_table (1).sas7bdat";
RUN;

* Transpose it to wideform;
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
	
* Check contents to ensure correct data types;
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.ref_table_long;
	RUN;
	
* Visualize data set;
PROC PRINT DATA = Impt.ref_table_long (OBS = 10);
	RUN;
	
* Dispcode should be numeric, but all values are also missing. 
  I believe this is intentional so we do not need to address.
  All other variables look correct;

***** Step 2.2: Read in Demographics ***** ;

* Import demographics data set;
TITLE "Demographics";
PROC IMPORT 
        DATAFILE =  "&CourseRoot/BRFSS/Data/1_Source/demographics (2).xlsx"
        OUT     =   Impt.Demographics
        DBMS    =   XLSX
        REPLACE;
RUN;

PROC PRINT DATA = Impt.Demographics (OBS = 10);
  RUN;
  
* Check contents to ensure correct data types;
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.DEMOGRAPHICS;
	RUN;
	
* Visualize data set;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	RUN;
	
* Demographics data set checks out;

***** Step 2.3: Read in Outcomes data *****;

* Import the dataset;
TITLE "Outcomes";
PROC IMPORT DATAFILE="&CourseRoot/BRFSS/Data/1_Source/outcomes (1).csv"
            OUT=Impt.Outcomes
            DBMS=CSV
            REPLACE;
     GETNAMES=YES;
     GUESSINGROWS=MAX; /* Ensures that SAS scans more rows to determine the correct data types */
     RUN;

* Check contents to ensure correct data types;
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.Outcomes;
	RUN;

* Note:
* cncrdiff, cncrage, cncrtyp2 should all be numeric with a length of 8. 
* They are characters of length $1 however.

* Fix erroneous character formats;
/* DATA Impt.Outcomes; */
/*     SET Impt.Outcomes; */
/*     ARRAY char_vars[3] $ cncrdiff cncrage cncrtyp2; */
/*     ARRAY num_vars[3] cncrdiff_num cncrage_num cncrtyp2_num; */
/*      */
/*     DO i = 1 TO 3; */
/*     	num_vars[i] = INPUT(char_vars[i], BEST12.); */
/*     END; */
/*  */
/*     DROP cncrdiff cncrage cncrtyp2 i; */
/*     RENAME cncrdiff_num = cncrdiff cncrage_num = cncrage cncrtyp2_num = cncrtyp2; */
/* RUN; */

/* * Apply format; */
/* DATA Impt.Outcomes; */
/* 	SET Impt.Outcomes; */
/* 	FORMAT cncrdiff cncrage cncrtyp2 BEST12.; */
/* 	RUN; */

/* * Double check data format; */
/* ODS SELECT VARIABLES; */
/* PROC CONTENTS DATA = Impt.Outcomes; */
/* 	RUN; */
	
* Visualize data set;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	RUN;
	
***** Step 2.4: Read in Risk Factors data *****;

* Import risk factors;
TITLE "Risk Factors";
PROC IMPORT 
        DATAFILE =  "&CourseRoot/BRFSS/Data/1_Source/riskfactors (1).xlsx"
        OUT     =   Impt.Risk
        DBMS    =   XLSX
        REPLACE ;
    RUN;
    
* Check contents to ensure correct data types;
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.Risk;
	RUN;
	
** Risk Factors checks out.;

* Visualize data set;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	RUN;
    
***********************************************************************
* Step 3: Create Demographics Data Set
***********************************************************************;   

***** Step 3.1 Race *****;	

* Create FORMAT for race;
TITLE "Demographics: Race";
PROC FORMAT;
	VALUE RaceCd
			1 = "Non-Hispanic White"
			2 = "Non-Hispanic Black"
			3 = "Non-Hispanic American Indian or Alaskan Native"
			4 = "Hispanic"
			5 = "Other";
	RUN;
	
* Use boolean logic to create RaceCd variable per PI specifications;
DATA Impt.Demographics;
	SET Impt.Demographics;
		IF _PRACE2 = . 						 THEN RaceCd = .;
		ELSE IF _PRACE2 = 1 AND _HISPANC = 2 THEN RaceCd = 1;
		ELSE IF _PRACE2 = 2 AND _HISPANC = 2 THEN RaceCd = 2;
		ELSE IF _PRACE2 = 3 AND _HISPANC = 2 THEN RaceCd = 3;
		ELSE IF				    _HISPANC = 1 THEN RaceCd = 4;
		ELSE 									  RaceCd = 5;
	ATTRIB RaceCd LABEL = "Race" FORMAT = RaceCd.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	VAR _PRACE2 _HISPANC RaceCd;
	FORMAT RaceCd RaceCd.;
	RUN;
	
***** Step 3.2 Insurance *****;

* Create FORMAT for insurance;
TITLE "Demographics: Insurance";
PROC FORMAT;
	VALUE InsuranceCd
			1 = "Private/commercial/Medicare/Medigap"
			2 = "Medicaid/CHIP"
			3 = "Tricare"
			4 = "Indian Health Services"
			5 = "Other"
			6 = "None/Uninsured/Unknown";
	RUN;
	
* Create Insurance variable per PI specifications;
DATA Impt.Demographics;
	SET Impt.Demographics;
		IF 		PRIMINSR in (1, 2, 3, 4) THEN InsuranceCd = 1;
		ELSE IF PRIMINSR in (5,6)		 THEN InsuranceCd = 2;
		ELSE IF PRIMINSR = 7 			 THEN InsuranceCd = 3;
		ELSE IF PRIMINSR = 8			 THEN InsuranceCd = 4;
		ELSE IF PRIMINSR in (9,10)		 THEN InsuranceCd = 5;
		ELSE 								  InsuranceCd = 6;
	ATTRIB InsuranceCd LABEL = "Insurance" FORMAT = InsuranceCd.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	VAR PRIMINSR InsuranceCd;
	FORMAT InsuranceCd InsuranceCd.;
	RUN;
	
***** Step 3.3: Education *****;

* Create FORMAT for Education;
TITLE "Demographics: Education";
PROC FORMAT;
	VALUE EducationCd
			1 = "<High School"
			2 = "Graduated High School"
			3 = "Some College"
			4 = "4+ Year College Degree";
	RUN;

* Create Education variable per PI specifications;
DATA Impt.Demographics;
	SET Impt.Demographics;
		IF 		EDUCA IN(.,9) 	THEN EducationCd = .;
		ELSE IF EDUCA IN(1,2,3) THEN EducationCd = 1;
		ELSE IF EDUCA = 4		THEN EducationCd = 2;
		ELSE IF EDUCA = 5		THEN EducationCd = 3;
		ELSE 						 EducationCd = 4;
	ATTRIB EducationCd LABEL = "Education" FORMAT = EducationCd.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	VAR EDUCA EducationCd;
	FORMAT InsuranceCd InsuranceCd.;
	RUN;	
	
***** Step 3.4: Age *****;

* Create FORMAT for age;
TITLE "Demographics: Age";
PROC FORMAT;
	VALUE AgeCd
		1 = "18-44"
		2 = "45-64"
		3 = "65+";
	RUN;

* Create Age variable per PI specifications;	
DATA Impt.Demographics;
	SET Impt.Demographics;
		IF 		_AGEG5YR IN(.,14)	   THEN AgeCd = .;
		ELSE IF _AGEG5YR IN(1,2,3,4,5) THEN AgeCd = 1;
		ELSE IF _AGEG5YR IN(6,7,8,9)	   THEN AgeCd = 2;
		ELSE 								AgeCd = 3;
	ATTRIB AgeCd LABEL = "Age" FORMAT = AgeCd.;
	RUN;

* Double check for correct assignment;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	VAR _AGEG5YR AgeCd;
	FORMAT AgeCd AgeCd.;
	RUN;	
	
***** Step 3.5: Income *****;

* Create format for Income;
TITLE "Demographics: Income";
PROC FORMAT;
	VALUE IncomeCd
		1 = "<15k"
		2 = "15-<35k"
		3 = "35-<50K"
		4 = "50k-<100k"
		5 = "100k-<200k"
		6 = "200K+";
	RUN;
	
* Create Income variable per PI specifications;
DATA Impt.Demographics;
	SET Impt.Demographics;
		IF 		INCOME3 IN(., 77, 99) THEN IncomeCd = .;
		ELSE IF	INCOME3 IN(1,2)		  THEN IncomeCd = 1;
		ELSE IF INCOME3 IN(3,4)		  THEN IncomeCd = 2;
		ELSE IF INCOME3 IN(5,6)		  THEN IncomeCd = 3;
		ELSE IF INCOME3 IN(7,8) 	  THEN IncomeCd = 4;
		ELSE IF INCOME3 IN(9,10)	  THEN IncomeCd = 5;
		ELSE						  	   IncomeCd = 6;
	ATTRIB IncomeCd LABEL = "Income" FORMAT = IncomeCd.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	VAR INCOME3 IncomeCd;
	FORMAT IncomeCd IncomeCd.;
	RUN;	

***** Step 3.6: Total Disabilities *****;

* Create Disabilities variable per PI specifications;
TITLE "Demographics: Disabilities";
DATA Impt.Demographics;
	SET Impt.Demographics;
	Disabilities = SUM((DEAF = 1), (BLIND = 1), (DECIDE = 1), (DIFFWALK = 1), (DIFFDRES = 1), (DIFFALON = 1));
	ATTRIB Disabilities LABEL = "Total Number of Disabilities" FORMAT = BEST.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Demographics (OBS = 10);
	VAR DEAF BLIND DECIDE DIFFWALK DIFFDRES DIFFALON Disabilities;
	RUN;	
	
***** Step 3.6: Consolidate Data Set *****:
* Here we will collect only the variables we created which the
  investigator asked for, and the variables needed for the final merge;
TITLE "Demographics Cleaned";
DATA Impt.Demographics_Final;
	SET Impt.Demographics;
	KEEP seqno RaceCd InsuranceCd EducationCd AgeCd IncomeCd Disabilities;
	RENAME  RaceCd = Race 
			InsuranceCd = Insurance
			EducationCd = Education
			AgeCd = Age
			IncomeCd = Income;
	RUN;

* Examine data set;
PROC PRINT DATA = Impt.DEMOGRAPHICS_FINAL (OBS = 10);
	RUN;

* Double check for format;
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.Demographics_Final;
	RUN;
	
***********************************************************************
* Step 4: Create Outcomes Data Set
***********************************************************************; 
  
***** Step 4.1: CHDattack *****:
* Create format for ever had a CHD or heart attack;
TITLE "Outcomes: CHD or Heart Attack";
PROC FORMAT;
	VALUE CHDattackCd
		1 = "Yes"
		2 = "No"
		3 = "Not Asked or Missing";
	RUN;
	
* Create CHDattack variable per PI specifications;
DATA Impt.Outcomes;
	SET Impt.Outcomes; 
	IF 		CVDCRHD4 = 1 OR CVDINFR4 = 1 THEN CHDattackCd = 1;
	ELSE IF CVDCRHD4 = 2 AND CVDINFR4 = 2 THEN CHDattackCd = 2;
	ELSE 										CHDattackCd = 3;
	ATTRIB CHDattackCd LABEL = "CHD or Heart Attack" FORMAT = CHDattackCd.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	VAR CVDCRHD4 CVDINFR4 CHDattackCd;
	RUN;	
	
***** Step 4.2: Asthma *****:

* Create format for asthma status;
TITLE "Outcomes: Asthma";
PROC FORMAT;
	VALUE AsthmaCd
		1 = "Current" 
		2 = "Former"
		3 = "Never"
		4 = "Don’t Know/Not Sure";
	RUN;
	
* Create Disabilities variable per PI specifications;
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		IF		 ASTHMA3 = 1 AND ASTHNOW = 1 THEN AsthmaCd = 1;
		ELSE IF  ASTHMA3 = 1 AND ASTHNOW = 2 THEN AsthmaCd = 2;
		ELSE IF  ASTHMA3 = 2				 THEN AsthmaCd = 3;
		ELSE									  AsthmaCd = 4;
		ATTRIB AsthmaCd LABEL = "Asthma Status" FORMAT = AsthmaCd.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	VAR ASTHMA3 ASTHNOW AsthmaCd;
	RUN;	

***** Step 4.3: BMI *****;

* Create BMI variable per PI specifications;

* Some weights are in lbs and some are in kg, we will have to address;
* Convert all weights in kg into lbs;
TITLE "Outcomes: BMI";
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		WEIGHT2_char = TRIM(PUT(WEIGHT2, 4.));  *convert HEIGHT2 into a string so I can find if it starts with 9 (i.e. is in metric);
		IF WEIGHT2_char ^= "9999" AND SUBSTR(WEIGHT2_char,1,1) = "9"
		 THEN DO WEIGHT2 = INPUT(SUBSTR(WEIGHT2_char,2), BEST12.)*2.20462; *Select the weight in kg and convert to lbs;
			END;
		DROP WEIGHT2_char;
	RUN;
	
* Convert all heights in cm to feet and inches;
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		IF HEIGHT3 ^= 7777 AND HEIGHT3 ^= 9999
			THEN DO 
				HEIGHT3_char = TRIM(PUT(HEIGHT3, 3.)); 
				feet = INPUT(SUBSTR(HEIGHT3_char, 1, 1), 8.); *Select the first digit as the number of feet;
	  			inches = INPUT(SUBSTR(HEIGHT3_char, 2, 2), 8.); * Select digits 2 and 3 as the number of inches;
	  			HEIGHT_inches = (feet*12) + inches; * Convert feet and inches into just inches;

  				END;
  		ELSE IF HEIGHT3 ^= 7777 AND HEIGHT3 ^= 9999
  			THEN DO 
  				HEIGHT3_char = TRIM(PUT(HEIGHT3, 4.)); *convert HEIGHT3 into a string so I can find if it starts with 9 (i.e. is in metric);
  				HEIGHT_inches = INPUT(SUBSTR(WEIGHT2_char,2), BEST12.)*0.393701; * convert centimeters into inches;
  				END;
  		DROP feet inches HEIGHT3_char;
  	RUN;
  	
* Calculate BMI;
DATA Impt.Outcomes;
	SET Impt.Outcomes;
	IF 		WEIGHT2 IN(7777,9999) OR HEIGHT3 IN(7777,9999) THEN BMI = .;
	ELSE 														BMI = (WEIGHT2 / (HEIGHT_inches**2)) * 730;
	ATTRIB BMI LABEL = "Patient BMI";
RUN;

* Double check for correct assignment;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	VAR WEIGHT2 HEIGHT3 BMI;
	RUN;	

* For good measure, let's plot a histogram which should be normally distributed if we did that all correctly;
PROC SGPLOT DATA = Impt.Outcomes;
	HISTOGRAM BMI;
	RUN;
	
* BMI looks good;

***** Step 4.4: BMI Category *****;

* Create format for BMI category;
TITLE "Outcomes: BMI Category";
PROC FORMAT;
	VALUE BMIcat
		1= "Underweight"
		2 = "Normal Weight"
		3 = "Overweight"
		4 = "Obese";
	RUN;

* Create BMIcat per PI specifications;
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		IF BMI = . 		   THEN BMIcat = .; 		
		ELSE IF BMI < 18.5 THEN BMIcat = 1;
		ELSE IF	18.5 <= BMI < 25.0 THEN BMIcat = 2;
		ELSE IF 25.0 <= BMI < 30.0 THEN BMIcat = 3;
		ELSE 							BMIcat = 4;
	ATTRIB BMICat LABEL = "BMI Category" FORMAT = BMIcat.;
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	VAR BMI BMIcat;
	RUN;	

***** Step 4.5: Covid *****;	

* Create format for covid status;
PROC FORMAT;
	VALUE CovidCd
		1 = "Yes"
		2 = "No"
		3 = "Unknown/Refused/Missing";
	RUN;
	
* Create Covid status per PI specifications;
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		IF 		COVIDPOS IN(1,3) THEN CovidCd = 1;
		ELSE IF COVIDPOS = 2 THEN CovidCd = 2;
		ELSE					  CovidCd = 3;
	ATTRIB CovidCd LABEL = "Ever had Covid" FORMAT = CovidCd.;
	RUN;

	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	VAR COVIDPOS CovidCd;
	RUN;	

***** Step 4.6: Cancer *****;

* Create format for cancer status;
PROC FORMAT;
	VALUE CancerCd
		1 = "Yes"
		2 = "No";
	RUN;
	
* Create BMIcat per PI specifications;	
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		IF 		CNCRDIFF = . THEN CancerCd = 2;
		ELSE IF CNCRDIFF IN(1,2,3,7) THEN CancerCd = 1;
	ATTRIB CancerCd LABEL = "Ever had Cancer" FORMAT = Cancercd.;
	RUN;

* Double check for correct assignment;
PROC PRINT DATA = Impt.Outcomes (OBS = 10);
	VAR CNCRDIFF Cancercd;
	RUN;	

***** Step 4.7: Age at First Cancer Diagnosis *****;

* Create format for cancer age;
TITLE "Outcomes: Cancer Age";
PROC FORMAT;
	VALUE CancerAge
		1 = "0-<15"
		2 = "15-<18"
		3 = "18-<39"
		4 = "40+";
	RUN;
	
* Create cancer age per PI specifications;	
DATA Impt.Outcomes;
	SET Impt.Outcomes;
		IF		CNCRAGE IN(., 98, 99) THEN CancerAge = .;
		ELSE IF 0 < CNCRAGE < 15 THEN CancerAge = 1;
		ELSE IF 15 <= CNCRAGE < 18 THEN CancerAge = 2;
		ELSE IF 18 <= CNCRAGE < 39 THEN CancerAge = 3;
		ELSE							CancerAge = 4;
	ATTRIB CancerAge LABEL = "Age at first cancer diagnosis" FORMAT = CancerAge.;
	RUN;

* Double check for correct assignment;
PROC SORT DATA = Impt.Outcomes OUT = Impt.Sorted_Data;
	BY DESCENDING CancerAge;
	RUN;
	
PROC PRINT DATA = Impt.Sorted_Data (OBS = 10);
	VAR CNCRAGE CancerAge;
	RUN;	
	
***** Step 4.7: Consolidate Data Set *****:
* Here we will collect only the variables we created which the
  investigator asked for, and the variables needed for the final merge;
TITLE "Outcomes: Final Data Set";
DATA Impt.Outcomes_Final;
	SET Impt.Outcomes;
	KEEP SEQNO CHDattackCd AsthmaCd BMI BMIcat CovidCd CancerCd CancerAge;
	RENAME  CHDattackCd = CHDattack
		 	AsthmaCd = Asthma
		 	CovidCd = Covid
		 	CancerCd = Cancer;
	RUN;
	
* Print head;
PROC PRINT DATA = Impt.Outcomes_Final (OBS = 10);
	RUN;
	
* Double check formats;
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.Outcomes_Final;
	RUN;
	
***********************************************************************
* Step 5: Create Risk Factors Data Set
***********************************************************************;

***** Step 5.1: Physical Health Not Good*****;

* Create physical health variable per PI specifications;
TITLE "Risk Factors: Physical Health Not Good";
DATA Impt.Risk;
	SET Impt.Risk;
	IF 		PHYSHLTH IN(.,77,99) THEN PhysHealth = .;
	ELSE IF PHYSHLTH = 88 		 THEN PhysHealth  = 0;
	ELSE 				       		  PhysHealth = (PHYSHLTH /30) * 100;
	LABEL PhysHealth = "Percent of 30 days with physical health not good";
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	VAR PHYSHLTH PhysHealth;
	RUN;
	
***** Step 5.2: Mental Health Not Good*****;

* Create mental health variable per PI specifications;
TITLE "Risk Factors: Mental Health Not Good";
DATA Impt.Risk;
	SET Impt.Risk;
	IF 		MENTHLTH IN(.,77,99) THEN MentHealth = .;
	ELSE IF MENTHLTH = 88 		 THEN MentHealth  = 0;
	ELSE 				       		  MentHealth = (MENTHLTH /30) * 100;
	LABEL MentHealth = "Percent of 30 days with mental health not good";
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	VAR MENTHLTH MentHealth;
	RUN;
	
***** Step 5.3: Smoking Status*****;

* Create format for smoking status;
TITLE "Risk Factors: Smoking Status";
PROC FORMAT;
	VALUE SmokeStat
		1 = "Current Smoker"
		2 = "Former Smoker"
		3 = "Never Smoker"
		4 = "Don't Know/Refused/Missing";
	RUN;

* Create smoking status variable per PI specifications;
DATA Impt.Risk;
	SET Impt.Risk;
		IF		 SMOKE100 = 1 AND SMOKDAY2 IN(1,2) THEN SmokeStat = 1;
		ELSE IF	 SMOKE100 = 1 AND SMOKDAY2 = 3	   THEN SmokeStat = 2;
		ELSE IF  SMOKE100 = 2					   THEN SmokeStat = 3;
		ELSE 									  	    SmokeStat = 4;
		ATTRIB SmokeStat LABEL = "Smoking Status" FORMAT = SmokeStat.;
	RUN;

* Double check for correct assignment;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	VAR SMOKE100 SMOKDAY2 SmokeStat;
	RUN;
	
***** Step 5.4: Years Smoked*****;

* Create smoking status variable per PI specifications;
DATA Impt.Risk;
	SET Impt.Risk;
	
		IF 		SmokeStat = 4 THEN SmokeTime = .;
		ELSE IF SmokeStat = 3 THEN SmokeTime = 0;
		ELSE IF SmokeStat IN(1,2) AND LCSFIRST NOT IN(777,999) AND LCSLAST NOT IN(777,999)
			THEN SmokeTime = LCSLAST - LCSFIRST;
		ELSE 						SmokeTime = .;
	ATTRIB SmokeTime LABEL = "How many years smoked";
	RUN;
	
* Double check for correct assignment;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	VAR SmokeStat LCSFIRST LCSLAST SmokeTime;
	RUN;
	
* Note: there is a current smoker with 999 for last cigarrete smoked;
* We will assume these are patients that did not wish to report their age.;
	
***** Step 5.5: Drank Alcohol in Past 30 Days *****;

* Create format for smoking status;
TITLE "Risk Factors: Alcohol";
PROC FORMAT;
	VALUE Alcohol
		1 = "Yes"
		2 = "No";
	RUN;

* Create alcohol consumption variable per PI specifications;
DATA Impt.Risk;
	SET Impt.Risk;
		IF  	ALCDAY4 IN(., 777,999) THEN Alcohol = .;
		ELSE IF ALCDAY4 = 888 THEN Alcohol = 2;
		ELSE					   Alcohol = 1;
	ATTRIB Alcohol LABEL = "Consumed alcohol in past 30 days" FORMAT = Alcohol.;
	RUN;

* Double check for correct assignment;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	VAR ALCDAY4 Alcohol;
	RUN;

***** Step 5.6: Months Between Covid Vaccines *****;

* Create covid vaccine variable per PI specifications;
DATA Impt.Risk;
  SET Impt.Risk;
  * Convert dates to a character string;
  firstvax_char = TRIM(LEFT(PUT(COVIDFS1, 6.)));
  secondvax_char = TRIM(LEFT(PUT(COVIDSE1, 6.)));
  
  * Extract Month and Year for firstvax_char;
  IF LENGTH(firstvax_char) = 5 THEN DO;
    month1 = INPUT(SUBSTR(firstvax_char, 1, 1), BEST12.);
    year1 = INPUT(SUBSTR(firstvax_char, 2), BEST12.);
  END;
  ELSE IF LENGTH(firstvax_char) = 6 THEN DO;
    month1 = INPUT(SUBSTR(firstvax_char, 1, 2), BEST12.);
    year1 = INPUT(SUBSTR(firstvax_char, 3), BEST12.);
  END;

  * Extract Month and Year for secondvax_char;
  IF LENGTH(secondvax_char) = 5 THEN DO;
    month2 = INPUT(SUBSTR(secondvax_char, 1, 1), BEST12.);
    year2 = INPUT(SUBSTR(secondvax_char, 2), BEST12.);
  END;
  ELSE IF LENGTH(secondvax_char) = 6 THEN DO;
    month2 = INPUT(SUBSTR(secondvax_char, 1, 2), BEST12.);
    year2 = INPUT(SUBSTR(secondvax_char, 3), BEST12.);
  END;

	* Calculate number of months inbetween;
	IF SUBSTR(firstvax_char, 1, 2) NOT IN("77" "99") 
	AND SUBSTR(Secondvax_char,1, 2) NOT IN ("77", "99")
	THEN DO
  date1 = MDY(month1, 1, year1);
  date2 = MDY(month2, 1, year2);
  
  * Calculate the difference in months;
  CovidVaxMonths = INTCK('MONTH', date1, date2);
  ATTRIB CovidVaxMonths Label = "Number of months between first and second covid vaccination";
  END;
  
  RUN;
	
* Double check calculations are correct;
PROC SORT DATA=Impt.Risk OUT=Impt.SortedRisk;
  BY DESCENDING CovidVaxMonths;
RUN;
PROC PRINT DATA = Impt.SortedRisk (OBS = 10);
	VAR month1 year1 month2 year2 CovidVaxMonths;
	RUN;

***** Step 5.7: Marijuana Percent *****;
* Create Marijuana percent variable per PI specifications;
TITLE "Risk Factors: Marijuana Percentage";
DATA Impt.Risk;
	SET Impt.Risk;
	IF 		MARIJAN1 IN(.,77,99) THEN MarijuanaPerc = .;
	ELSE IF MARIJAN1 = 88 		 THEN MarijuanaPerc  = 0;
	ELSE 				       		  MarijuanaPerc = (MARIJAN1 /30) * 100;
	LABEL MarijuanaPerc = "Percent of 30 days with marijuana use";
	RUN;
	
* Double check for correct assignment;
PROC SORT DATA=Impt.Risk OUT=Impt.SortedRisk;
  BY DESCENDING MarijuanaPerc;
RUN;
PROC PRINT DATA = Impt.SortedRisk (OBS = 10);
	VAR MARIJAN1 MarijuanaPerc;
	RUN;
	
***** Step 5.8: Marijuana Use *****;
* Create format for Marijuana Use;
TITLE "Risk Factors: Marijuana Use";
PROC FORMAT;
	VALUE Marijuana
		1 = "Yes"
		2 = "No";
	RUN;

* Create marijuana use variable per PI specifications;
DATA Impt.Risk;
	SET Impt.Risk;
		IF  	MARIJAN1 IN(., 77,99) THEN Marijuana = .;
		ELSE IF MARIJAN1 = 88 THEN Marijuana = 2;
		ELSE					   Marijuana = 1;
	ATTRIB Marijuana LABEL = "Marijuana use in past 30 days" FORMAT = Marijuana.;
	RUN;

* Double check for correct assignment;
PROC SORT DATA=Impt.Risk OUT=Impt.SortedRisk;
  BY DESCENDING Marijuana;
RUN;
PROC PRINT DATA = Impt.SortedRisk (OBS = 10);
	VAR MARIJAN1 Marijuana;
	RUN;
	
***** Step 5.9: Number of Packs Per Day *****;
TITLE "Risk Factors: Number of Cigarette Packs per Day";
DATA Impt.Risk;
	SET Impt.Risk;
		IF		LCSNUMCG IN(777,999) THEN SmokeNum = .;
		ELSE IF LCSNUMCG = . 		 THEN SmokeNum = .;
		ELSE IF LCSNUMCG = 0		 THEN SmokeNum = 0;
		ELSE 							  SmokeNum =  LCSNUMCG/20;
	ATTRIB SmokeNum LABEL = "Numer of Cigarette Packs Per Day";
	RUN;

* Double check for correct assignment;
PROC PRINT DATA = Impt.Risk (OBS = 10);
	VAR LCSNUMCG SmokeNum;
	RUN;

***** Step 5.10: Consolidate Data Set *****;
* Here we will collect only the variables we created which the
  investigator asked for, and the variables needed for the final merge;
TITLE "Risk Factors: Final Data Set";
DATA Impt.Risk_Final;
	SET Impt.Risk;
	KEEP id PhysHealth MentHealth SmokeStat SmokeNum SmokeTime Alcohol CovidVaxMonths MarijuanaPerc Marijuana;
	RUN;
	
* Print head;
PROC PRINT DATA = Impt.Risk_Final (OBS = 10);
	RUN;
	
* Double check formats;
TITLE "Risk Factors: Check Formats";
ODS SELECT VARIABLES;
PROC CONTENTS DATA = Impt.Risk_Final;
	RUN;
	
***********************************************************************
* Step 6: Create Date Variable in Reference Table
***********************************************************************;
PROC FORMAT;
	VALUE $stateCd
	6 = "California"
	8 = "Colorado"
	9 = "Connecticut";
	RUN;
		
* Create and Format date variable;
DATA Impt.Ref_Final;
	SET Impt.ref_table_long;
	state = strip(state);
	IntvDate = MDY(IMONTH, IDAY, IYEAR);
	ATTRIB IntvDate LABEL = "Interview Date" FORMAT = MMDDYY10.;
	ATTRIB state LABEL = "State" FORMAT = $StateCd.;
	KEEP id SEQNO IntvDate state;
	RUN;
	
* Double check by printing;
PROC PRINT DATA = Impt.Ref_Final (OBS = 10);
	FORMAT state $StateCd.;
	RUN;
	
***********************************************************************
* Step 7: Merge Data Sets
***********************************************************************;

* Convert SEQNO in the reference table to numeric and SORT;
TITLE "Final Data Set";
DATA Impt.Ref_Final;
	SET Impt.Ref_Final;
	SEQNO_num = INPUT(SEQNO, BEST12.);
	FORMAT SEQNO_num BEST12.;
	DROP SEQNO;
	RENAME SEQNO_NUM = SEQNO;
	RUN;

* SEQNO in the demographics Final data set is a character variable
* with '-'s in it. We have to clean those out and make it numeric to 
* merge it using the reference table;
DATA Impt.Demographics_Final;
	SET Impt.Demographics_Final;
	SEQNO = COMPRESS(SEQNO, "-");
	SEQNO_num = INPUT(SEQNO, BEST12.);
	FORMAT SEQNO_num BEST12.;
	DROP SEQNO;
	RENAME SEQNO_num = SEQNO;
	RUN;
	
* Sort Demographics_Final by SEQNO;
PROC SORT DATA = Impt.Demographics_Final;
	BY SEQNO;
	RUN;

* Sort Outcomes_Final by SEQNO;
PROC SORT DATA = Impt.Outcomes_Final;
	BY SEQNO;
	RUN;	

* Sort Ref_Final by SEQNO;
PROC SORT DATA = Impt.Ref_Final;
	BY SEQNO;
	RUN;
	
* First merge by SEQNO;
DATA Impt.BRFSS_Final;
	MERGE Impt.Demographics_Final (IN = a)
		  Impt.Ref_Final (IN = b)
		  Impt.Outcomes_Final (IN = c);
	BY SEQNO;
	IF a AND b AND c;
	RUN;
	
* Sort by ID;
PROC SORT DATA = Impt.BRFSS_Final;
	BY id;
	RUN;
	
* Sort by ID;
PROC SORT DATA = impt.Risk_Final;
	BY id;
	RUN;
	
* Then merge by Id;
DATA Impt.BRFSS_Final;
	MERGE Impt.BRFSS_Final (IN = a)
		  Impt.Risk_Final (IN = b);	
	BY id;
	IF a AND b;
	RUN;

* Print top 10 observations;
PROC PRINT DATA = Impt.BRFSS_Final (OBS = 10);
	RUN;
	
* Check variable types;
PROC CONTENTS DATA = Impt.BRFSS_Final;
	RUN;
	
/* * Use PROC SQL to merge all data sets; */
/* PROC SQL NUMBER; */
/*     CREATE TABLE Impt.BRFSS_Final AS */
/*     SELECT */
/*         Demo.*,  */
/*         Outcomes.*,  */
/*         REF.*,  */
/*         RISK.*,  */
/*         Demo.SEQNO AS SEQNO_DEMO,  */
/*         Outcomes.SEQNO AS SEQNO_OUTCOMES,  */
/*         REF.SEQNO AS SEQNO_REF,  */
/*         RISK.id AS id_RISK */
/*     FROM  */
/*         Impt.Ref_Final AS REF */
/*     INNER JOIN  */
/*         Impt.Outcomes_Final AS Outcomes */
/*         ON REF.SEQNO = Outcomes.SEQNO */
/*     INNER JOIN  */
/*         Impt.Demographics_Final AS DEMO */
/*         ON Demo.SEQNO = REF.SEQNO */
/*     INNER JOIN */
/*         Impt.Risk_Final AS RISK */
/*         ON Ref.id = RISK.id */
/*     ORDER BY REF.id; */
/* QUIT; */