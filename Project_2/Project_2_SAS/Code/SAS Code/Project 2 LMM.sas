*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    Project 2 - LMM	                                						*
*   PURPOSE:    Recreate Analysis of Project 3 in SAS					                *
*   AUTHOR:     Sean Vieau	                                                            *
*   CREATED:    2024-11-25                                                              *
*                                                                                       *
*   COURSE:     Advanced Data Analysis					                                *
*   DATA USED:  Proj2_data_2_cleaned.csv                                                       *
*   SOFTWARE:   SAS (r) Proprietary Software 9.4 (TS1M4)                                *

*****************************************************************************************
***********************************************************************************; RUN;

*****************************************************************************************
* Step 1: Import Dataset
*****************************************************************************************;

* Create library;
%LET CourseRoot = /home/u63376223/sasuser.v94/Advanced Data Analysis;
LIBNAME Proj2LMM "&CourseRoot/Project 2 LMM";

* Import the dataset;
PROC IMPORT
    DATAFILE = "&CourseRoot/Project 2 LMM/Proj2_data_2_cleaned.csv"
    OUT = Proj2LMM.data
    REPLACE;
    GETNAMES = YES;
    RUN;
    
* Drop the extraneous VAR1 column;
DATA Proj2LMM.data;
	SET Proj2LMM.data;
	DROP VAR1;
	RUN;
    
* Examine data set;
PROC PRINT DATA = Proj2LMM.data (OBS = 6);
    RUN;
    
*****************************************************************************************
* Step 2: Create Variables
*****************************************************************************************;
    
* Fix missing values;
DATA Proj2LMM.data;
    SET Proj2LMM.data;
    if hard_drugs = "NA" then hard_drugs = "";
    	else hard_drugs = hard_drugs;
    if ADH = "NA" then ADH = "";
    	ELSE ADH = ADH;
    IF FRP = "NA" THEN FRP = "";
    	ELSE FRP = FRP;
    RUN;
  
 
* Create hard drugs use group;
PROC SQL;
    CREATE TABLE data_2_grouped AS
    SELECT newid,
           hard_drugs,
           years,
           CASE
               WHEN max(CASE WHEN hard_drugs = 'Yes' AND years = 2 THEN 1 ELSE 0 END) > 0 THEN 'Current User'
               WHEN max(CASE WHEN hard_drugs = 'Yes' AND (years = 0 OR years = 1) THEN 1 ELSE 0 END) > 0 AND 
                    max(case when hard_drugs = 'No' AND years = 2 THEN 1 ELSE 0 END) > 0 THEN 'Previous User'
               ELSE 'Never User'
           END AS hard_drugs_grp
    FROM PROJ2LMM.data
    GROUP BY newid
    ORDER BY newid, years;
QUIT;

* Append to original data set;
DATA Proj2LMM.Data;
	MERGE Proj2LMM.Data(IN = A) data_2_grouped(IN = B);
	BY newid;
	IF A;
	RUN;
	
* Double check classification;
* This matches the previous analysis in R;
PROC TABULATE DATA = Proj2LMM.data;
	CLASS hard_drugs_grp;
	TABLE hard_drugs_grp;
	RUN;
	
* Create adherence group;
PROC SQL;
    CREATE TABLE data_2_grouped AS
    SELECT newid,
           ADH,
           years,
           CASE
               WHEN max(case when years = 2 AND (ADH = '100%' or ADH = '95-99%') THEN 1 ELSE 0 END) = 1 THEN 'High Adherence' 
               ELSE 'Low Adherence'
           END AS ADH_HIGHVSLOW
    FROM PROJ2LMM.data
    GROUP BY newid
    ORDER BY newid, years;
QUIT;

* Append to original data set;
DATA Proj2LMM.Data;
	MERGE Proj2LMM.Data(IN = A) data_2_grouped(IN = B);
	BY newid;
	IF A;
	RUN;
	
* Double check classification;
* This matches the previous analysis in R;
PROC TABULATE DATA = Proj2LMM.data;
	CLASS ADH_HIGHVSLOW;
	TABLE ADH_HIGHVSLOW;
	RUN;
	
	
* Create college level variable;
PROC SQL;
    CREATE TABLE data_2_grouped AS
    SELECT newid,
           EDUCBAS,
           years,
           CASE
               WHEN max(case when years = 2 AND (EDUCBAS IN ("At least one year college but no degree",
                                                             "Four years college or got degree",
                                                             "Some graduate work",
                                                             "Post-graduate degree"))
                                   THEN 1 ELSE 0 END) = 1 
               THEN 'College' 
               ELSE 'No College'
           END AS EDUC_COLLEGE
    FROM PROJ2LMM.data
    GROUP BY newid
    ORDER BY newid, years;
QUIT;

* Append to original data set;
DATA Proj2LMM.Data;
	MERGE Proj2LMM.Data(IN = A) data_2_grouped(IN = B);
	BY newid;
	IF A;
	RUN;
	
* Double check classification;
* This matches the previous analysis in R;
PROC TABULATE DATA = Proj2LMM.data;
	CLASS EDUC_COLLEGE;
	TABLE EDUC_COLLEGE;
	RUN;
	
	
	
*****************************************************************************************
* Step 3: Data Visualization
*****************************************************************************************;


****** Viral Load

*** By hard drug use and year;
PROC SQL;
	CREATE TABLE summary_data as
	SELECT hard_drugs_grp,
		   years,
		   mean(VLOAD_log) as avg_VLOAD_log
	FROM Proj2LMM.data
	GROUP BY hard_drugs_grp, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_VLOAD_log / GROUP = hard_drugs_grp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Viral Load";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Viral Load by Year and Hard Drug Use Group";
	RUN;

*** By Adherence;
PROC SQL;
	CREATE TABLE summary_data as
	SELECT ADH_HIGHVSLOW,
		   years,
		   mean(VLOAD_log) as avg_VLOAD_log
	FROM Proj2LMM.data
	GROUP BY ADH_HIGHVSLOW, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_VLOAD_log / GROUP = ADH_HIGHVSLOW;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Viral Load";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Viral Load by Year and Adherence";
	RUN;
	
	
****** CD4+ T Cell Count;

*** By hard drug use;
PROC SQL;
	CREATE TABLE summary_data as
	SELECT hard_drugs_grp,
		   years,
		   mean(LEU3N) as avg_LEU3N
	FROM Proj2LMM.data
	GROUP BY hard_drugs_grp, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_LEU3N / GROUP = hard_drugs_grp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average CD4+ T Cell Count";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average CD4+ T Cell Count by Year and Hard Drug Use Group";
	RUN;
	
*** By adherence;
PROC SQL;
	CREATE TABLE summary_data as
	SELECT ADH_HIGHVSLOW,
		   years,
		   mean(LEU3N) as avg_LEU3N
	FROM Proj2LMM.data
	GROUP BY ADH_HIGHVSLOW, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_LEU3N / GROUP = ADH_HIGHVSLOW;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average CD4+ T Cell Count";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average CD4+ T Cell Count by Year and Adherence";
	RUN;

****** Mental QOL by Hard Drug Use;

* Calculate the average AGG_MENT for each hard_drugs_grp and year;
PROC SQL;
	CREATE TABLE summary_data as
	SELECT hard_drugs_grp,
		   years,
		   mean(AGG_MENT) as avg_AGG_MENT
	FROM Proj2LMM.data
	GROUP BY hard_drugs_grp, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_MENT / GROUP = hard_drugs_grp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Mental QOL";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Mental Quality of Life by Year and Hard Drug Use Group";
	RUN;
	
* Mental QOL by Adherence;
* Calculate the average for each year;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT ADH_HIGHVSLOW,
		   years,
		   mean(AGG_MENT) AS avg_AGG_MENT
	FROM Proj2LMM.data
	WHERE years > 0
	GROUP BY ADH_HIGHVSLOW, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_MENT / GROUP = ADH_HIGHVSLOW;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Mental QOL";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Mental Quality of Life by Year and Adherence";
	RUN;


******* Physical QOL by Hard Drug Use;

* Calculate the average AGG_PHYS for each hard_drugs_grp and year;
PROC SQL;
	CREATE TABLE summary_data as
	SELECT hard_drugs_grp,
		   years,
		   mean(AGG_PHYS) as avg_AGG_PHYS
	FROM Proj2LMM.data
	GROUP BY hard_drugs_grp, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_PHYS / GROUP = hard_drugs_grp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Physical QOL";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Physical Quality of Life by Year and Hard Drug Use Group";
	RUN;
	
* Physical QOL by Adherence;	

* Calculate the average for each year;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT ADH_HIGHVSLOW,
		   years,
		   mean(AGG_PHYS) AS avg_AGG_PHYS
	FROM Proj2LMM.data
	WHERE years > 0
	GROUP BY ADH_HIGHVSLOW, years;
	QUIT;
	
* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_PHYS / GROUP = ADH_HIGHVSLOW;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Physical QOL";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Physical Quality of Life by Year and Adherence";
	RUN;

*****************************************************************************************
* Step 4: Covariates
*****************************************************************************************;

***** Depression

*** By hard drug use;

* Calculate the average for each year;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT hard_drugs_grp,
		   years,
		   mean(CESD) AS avg_CESD
	FROM Proj2LMM.data
	WHERE years > 0
	GROUP BY hard_drugs_grp, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_CESD / GROUP = hard_drugs_grp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Depresion Score";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Depression Score by Year and Hard Drugs Use";
	RUN;

*** By adherence;
* Calculate the average for each year;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT ADH_HIGHVSLOW,
		   years,
		   mean(CESD) AS avg_CESD
	FROM Proj2LMM.data
	WHERE years > 0
	GROUP BY ADH_HIGHVSLOW, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_CESD / GROUP = ADH_HIGHVSLOW;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Depresion Score";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Depression Score by Year and Adherence";
	RUN;
	
***** Frailty Related Phenotype;

*** CD4+ T Cell Count;
* Calculate the average for each year;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT FRP,
		   years,
		   mean(LEU3N) AS avg_LEU3N
	FROM Proj2LMM.data
	GROUP BY FRP, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_LEU3N / GROUP = frp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average CD4+ T Cell Count";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average CD4+ T Cell Count by Year and Frailty Related Phenotype";
	RUN;
	
*** Physical QOL;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT FRP,
		   years,
		   mean(AGG_PHYS) AS avg_AGG_PHYS
	FROM Proj2LMM.data
	GROUP BY FRP, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_PHYS / GROUP = frp;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average CD4+ T Cell Count";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Physical QOL by Year and Frailty Related Phenotype";
	RUN;
	
************* College Education;

*** Viral Load;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT EDUC_COLLEGE,
		   years,
		   mean(VLOAD_log) AS avg_VLOAD_log
	FROM Proj2LMM.data
	GROUP BY EDUC_COLLEGE, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_VLOAD_log / GROUP = EDUC_COLLEGE;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Viral Load";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Viral Load by Year and College Education";
	RUN;

*** CD4+ T Cell Count;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT EDUC_COLLEGE,
		   years,
		   mean(LEU3N) AS avg_LEU3N
	FROM Proj2LMM.data
	GROUP BY EDUC_COLLEGE, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_LEU3N / GROUP = EDUC_COLLEGE;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average CD4+ T Cell Count";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average CD4+ T Cell Count by Year and College Education";
	RUN;
	
*** Mental QOL;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT EDUC_COLLEGE,
		   years,
		   mean(AGG_MENT) AS avg_AGG_MENT
	FROM Proj2LMM.data
	GROUP BY EDUC_COLLEGE, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_MENT / GROUP = EDUC_COLLEGE;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Mental QOL";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Mental QOL by Year and College Education";
	RUN;

	
*** Physical QOL;
PROC SQL;
	CREATE TABLE summary_data AS
	SELECT EDUC_COLLEGE,
		   years,
		   mean(AGG_PHYS) AS avg_AGG_PHYS
	FROM Proj2LMM.data
	GROUP BY EDUC_COLLEGE, years;
	QUIT;

* Plot;
PROC SGPLOT DATA = summary_data;
	SERIES x = years y = avg_AGG_PHYS / GROUP = EDUC_COLLEGE;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Physical QOL";
	KEYLEGEND / POSITION = right ACROSS = 1 LOCATION = outside;
	TITLE "Average Physical QOL by Year and College Education";
	RUN;
	
*****************************************************************************************
* Step 5: Perform the Analyses
*****************************************************************************************;

*** Log Viral Load;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF='Never User') ADH_HIGHVSLOW;
	MODEL VLOAD_log = hard_drugs_grp ADH_HIGHVSLOW years hard_drugs_grp*years / SOLUTION CL;
	RANDOM INTERCEPT / SUBJECT = newid VCORR;
	ODS OUTPUT FitStatistics = Model_full;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;

** The interaction is not significant, take it out.;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF='Never User') ADH_HIGHVSLOW;
	MODEL VLOAD_log = hard_drugs_grp ADH_HIGHVSLOW years / SOLUTION CL;
	RANDOM INTERCEPT / SUBJECT = newid VCORR;
	ODS OUTPUT FitStatistics = Model_Reduced1;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;

** Add Adherence interaction term;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF='Never User') ADH_HIGHVSLOW;
	MODEL VLOAD_log = hard_drugs_grp ADH_HIGHVSLOW years ADH_HIGHVSLOW*years / SOLUTION CL;
	RANDOM INTERCEPT / SUBJECT = newid VCORR;
	ODS OUTPUT FitStatistics = Model_Reduced2;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;
	
** Perform Model Selection Comparing BIC;
DATA COMPARE_BIC;
	SET Model_Full(in=full) Model_Reduced1(in = reduced1) Model_Reduced2(in = reduced2);
	IF full THEN Model = "Model 1";
	ELSE IF reduced1 THEN MODEL = "Model 2";
	ELSE Model = "Model 3";
	
	IF Descr = "BIC (Smaller is Better)";
	
	RUN;
	
* Print BICs for each model;
PROC PRINT DATA = COMPARE_BIC;
	RUN;


***** Perform Analysis;
* Run Final Model;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF='Never User') ADH_HIGHVSLOW;
	MODEL VLOAD_log = hard_drugs_grp ADH_HIGHVSLOW years ADH_HIGHVSLOW*years / SOLUTION CL;
	RANDOM INTERCEPT / SUBJECT = newid VCORR;
	ODS OUTPUT FitStatistics = Model_Reduced2;
	STORE OUT=Model_VLOAD; *Store the model information for later use;
	RUN;
	
* Change reference group;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF='Previous User') ADH_HIGHVSLOW;
	MODEL VLOAD_log = hard_drugs_grp ADH_HIGHVSLOW years ADH_HIGHVSLOW*years / SOLUTION CL;
	RANDOM INTERCEPT / SUBJECT = newid VCORR;
	ODS OUTPUT FitStatistics = Model_Reduced2;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;
	
** Visualize;

* Use PROC PLM to generate the predicted values under the final model;
PROC PLM RESTORE = Model_VLOAD;
	SCORE DATA = Proj2LMM.Data OUT = Proj2LMM.Data PREDICTED = Predicted_VLOAD_log; 
	RUN;

* Visualize the predicted values under the final model;
PROC SGPLOT DATA = Proj2LMM.Data;
	REG X = Years Y = Predicted_VLOAD_log / GROUP = ADH_HIGHVSLOW LINEATTRS = (THICKNESS=2) NOMARKERS CLM;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Average Log Viral Load";
	TITLE "Predicted Log Viral Load by Adherence Level";
	RUN;
	
*************** CD4+ T Cell Count;		
* Run with unstructured covariance matrix;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Never User") ADH_HIGHVSLOW;
	MODEL LEU3N = hard_drugs_grp ADH_HIGHVSLOW years hard_drugs_grp*years / CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Un;
	STORE OUT=Model_LEU3N; *Store the model information for later use;
	RUN;
	
* Change reference Category;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS newid hard_drugs_grp (REF = "Previous User") ADH_HIGHVSLOW;
	MODEL LEU3N = hard_drugs_grp ADH_HIGHVSLOW years hard_drugs_grp*years / CL;
	RANDOM intercept / SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Un;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;
	
** Visualize;

* Use PROC PLM to generate the predicted values under the final model;
PROC PLM RESTORE = Model_LEU3N;
	SCORE DATA = Proj2LMM.Data OUT = Proj2LMM.Data PREDICTED = Predicted_LEU3N;
	RUN;
	
* Visualize the predicted values under the final model;
PROC SGPLOT DATA = Proj2LMM.Data;
	REG X = Years Y = Predicted_LEU3N / GROUP = hard_drugs_grp LINEATTRS = (THICKNESS=2) NOMARKERS;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Predicted CD4+ T Cell Count";
	TITLE "Predicted CD4+ T Cell Count by Hard Drug Use";
	RUN;
	
************** Mental QOL;
* Run with unstructured covariance matrix;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Never User") ADH_HIGHVSLOW;
	MODEL AGG_MENT = hard_drugs_grp ADH_HIGHVSLOW years CESD hard_drugs_grp*years / CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Un;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	STORE OUT=Model_AGG_MENT; *Store the model information for later use;
	RUN;
	
* Run without the interaction term;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Never User") ADH_HIGHVSLOW;
	MODEL AGG_MENT = hard_drugs_grp ADH_HIGHVSLOW years CESD / CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Red_Model;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;
	
** Perform Model Selection Comparing BIC;
DATA COMPARE_BIC;
	SET Full_Model_Un(in=full) Red_Model(in = reduced1);
	IF full THEN Model = "Full Model";
	ELSE IF reduced1 THEN MODEL = "No Interaction";
	
	IF Descr = "BIC (Smaller is Better)";
	
	RUN;
	
* Print BICs;
PROC PRINT DATA = Compare_bic;
	RUN;
	
* Change the reference level;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Previous User") ADH_HIGHVSLOW;
	MODEL AGG_MENT = hard_drugs_grp ADH_HIGHVSLOW years CESD hard_drugs_grp*years / CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Un;
	RUN;
	
** Visualize;

* Use PROC PLM to generate the predicted values under the final model;
PROC PLM RESTORE = Model_AGG_MENT;
	SCORE DATA = Proj2LMM.Data OUT = Proj2LMM.Data PREDICTED = Predicted_AGG_MENT;
	RUN;
	
* Visualize the predicted values under the final model;
PROC SGPLOT DATA = Proj2LMM.Data;
	REG X = Years Y = Predicted_AGG_MENT / GROUP = hard_drugs_grp LINEATTRS = (THICKNESS=2) NOMARKERS;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Predicted Mental QOL";
	TITLE "Predicted Mental QOL by Hard Drug Use";
	RUN;
	
****** Physical QOL;
* Run with unstructured covariance matrix;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Never User") ADH_HIGHVSLOW FRP;
	MODEL AGG_PHYS = hard_drugs_grp ADH_HIGHVSLOW years hard_drugs_grp*years FRP/ CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Phys;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	RUN;
	
* Run with unstructured covariance matrix;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Never User") ADH_HIGHVSLOW FRP;
	MODEL AGG_PHYS = hard_drugs_grp ADH_HIGHVSLOW years hard_drugs_grp*years ADH_HIGHVSLOW*years FRP/ CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Phys2;
	ODS SELECT SolutionF Tests3; * Output only these specific tables;
	STORE OUT=Model_AGG_PHYS; *Store the model information for later use;
	RUN;

** Perform Model Selection Comparing BIC;
DATA COMPARE_BIC;
	SET Full_Model_Phys(in=full) Full_Model_Phys2(in = reduced1);
	IF full THEN Model = "One Interaction";
	ELSE IF reduced1 THEN MODEL = "Two Interactions";
	
	IF Descr = "BIC (Smaller is Better)";
	RUN;
	
* Print BICs;
PROC PRINT DATA = Compare_bic;
	RUN;

* Change reference level;
PROC MIXED DATA = PROJ2LMM.DATA METHOD = REML;
	CLASS hard_drugs_grp (REF = "Previous User") ADH_HIGHVSLOW FRP;
	MODEL AGG_PHYS = hard_drugs_grp ADH_HIGHVSLOW years hard_drugs_grp*years ADH_HIGHVSLOW*years FRP/ CL;
	RANDOM intercept newid/ SUBJECT = newid TYPE = UN VCORR;
	ODS OUTPUT FitStatistics = Full_Model_Phys2;
	RUN;
	
** Visualize;
* Use PROC PLM to generate the predicted values under the final model;
PROC PLM RESTORE = Model_AGG_PHYS;
	SCORE DATA = Proj2LMM.Data OUT = Proj2LMM.Data PREDICTED = Predicted_AGG_PHYS;
	RUN;
	
* Visualize the predicted values under the final model;
PROC SGPLOT DATA = Proj2LMM.Data;
	REG X = Years Y = Predicted_AGG_PHYS / GROUP = hard_drugs_grp LINEATTRS = (THICKNESS=2) NOMARKERS;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Predicted Physical QOL";
	TITLE "Predicted Physical QOL by Hard Drug Use";
	RUN;


***** Final Summary Plots;

**** VLOAD LOG;

* Generate Predicted Values and SE's;
PROC PLM RESTORE = Model_VLOAD_log;
    SCORE DATA = Proj2LMM.Data OUT = Predicted_Data PREDICTED = Predicted_VLOAD_LOG STDERR = Predicted_SE;
RUN;

* Calculate the average predicted values and SE's;
PROC SQL;
    CREATE TABLE Summarized_Data AS
    SELECT Years,
           ADH_HIGHVSLOW,
           mean(Predicted_VLOAD_log) as Avg_Predicted_VLOAD_log,
           mean(Predicted_SE) as Avg_Predicted_SE
    FROM Predicted_Data
    GROUP BY Years, ADH_HIGHVSLOW;
	QUIT;


* Calculate the lower and upper bounds for the standard errors;
DATA Summarized_Data;
    SET Summarized_Data;
    LowerSE = Avg_Predicted_VLOAD_log - Avg_Predicted_SE;
    UpperSE = Avg_Predicted_VLOAD_log + Avg_Predicted_SE;
	RUN;

* Create the final plot;
PROC SGPLOT DATA = Summarized_Data;
	SERIES X = Years Y = Avg_Predicted_VLOAD_log / GROUP = ADH_HIGHVSLOW LINEATTRS = (THICKNESS = 2);
	BAND X = Years LOWER = LowerSE UPPER = UpperSE / GROUP = ADH_HIGHVSLOW TRANSPARENCY = 0.7;
	XAXIS LABEL = "Year";
	YAXIS LABEL = "Predicted Log Viral Load";
	TITLE "Predicted Log Viral Load by Adherhence Level";
	RUN;

**** CD4+ T Cell Count;
* Suppress the PROC PLM Table from the output;
ODS EXCLUDE ALL;

* Generate Predicted Values and SE's;
PROC PLM RESTORE = Model_LEU3N;
    SCORE DATA = Proj2LMM.Data OUT = Predicted_Data PREDICTED = Predicted_LEU3N STDERR = Predicted_SE;
RUN;

ODS SELECT ALL;

* Calculate the average predicted values and SE's;
PROC SQL;
    CREATE TABLE Summarized_Data AS
    SELECT Years,
           hard_drugs_grp,
           mean(Predicted_LEU3N) as Avg_Predicted_LEU3N,
           mean(Predicted_SE) as Avg_Predicted_SE
    FROM Predicted_Data
    GROUP BY Years, hard_drugs_grp;
	QUIT;

* Calculate the lower and upper bounds for the standard errors;
DATA Summarized_Data;
    SET Summarized_Data;
    LowerSE = Avg_Predicted_LEU3N - Avg_Predicted_SE;
    UpperSE = Avg_Predicted_LEU3N + Avg_Predicted_SE;
	RUN;

* Create the final plot;
PROC SGPLOT DATA = Summarized_Data;
	SERIES X = Years Y = Avg_Predicted_LEU3N / GROUP = hard_drugs_grp LINEATTRS = (THICKNESS = 2);
	BAND X = Years LOWER = LowerSE UPPER = UpperSE / GROUP = hard_drugs_grp TRANSPARENCY = 0.7;
	XAXIS LABEL = "Year";
	YAXIS LABEL = "Predicted CD4+ T Cell Count";
	TITLE "Predicted CD4+ T Cell Count by Hard Drug Use";
	RUN;
	
**** Mental QOL;

* Exclude PROC PLM Table;
ODS EXCLUDE ALL;

* Use PROC PLM to generate the predicted values under the final model;
PROC PLM RESTORE = Model_AGG_MENT;
	SCORE DATA = Proj2LMM.Data OUT = Proj2LMM.Data PREDICTED = Predicted_AGG_MENT;
	RUN;

ODS SELECT ALL;
	
* Visualize the predicted values under the final model;
PROC SGPLOT DATA = Proj2LMM.Data;
	REG X = Years Y = Predicted_AGG_MENT / GROUP = hard_drugs_grp LINEATTRS = (THICKNESS=2) NOMARKERS CLM CLMTRANSPARENCY=0.7;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Predicted Mental QOL";
	TITLE "Predicted Mental QOL by Hard Drug Use";
	RUN;
	
**** Physical QOL;
** Visualize;

* Exclude PROC PLM Table;
ODS EXCLUDE ALL;

* Use PROC PLM to generate the predicted values under the final model;
PROC PLM RESTORE = Model_AGG_PHYS;
	SCORE DATA = Proj2LMM.Data OUT = Proj2LMM.Data PREDICTED = Predicted_AGG_PHYS;
	RUN;
	
ODS SELECT ALL;
	
* Visualize the predicted values under the final model;
PROC SGPLOT DATA = Proj2LMM.Data;
	REG X = Years Y = Predicted_AGG_PHYS / GROUP = hard_drugs_grp LINEATTRS = (THICKNESS=2) NOMARKERS CLM CLMTRANSPARENCY = 0.7;
	XAXIS LABEL = "Years";
	YAXIS LABEL = "Predicted Physical QOL";
	TITLE "Predicted Physical QOL by Hard Drug Use";
	RUN;
