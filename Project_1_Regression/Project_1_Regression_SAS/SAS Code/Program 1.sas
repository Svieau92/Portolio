*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    Project 1- SLR	                                						*
*   PURPOSE:    Recreate Analysis of Project 1 in SAS					                *
*   AUTHOR:     Sean Vieau	                                                            *
*   CREATED:    2024-09-26                                                              *
*                                                                                       *
*   COURSE:     Advanced Data Analysis					                                *
*   DATA USED:  Project1_data.csv                                                       *
*   SOFTWARE:   SAS (r) Proprietary Software 9.4 (TS1M4)                                *

*****************************************************************************************
***********************************************************************************; RUN;


*****************************************************************************************
* Step 1: Import Dataset
*****************************************************************************************;

* Step 1.1 Create library;
%LET CourseRoot = /home/u63376223/sasuser.v94/Advanced Data Analysis;
LIBNAME Proj1 "&CourseRoot/Project 1";
	
* Step 1.2a Import the dataset;
PROC IMPORT
	DATAFILE = "&CourseRoot/Project 1/Project1_data.csv"
	OUT = Proj1.raw_data
	REPLACE;
	RUN;

* Step 1.2 Fix missing value format.
* Note: We have missing values as 'NA'. SAS will not be able to handle those.
	We need to convert them to missing values in SAS format ("" for characters and . for numeric);
* Fix missing value format for attachment loss;
DATA Proj1.data;
	SET Proj1.raw_data;
	if attach1year = "NA" then attach1year = "";
	attach1year = INPUT(attach1year, best32.);
	RUN;
	
* Fix missing value format for pocket depth change;
DATA Proj1.data;
	SET Proj1.data;
	if pd1year = "NA" then pd1year = "";
	pd1year = INPUT(pd1year, best32.);
	RUN;
	
/* Convert 'NA' to missing values and ensure numeric format for multiple variables */
/* Code from ChatGPT */
DATA Proj1.data;
    SET Proj1.data;
    
    /* Define arrays for character and numeric variables */
    array char_vars[2] attach1year pd1year ; /* Add more variables as needed */
    array num_vars[2] attach1year_num pd1year_num; /* Corresponding numeric variables */
    
    /* Loop through the arrays to handle 'NA' and convert to numeric */
    do i = 1 to dim(char_vars);
        if char_vars[i] = 'NA' then char_vars[i] = '';
        num_vars[i] = input(char_vars[i], best32.);
    end;
    
    /* Drop the original character variables and rename the numeric ones */
    drop attach1year pd1year; /* Add more variables as needed */
    rename attach1year_num = attach1year pd1year_num = pd1year; /* Corresponding renaming */
RUN;

	
* Step 1.3a Create labels for categorical variables;
* Creat format;
PROC FORMAT;
	VALUE trtgroup_fmt
		1 = "Placebo"
		2 = "Control"
		3 = "Low"
		4 = "Medium"
		5 = "High";
	VALUE gender_fmt
		1 = "Male"
		2 = "Female";
	VALUE race_fmt
		1 = "Native American"
		2 = "African American"
		4 = "White"
		5 = "Asian";
	VALUE smoker_fmt
		0 = "Non-Smoker"
		1 = "Smoker";
	RUN;

* Apply format;
DATA Proj1.data;
	SET Proj1.data;
	FORMAT 	trtgroup trtgroup_fmt.
			gender gender_fmt.
			race race_fmt.
			smoker smoker_fmt.;
	RUN;
	
* Step 1.3b Change labels of all variable names so they are capitalized;
DATA Proj1.data;
	SET Proj1.data;
	LABEL 
		id = "ID"
		trtgroup = "Treatment Group"
		gender = "Gender"
		race= "Race"
		age = "Age"
		smoker = "Smoking Status"
		sites = "Sites"
		attachbase = "Attachment Loss Baseline"
		attach1year = "Attachment Loss One Year"
		pdbase = "Pocket Depth Baseline"
		pd1year = "Pocket Depth One Year"
		;
	RUN;
	
* Step 1.4 View Dataset;
PROC PRINT DATA = Proj1.data (OBS = 20);
	RUN;
	
*****************************************************************************************
* Step 2: Data Management and Variable Creation
*****************************************************************************************;

* Step 2.1 Visualize Data Using Boxplots To Assess for Outliers;
TITLE "Boxplot of Attachment Loss at Baseline";
PROC SGPLOT DATA = Proj1.data;
	VBOX attachbase;
	RUN;
	
TITLE "Boxplot of Attachment Loss at 1 Year";
PROC SGPLOT DATA = Proj1.data;
	VBOX attach1year;
	RUN;
	
TITLE "Boxplot of Pocket Depth at 1 Baseline";
PROC SGPLOT DATA = Proj1.data;
	VBOX pdbase;
	RUN;
	
TITLE "Boxplot of Pocket Depth at 1 Year";
PROC SGPLOT DATA = Proj1.data;
	VBOX pd1year;
	RUN;
	
* Step 2.2 Variable Creation;
* Here we will create change scores for attachment loss and pocket depth.;
DATA Proj1.data;
	SET Proj1.data;
	attachchange = attach1year - attachbase;
	pdchange = pd1year - pdbase;
	LABEL 
	attachchange = "Attachment Loss Change"
	pdchange = "Pocket Depth Change";
	RUN;
	
		
* Step 2.3 Examine Missingness;
TITLE "Missingness";
PROC MEANS DATA = Proj1.data n nmiss;
	VAR _ALL_;
	RUN;
	
* Break missing data down by treatment group;
TITLE "Missingness by Treatment Condition";
PROC MEANS DATA = Proj1.data n nmiss;
	CLASS trtgroup;
	VAR attachchange;
	RUN;
	
* Break missing data down by gender;
TITLE "Missingness by Gender";
PROC MEANS DATA = Proj1.data n nmiss;
	CLASS gender;
	VAR attachchange;
	RUN;
	
*****************************************************************************************
* Step 3: Descriptive Statistics
*****************************************************************************************;
* Generate Table 1;
TITLE "Table 1";
PROC TABULATE DATA=Proj1.data;
    CLASS trtgroup gender race smoker;
    VAR age sites attachbase attach1year pdbase pd1year attachchange pdchange;
    TABLE  
        (gender race smoker)*(n) (age sites attachbase attach1year pdbase pd1year attachchange pdchange)*(n mean std min max), 
        trtgroup ALL;
RUN;

*****************************************************************************************
* Step 4: Preliminary Evaluation of Assumptions
*****************************************************************************************;
* Step 4.1 Correlation Matrix;

* Create and visualize the correlation matrix;
TITLE "Correlation Matrix";
ODS GRAPHICS ON;
PROC CORR DATA = Proj1.data PLOTS = MATRIX(HISTOGRAM);
	VAR age sites attachbase attach1year pdbase pd1year attachchange pdchange;
	RUN;
ODS GRAPHICS OFF;

* Step 4.2 Assess Normality of Dependent Variables (attachchange, pdchange);
* Note: Here we would use the Kolmogorov-Smirnov test, since our sample size is > 50
	if the sample size was < 50 you would use Shapiro Wilk's test;
TITLE "Normality Assesment";
PROC UNIVARIATE DATA = Proj1.data;
	VAR attachchange pdchange;
	HISTOGRAM attachchange pdchange / NORMAL;
	QQPLOT attachchange pdchange / NORMAL (MU=EST SIGMA = EST);
	RUN;
	
*****************************************************************************************
* Step 5: Exploratory Data Analysis
*****************************************************************************************;
* Step 5.1 Assess Relationship Between Primary Outcomes attachchange / pdchange and Treatment Condition;
* Boxplot of attachchange vs treatment condition;
TITLE "Primary Attachment Loss Change by Treatment Condition";
PROC SGPLOT DATA = Proj1.data;
	VBOX attachchange / CATEGORY = trtgroup;
	RUN;

* Boxplot of pdchange vs treatment condition;
TITLE "Primary Pocket Depth Change by Treatment Condition";
PROC SGPLOT DATA = Proj1.data;
	VBOX pdchange / CATEGORY = trtgroup;
	RUN;	
	
* Step 5.2 Assess Relationship Between Potential Covariates and Primary Outcomes attachchange / pdchange;

* Step 5.2a Gender;
* Create boxplot of gender vs attachmnent loss change;
TITLE "Boxplot of Gender vs Attachment Loss Change";
PROC SGPLOT DATA = Proj1.data;
	VBOX attachchange / CATEGORY = gender;
	RUN;
	
* Create boxplot of gender vs pocket depth change;
TITLE "Boxplot of Gender vs Pocket Depth Change";
PROC SGPLOT DATA = Proj1.data;
	VBOX pdchange / CATEGORY = gender;
	RUN;
	
* Note: Males may have more pocket depth change than females
	Run a t-test to assess;
PROC TTEST DATA = Proj1.data;
	CLASS gender;
	VAR pdchange;
	RUN;
	
* Note: T-test significant, including gender in final model;

* Step 5.2b Age;
* Create scatterplot of age vs attachment loss change;
TITLE "Age vs Attachment Loss Change";
PROC SGPLOT DATA = Proj1.data;
	SCATTER X = age y = attachchange / GROUP = trtgroup;
	RUN;
	
* Create scatterplot of sites vs pocket depth change;
TITLE "Age vs Pocket Depth Change";
PROC SGPLOT DATA = Proj1.data;
	SCATTER X = age y = pdchange / GROUP = trtgroup;
	RUN;

* Step 5.2c Sites;
* Create scatterplot of sites vs attachment loss change;
TITLE "Sites vs Attachment Loss Change";
PROC SGPLOT DATA = Proj1.data;
	SCATTER X = sites y = attachchange / GROUP = trtgroup;
	RUN;
	
* Create scatterplot of sites vs pocket depth change;
TITLE "Sites vs Pocket Depth Change";
PROC SGPLOT DATA = Proj1.data;
	SCATTER X = sites y = pdchange / GROUP = trtgroup;
	RUN;

* Step 5.2d Race;
* Create boxplot of race vs attachment loss change;
TITLE "Race vs Attachment Loss Change";
PROC SGPLOT DATA = Proj1.data;
	VBOX attachchange / CATEGORY = race;
	RUN;
	
* Create boxplot of race vs pocket depth change;
TITLE "Race vs Pocket Depth Change";
PROC SGPLOT DATA = Proj1.data;
	VBOX pdchange / CATEGORY = race;
	RUN;

* Step 5.2e Smoking Status
* Create boxplot of smoking status vs attachment loss change;
TITLE "Smoking Status vs Attachment Loss Change";
PROC SGPLOT DATA = Proj1.data;
	VBOX attachchange / CATEGORY = smoker;
	RUN;

* Create boxplot of smoking status vs pocket depth change;
TITLE "Smoking Status vs Pocket Depth Change";
PROC SGPLOT DATA = Proj1.data;
	VBOX pdchange / CATEGORY = smoker;
	RUN;

* Note: there appears to be no difference in outcome variables by 
 any of the potential covariates except for gender;
 
*****************************************************************************************
* Step 6: Data Analysis
*****************************************************************************************;
 
* Step 6.1 Create Dummy Variables;
TITLE "Create Dummy Variables";
DATA Proj1.data;
    SET Proj1.data;
    IF trtgroup ne . THEN DO;
        placebo = (trtgroup = 1);
        control = (trtgroup = 2);
        low     = (trtgroup = 3);
        medium  = (trtgroup = 4);
        high    = (trtgroup = 5);
	END;
RUN;

* Check Dummy Variables Created Correctly;
PROC PRINT DATA = Proj1.data (OBS = 6);
	RUN;

* Step 6.2 Simple Linear Regression Predicting Attachment Loss Change by Treatment Condition;

* Run regression of attachment loss by treatment condition with control as reference;
TITLE "SLR of Attachment Loss Change by Treatment Condition - Control as Reference";
PROC REG DATA = Proj1.data;
	MODEL attachchange = placebo low medium high;
	RUN;
	
* Run regression of attachment loss by treatment condition with placebo as reference;
TITLE "SLR of Attachment Loss Change by Treatment Condition - Placebo as Reference";
PROC REG DATA = Proj1.data;
	MODEL attachchange = control low medium high;
	RUN;
	
* Step 6.3 Simple Linear Regression Predicting Pocket Depth Change by Treatment Condition;
TITLE "SLR of Pocket Depth Change by Treatment Condition - Control as Reference";
PROC REG DATA = Proj1.data;
	MODEL pdchange = placebo low medium high;
	RUN;
	
* Step 6.4 Try a model of attachment loss change by treatment condition, controlling for gender;
TITLE "MLR of Attachment Loss Change by Treatment Condition and Gender";
PROC REG DATA = Proj1.data;
	MODEL attachchange = placebo low medium high gender;
	RUN;
	
* Step 6.4 Try a model of attachment loss change by treatment condition, controlling for gender;
TITLE "MLR of Pocket Depth Change by Treatment Condition and Gender";
PROC REG DATA = Proj1.data;
	MODEL pdchange = placebo low medium high gender;
	RUN;
	
	
	
	