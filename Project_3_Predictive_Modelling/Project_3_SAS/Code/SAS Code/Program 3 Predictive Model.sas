*************   P   R   O   G   R   A   M       H   E   A   D   E   R   *****************
*****************************************************************************************
*                                                                                       *
*   PROGRAM:    Project 3 - Predictive Modeling	                                						*
*   PURPOSE:    Recreate Analysis of Project 3 in SAS					                *
*   AUTHOR:     Sean Vieau	                                                            *
*   CREATED:    2024-12-20                                                              *
*                                                                                       *
*   COURSE:     Advanced Data Analysis					                                *
*   DATA USED:  Proj3_data.csv                                                *
*   SOFTWARE:   SAS (r) Proprietary Software 9.4 (TS1M4)                                *

*****************************************************************************************
***********************************************************************************; RUN;

*****************************************************************************************
* Step 1: Import Dataset
*****************************************************************************************;

* Create library;
%LET CourseRoot = /home/u63376223/sasuser.v94/Advanced Data Analysis;
LIBNAME Proj3 "&CourseRoot/Project 3 Predictive Model";

* Import the dataset;
PROC IMPORT
    DATAFILE = "&CourseRoot/Project 3 Predictive Model/Project3_data.csv"
    OUT = Proj3.data
    REPLACE;
    GETNAMES = YES;
    RUN;
    
* Examine data;
PROC PRINT DATA = PROJ3.Data (OBS=6);
	RUN;

* Examine formats;	
PROC CONTENTS DATA = PROJ3.DATA;
	RUN;

* Create factors;

* Create format;
PROC FORMAT;
	VALUE ProgressionCd
		0 = "Slow"
		1 = "Fast";
	RUN;
	
* Apply Format;
DATA Proj3.Data;
	SET Proj3.Data;
	FORMAT progression ProgressionCd.;
	RUN;
	

*****************************************************************************************
* Step 2: Feature Engineering
*****************************************************************************************;

**** Correlation Matrix I;

* Print correlation matrix;
PROC CORR DATA = Proj3.Data OUTP = corr_matr;
	VAR tkvht_base tkvht_change progression	 geom1 geom2 gabor1	gabor2	gabor3	gabor4	gabor5	glcm1	glcm2	txti1	txti2	txti3	txti4	txti5	lbp1	lbp2	lbp3	lbp4	lbp5; * List variables here;
	RUN;
	

* Create a dataset with only the correlation coefficients;
DATA corr_only;
    SET corr_matr;
    IF _TYPE_ = 'CORR'; /* Select only the rows with correlation coefficients */
RUN;

* Reshape the correlation matrix for the heat map;
DATA heatmap_data;
    SET corr_only;
    ARRAY vars[*] tkvht_base tkvht_change  progression	geom1 geom2	gabor1	gabor2	gabor3	gabor4	gabor5	glcm1	glcm2	txti1	txti2	txti3	txti4	txti5	lbp1	lbp2	lbp3	lbp4	lbp5; * List variables here;
    DO i = 1 to DIM(vars);
        ROW = _NAME_;
        COL = VNAME(vars[i]);
        corr_value = vars[i];
        IF NOT missing(corr_value) THEN DO;
            LABEL = PUT(corr_value, 8.2); * Format the text label;
            OUTPUT;
        END;
    END;
    KEEP row col corr_value label;
RUN;

* Set the dimensions of the output graphic;
ODS GRAPHICS / WIDTH=1000px HEIGHT=800px;

* Create the Heat Map;
PROC SGPLOT DATA=heatmap_data;
    HEATMAPPARM X=col Y=row COLORRESPONSE = corr_value / COLORMODEL = (blue white red);
    TEXT X=col Y=row TEXT=label / TEXTATTRS = (SIZE=8) POSITION = center;
    XAXIS DISPLAY = (nolabel) DISCRETEORDER = data;
    YAXIS DISPLAY=(nolabel) DISCRETEORDER = data;
    TITLE "Correlation Matrix Heat Map";
RUN;

**** Feature engineering II
** Create Average and Interaction Terms;
DATA Proj3.Data;
	SET Proj3.Data;
	gabor_avg = mean(gabor1 + gabor4);
	gabor_int = gabor1*gabor4;
	txti_avg = mean(txti2 + txti3);
	txti_int = txti2*txti3;
	lbp_avg = mean(lbp1 + lbp3 + lbp4);
	lbp_int = lbp1*lbp3*lbp4;
	RUN;
	
* Create correlation heat map with new engineered terms;


* Print correlation matrix;
PROC CORR DATA = Proj3.Data OUTP = corr_matr;
	VAR tkvht_base tkvht_change progression gabor_avg gabor_int txti_avg txti_int lbp_avg lbp_int;
	RUN;
	
* Create a dataset with only the correlation coefficients;
DATA corr_only;
    SET corr_matr;
    IF _TYPE_ = 'CORR'; /* Select only the rows with correlation coefficients */
RUN;

* Reshape the correlation matrix for the heat map;
DATA heatmap_data;
    SET corr_only;
    ARRAY vars[*] tkvht_base tkvht_change progression gabor_avg gabor_int txti_avg txti_int lbp_avg lbp_int;
    DO i = 1 to DIM(vars);
        ROW = _NAME_;
        COL = VNAME(vars[i]);
        corr_value = vars[i];
        IF NOT missing(corr_value) THEN DO;
            LABEL = PUT(corr_value, 8.2); * Format the text label;
            OUTPUT;
        END;
    END;
    KEEP row col corr_value label;
RUN;

* Set the dimensions of the output graphic;
ODS GRAPHICS / WIDTH=1000px HEIGHT=800px;

* Create the Heat Map;
PROC SGPLOT DATA=heatmap_data;
    HEATMAPPARM X=col Y=row COLORRESPONSE = corr_value / COLORMODEL = (blue white red);
    TEXT X=col Y=row TEXT=label / TEXTATTRS = (SIZE=8) POSITION = center;
    XAXIS DISPLAY = (nolabel) DISCRETEORDER = data;
    YAXIS DISPLAY=(nolabel) DISCRETEORDER = data;
    TITLE "Correlation Matrix Heat Map";
RUN;

**** Feature engineering III;
* Create quadratic terms;
DATA Proj3.Data;
	SET Proj3.Data;
	geom1_sq = geom1**2;
	geom2_sq = geom2**2;
	gabor1_sq = gabor1**2;
	gabor2_sq = gabor2**2;
	gabor3_sq = gabor3**2;
	gabor4_sq = gabor4**2;
	gabor5_sq = gabor5**2;
	glcm1_sq = glcm1**2;
	glcm2_sq = glcm2**2;
	txti1_sq = txti1**2;
	txti2_sq = txti2**2;
	txti3_sq = txti3**2;
	txti4_sq = txti4**2;
	txti5_sq = txti5**2;
	lbp1_sq = lbp1**2;
	lbp2_sq = lbp2**2;
	lbp3_sq = lbp3**2;
	lbp4_sq = lbp4**2;
	lbp5_sq = lbp5**2;
	RUN;
	
* Print correlation matrix;
PROC CORR DATA = Proj3.Data OUTP = corr_matr;
	VAR tkvht_base tkvht_change progression geom1_sq geom2_sq gabor1_sq gabor2_sq gabor3_sq gabor4_sq gabor5_sq glcm1_sq glcm2_sq txti1_sq txti2_sq txti3_sq txti4_sq txti5_sq lbp1_sq lbp2_sq lbp3_sq lbp4_sq lbp5_sq;
	RUN;
	
* Create a dataset with only the correlation coefficients;
DATA corr_only;
    SET corr_matr;
    IF _TYPE_ = 'CORR'; /* Select only the rows with correlation coefficients */
RUN;

* Reshape the correlation matrix for the heat map;
DATA heatmap_data;
    SET corr_only;
    ARRAY vars[*] tkvht_base tkvht_change progression geom1_sq geom2_sq gabor1_sq gabor2_sq gabor3_sq gabor4_sq gabor5_sq glcm1_sq glcm2_sq txti1_sq txti2_sq txti3_sq txti4_sq txti5_sq lbp1_sq lbp2_sq lbp3_sq lbp4_sq lbp5_sq;
    DO i = 1 to DIM(vars);
        ROW = _NAME_;
        COL = VNAME(vars[i]);
        corr_value = vars[i];
        IF NOT missing(corr_value) THEN DO;
            LABEL = PUT(corr_value, 8.2); * Format the text label;
            OUTPUT;
        END;
    END;
    KEEP row col corr_value label;
RUN;

* Set the dimensions of the output graphic;
ODS GRAPHICS / WIDTH=1000px HEIGHT=800px;

* Create the Heat Map;
PROC SGPLOT DATA=heatmap_data;
    HEATMAPPARM X=col Y=row COLORRESPONSE = corr_value / COLORMODEL = (blue white red);
    TEXT X=col Y=row TEXT=label / TEXTATTRS = (SIZE=8) POSITION = center;
    XAXIS DISPLAY = (nolabel) DISCRETEORDER = data;
    YAXIS DISPLAY=(nolabel) DISCRETEORDER = data;
    TITLE "Correlation Matrix Heat Map";
RUN;


*****************************************************************************************
* Step 3: Feature Scaling
*****************************************************************************************;
******** Feature Scaling/Standardizing;
* Standardize the variables;
PROC STANDARD DATA = Proj3.Data OUT = Proj3.data_scaled MEAN = 0 STD = 1;
	VAR lbp_int txti2 gabor3_sq;
	RUN;

*** Model 1A, Baseline Kidney Volume;
* Perform 5-Fold Cross Validation and Linear Regression;
PROC GLMSELECT DATA = Proj3.Data_scaled PLOTS = ALL SEED = 123;
	PARTITION FRACTION(VALIDATE = 0.2); * Sets up the 5-Fold CV;
	MODEL tkvht_change = tkvht_base / CVMETHOD =  SPLIT(5) SELECTION=none;
	OUTPUT OUT = Model1A PRED = Predicted RESID = Residuals;
	RUN;
	
* Plot residuals vs. predicted values;
PROC SGPLOT DATA = Model1A;
    SCATTER X=Predicted Y=Residuals;
    REFLINE 0 / AXIS=y LINEATTRS=(pattern=shortdash);
    XAXIS LABEL='Predicted Values';
    YAXIS LABEL='Residuals';
    TITLE 'Residuals vs. Predicted Values';
	RUN;

	
** Model 1B: MRI Image Features;
* Perform 5-Fold Cross Validation and Linear Regression;
PROC GLMSELECT DATA = Proj3.Data_scaled PLOTS = ALL SEED = 123;
	PARTITION FRACTION(VALIDATE = 0.2); * Sets up the 5-Fold CV;
	MODEL tkvht_change = lbp_int txti2 gabor3_sq / CVMETHOD =  SPLIT(5) SELECTION = NONE;
	RUN;
	
* We can see that gabor3_sq makes the model perform worse on all metrics. We remove it.

** Model 1B: MRI Image Features;
* Perform 5-Fold Cross Validation and Linear Regression;
PROC GLMSELECT DATA = Proj3.Data_scaled PLOTS = ALL SEED = 123;
	PARTITION FRACTION(VALIDATE = 0.2); * Sets up the 5-Fold CV;
	MODEL tkvht_change = lbp_int txti2 / CVMETHOD =  SPLIT(5) SELECTION = NONE;
	OUTPUT OUT = Model1B PRED = Predicted RESID = Residuals;
	RUN;
	
* Plot residuals vs. predicted values;
PROC SGPLOT DATA = Model1B;
    SCATTER X=Predicted Y=Residuals;
    REFLINE 0 / AXIS=y LINEATTRS=(pattern=shortdash);
    XAXIS LABEL='Predicted Values';
    YAXIS LABEL='Residuals';
    TITLE 'Residuals vs. Predicted Values';
	RUN;

** Model 1C: Baseline + MRI Image Features;
* Perform 5-Fold Cross Validation and Linear Regression;
PROC GLMSELECT DATA = Proj3.Data_scaled PLOTS = ALL SEED = 123;
	PARTITION FRACTION(VALIDATE = 0.2); * Sets up the 5-Fold CV;
	MODEL tkvht_change = tkvht_base lbp_int txti2 / CVMETHOD =  SPLIT(5) SELECTION = NONE;
	OUTPUT OUT = Model1C PRED = Predicted RESID = Residuals;
	RUN;
	
* Plot residuals vs. predicted values;
PROC SGPLOT DATA = Model1C;
    SCATTER X=Predicted Y=Residuals;
    REFLINE 0 / AXIS=y LINEATTRS=(pattern=shortdash);
    XAXIS LABEL='Predicted Values';
    YAXIS LABEL='Residuals';
    TITLE 'Residuals vs. Predicted Values';
	RUN;
	

*****************************************************************************************
* Step 4: Perform Analyses
*****************************************************************************************;

***** Run the Logistic Regressions;
** 2A: Baseline Kidney Volume Alone;
* Create cross validation folds;
DATA Proj3.Data_Scaled;
	SET Proj3.Data_Scaled;
	fold = mod(_N_, 5) + 1;
	RUN;

* Define a macro to perform the cross validation analyses;
%MACRO cross_validate(data=, target=, predictors=);
    %DO fold = 1 %TO 5;
        /* Split data into training and validation sets */
        DATA train valid;
            SET &data;
            IF fold ne &fold THEN OUTPUT train;
            ELSE OUTPUT valid;
        RUN;

        /* Train the model on the training set */
        PROC LOGISTIC DATA=train;
            MODEL &target(EVENT='Fast') = &predictors;
            OUTPUT=pred_train P=pred;
        run;

        /* Validate the model on the validation set */
       PROC LOGISTIC DATA = valid;
            MODEL &target(EVENT='Fast') = &predictors;
            OUTPUT OUT=pred_valid P=pred;
        run;

        /* Store the validation results */
        DATA results_&fold;
            SET pred_valid;
            IF fold = . THEN fold = &fold;
        RUN;
    %END;
    
    /* Combine the results from all folds */
    DATA all_results;
        SET results_:;
    RUN;
%MEND cross_validate;

/* Run the cross-validation for each model */
%cross_validate(data=Proj3.Data_scaled, target=progression, predictors=tkvht_base);
%cross_validate(data=Proj3.Data_scaled, target=progression, predictors=lbp_int txti2);
%cross_validate(data=Proj3.Data_scaled, target=progression, predictors=tkvht_base lbp_int txti2);


**** Model 2A;
*ODS TRACE ON;
ODS EXCLUDE ModelInfo NOBS ResponseProfile ConvergenceStatus ROCcurve;
/* Calculate ROC curve and AUC for each model */
PROC LOGISTIC DATA = all_results;
    MODEL progression(EVENT = 'Fast') = tkvht_base;
    OUTPUT OUT = Model2A PREDICTED = Model2Apred;
    ROC;
RUN;
*ODS TRACE OFF;

**** Model 2B;
ODS EXCLUDE ModelInfo NOBS ResponseProfile ConvergenceStatus ROCcurve;
/* Calculate ROC curve and AUC for the model */
PROC LOGISTIC DATA = all_results;
    MODEL progression(EVENT = 'Fast') = lbp_int txti2;
    OUTPUT OUT = Model2B PREDICTED = Model2Bpred;
    ROC;
RUN;


**** Model 2C;
ODS EXCLUDE ModelInfo NOBS ResponseProfile ConvergenceStatus ROCcurve;
/* Calculate ROC curve and AUC for  model */
PROC LOGISTIC DATA = all_results;
    MODEL progression(EVENT = 'Fast') = tkvht_base lbp_int txti2;
    OUTPUT OUT = Model2C PREDICTED = Model2Cpred;
    ROC;
RUN;

*** Final ROC Curves PLOT;
* Merge Data sets;
DATA ROC;
   MERGE Model2A Model2B Model2C;
RUN;


* overlay two or more ROC curves by using variables of predicted values;
PROC LOGISTIC DATA = ROC;
   MODEL progression(EVENT='Fast') = Model2APred Model2BPred Model2CPred / NOFIT;
   ROC 'Model2A' PRED=Model2APred;
   ROC 'Model2B'   PRED=Model2BPred;
   ROC 'Model2C'	PRED=Model2CPred;
   ODS SELECT ROCOverlay;
   /* optional: for a statistical comparison, use ROCCONTRAST stmt and remove the ODS SELECT stmt */
   *roccontrast reference('Expert Model') / estimate e;
RUN;





