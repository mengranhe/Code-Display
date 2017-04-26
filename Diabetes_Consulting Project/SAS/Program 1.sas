/*S17*/
/*Diabetes and Communal Coping*/
/*Hierarchical Modeling*/

/* Allow SAS Studio vs. native SAS */
%LET wd = /folders/myfolders/726;

/*Create library*/
LIBNAME lib "&wd";

/* Import dataset */
DATA lib.test;
	INFILE "&wd/sasdata.csv" DELIMITER = "," FIRSTOBS = 2 TRUNCOVER;
	INPUT X$ coupleid date :$10. time d1sex$ p1sex$ 
	d1racewhbl$ p1racewhbl$ isdhandler :$13. isphandler :$13. 
	isdcc1r :$13. ispcc1r :$13. isdcc2 ispcc2 isdemotional 
	ispemotional isdinstr ispinstr isdminimize
	ispminimize isdavoid ispavoid isdunsupp ispunsupp 
	isdhappy isphappy isddepr ispdepr isdanger 
	ispanger isdanx ispanx;
	IF isdhandler = "NA" THEN isdhandler = .;
	IF d1sex = "NA" THEN d1sex = .;
	IF p1sex = "NA" THEN p1sex = .;
	IF d1racewhbl = "NA" THEN d1racewhbl = .;
	IF p1racewhbl = "NA" THEN p1racewhbl = .;
	IF isphandler = "NA" THEN isphandler = .;
	IF isdcc1r = "NA" THEN isdcc1r = .;
	IF ispcc1r = "NA" THEN ispcc1r = .;
	timesq = time ** 2;
RUN;

/* Testing */
/* PROC MIXED DATA = lib.models COVTEST;
	CLASS coupleid d1sex p1sex d1racewhbl p1racewhbl
	 isdhandler isphandler isdcc1r ispcc1r;
	MODEL dv = ivs time / SOLUTION;
	REPEATED / SUBJECT = coupleid TYPE = AR(1);
	RANDOM INTERCEPT / SUBJECT = coupleid;
	RANDOM time / SUBJECT = coupleid;
RUN; */

/* Create full models for testing */
/* DATA lib.models_patient;
	INFILE DATALINES DELIMITER = ",";
	INPUT dv :$80. ivs :$100.;
	DATALINES;
	isdhappy, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r 
	isdunsupp, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
    isdemotional, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isdinstr, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isdminimize,isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isdavoid, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r 
	isdanx, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
    isddepr, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isdanger, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isphappy, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r 
	ispunsupp, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
    ispemotional, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	ispinstr, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	ispminimize,isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	ispavoid, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r 
	ispanx, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
    ispdepr, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	ispanger, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
RUN; */

/* Single test */
/* DATA lib.models_patient;
	INFILE DATALINES DELIMITER = ",";
	INPUT dv :$80. ivs :$1000.;
	DATALINES; 
	isdhappy, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isphappy, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	isddepr, isdhandler d1sex d1racewhbl isdcc2 isdcc1r isphandler p1sex p1racewhbl ispcc2 ispcc1r
	; 
RUN; */

/* Interactions */
DATA lib.models_patient;
	INFILE "&wd/SAS_Models/isdanger_models.csv" DELIMITER = "," FIRSTOBS = 2;
	INPUT dv :$80. ivs :$1000. vars :$1000. demog :$1000. inter :$1000.;
	dv = DEQUOTE(dv);
	ivs = DEQUOTE(ivs);
	vars = DEQUOTE(vars);
	demog = DEQUOTE(demog);
	inter = DEQUOTE(inter);
	LABEL
		vars = "Communal Coping Variables"
		demog = "Demographic Variables"
		inter = "Model Interactions";
RUN;

/* Store BICs */
DATA lib.isdanger_bics;
	INFILE DATALINES DELIMITER = ",";
	INPUT Descr$ Value;
	DATALINES;
RUN;

/*Macro for running mixture models*/
/*dataset is the data to run mixture model on. Must have column 'dept'*/
/*models requires columns 'dv' (dependent variable), 'ivs' (predictors)*/
/*Returns mixture model for every row in models*/
%MACRO diabetesModels(dataset, models, bic);
	DATA _NULL_;
		SET &models;
		CALL SYMPUT('dep', dv);
		CALL SYMPUT('indep', ivs);
		CALL SYMPUT('cc', _N_);
		CALL EXECUTE(CAT("TITLE &dep vs. &indep;"));
		CALL EXECUTE(CAT("PROC MIXED DATA = &dataset COVTEST PLOTS = ALL;
			CLASS coupleid d1sex p1sex d1racewhbl p1racewhbl
			 isdhandler isphandler isdcc1r ispcc1r;
			MODEL &dep = &indep time timesq;
			REPEATED / SUBJECT = coupleid TYPE = AR(1);
			RANDOM INTERCEPT / SUBJECT = coupleid;
			RANDOM time / SUBJECT = coupleid;
			RANDOM timesq / SUBJECT = coupleid;
			ODS SELECT FitStatistics;
			ODS OUTPUT FitStatistics = ms2 (WHERE = (Descr = 'BIC (Smaller is Better)'));
			RUN;"));
		CALL EXECUTE(CAT("PROC APPEND BASE = &bic DATA = ms2 FORCE;
		RUN;"));
	RUN;
%MEND;

%diabetesModels(lib.test, lib.models_patient, lib.isdanger_bics);

