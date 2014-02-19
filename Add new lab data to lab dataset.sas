%macro add_new_DUA_labs(DUA);

	/*######################################################################################################################################################*/
	/*Identify lab variables*/ data _null_;
	/*######################################################################################################################################################*/

		Proc sql;
			select name into :lab_var separated by ' '
			from dictionary.columns
			where 	libname="&DUA." 
					and memname="ALCO_&DUA."
					and (index(name,'vl') NE 0 or index(name,'cd4') NE 0) 
					and index(name,'num')=0
			order by name;

	/*######################################################################################################################################################*/
	/*Reshape data*/ data _null_;
	/*######################################################################################################################################################*/

			%do i=1 %to 9; /*for each set of lab variables (date, value, and sometimes type)...*/
				%if &i.=1 %then %let test=cd4_first_14;
				%else %if &i.=2 %then %let test=cd4_first_200;
				%else %if &i.=3 %then %let test=cd4_low_cnt;
				%else %if &i.=4 %then %let test=cd4_low_pct;
				%else %if &i.=5 %then %let test=cd4_recent_cnt;
				%else %if &i.=6 %then %let test=cd4_recent_pct;
				%else %if &i.=7 %then %let test=vl_first_det;
				%else %if &i.=8 %then %let test=vl_recent;
				%else %if &i.=9 %then %let test=cd4_first_hiv;

				data &test._1; /*...extract just those variables (along with stateno) from the full dataset...*/
					set &DUA..AlCo_&DUA.;
					%if &i.=9 %then %do;
						keep stateno cd4_first_hiv_dt cd4_first_hiv_value cd4_first_hiv_type;
					%end;
					%else %do;
						keep stateno &test._dt &test._value;
					%end;
				run;

				data &test._2; /*...create the generic lab_test, lab_dt, and lab_value variables and populate with the value of the appropriate DUA variable...*/
					set &test._1;

					length lab_date $ 8;
					lab_date=&test._dt;

					length lab_value $ 8;
					lab_value=&test._value;

					length lab_test $ 8;
					if &i.=9 then do;
						if cd4_first_hiv_type='CNT' then lab_test='CD4 cnt';
						else if cd4_first_hiv_type='PCT' then lab_test='CD4 pct';
						else lab_test='';
					end;
					else if (index("&test.",'cd4') NE 0 and index("&test.",'pct') NE 0)
						or (index("&test.",'cd4') NE 0 and index("&test.",'14') NE 0)
						then lab_test='CD4 pct';
					else if (index("&test.",'cd4') NE 0 and index("&test.",'cnt') NE 0)
						or (index("&test.",'cd4') NE 0 and index("&test.",'200') NE 0)
						then lab_test='CD4 cnt';
					else if index("&test.",'vl') then lab_test='VL';
				run;

				Proc sql; /*...and keep only these new generically-named variables*/
					create table &test. as
						select stateno, lab_date, lab_test, lab_value
						from &test._2;
			%end;

		data all_labs;
			set 
				cd4_first_14
				cd4_first_200
				cd4_low_cnt
				cd4_low_pct
				cd4_recent_cnt
				cd4_recent_pct
				vl_first_det
				vl_recent
				cd4_first_hiv;

				where 	not missing(stateno) 
						and not missing(lab_date);
		run;

	/*##########################################################################################################################################################*/
	/*Create numeric counterparts to dates and test result values stored in character variables*/
	/*##########################################################################################################################################################*/

		data all_labs_recode;
			set all_labs;

			length lab_value_num 4.;
			label lab_value='lab_value';
			lab_value_num=input(lab_value,8.);

			%MDImp(lab_date);
			%DImp(lab_date);
		run;

		/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
		/*%-%-%*/%macro check_labs;
			data check_labs;
				set all_labs_recode;
				rand=ranuni(1);
			run;

			proc sort data=check_labs; by rand; run;

			Proc sql;
				create table check_labs_sample as
					select lab_date, lab_date_num_MDImp, lab_date_num_DImp,
							lab_value, lab_value_num
					from check_labs
					where rand<0.001;
		/*%-%-%*/%mend;
		/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

		/*	%check_labs; run;*/

	/*######################################################################################################################################################*/
	/*Add to existing DUA labs dataset and de-duplicate on stateno and test date and type*/ data _null_;
	/*######################################################################################################################################################*/

		data all_labs_concat;
			set all_labs_recode lablib.all_dua_labs;
		run;

		proc sort data=all_labs_concat out=all_labs_dedup nodupkey; 
			by stateno lab_date lab_test; 
		run;

	/*######################################################################################################################################################*/
	/*Write dataset to disk*/ data _null_;
	/*######################################################################################################################################################*/

		options replace;
		data lablib.all_DUA_labs(label="Q4_2010_to_&DUA."); set labs_recode; run;
		options noreplace;
%mend;

/*%add_new_DUA_labs(DUA=Q4_2013); run;*/
