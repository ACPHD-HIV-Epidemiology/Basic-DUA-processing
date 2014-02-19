	/*Define a list of char var storing dates (for the most part, their names end with "_dt")*/ data _null_;
		Proc sql noprint;
				select name into :dt_var_char separated by ' '
				from dictionary.columns
				where (index(name,"_dt") NE 0 or name in ('dob' 'dod')) and type='char' and libname="WORK" and memname="ALL_DEDUP";

	/*Define a list of num counterparts for each char var storing dates (to be created from the char var in the data step below)*/ data _null_;
		Proc sql noprint;
				select name into :dt_var_num1 separated by '_num '
				from dictionary.columns
				where (index(name,"_dt") NE 0 or name in ('dob' 'dod')) and type='char' and libname="WORK" and memname="ALL_DEDUP";
		%let dt_var_num=&dt_var_num1._num;

	/*Define a list of num counterparts WITH DAY IMPUTED WHERE MISSING for each char var storing dates*/ data _null_;
		Proc sql noprint;
				select name into :dt_var_num_dimp1 separated by '_num_DImp '
				from dictionary.columns
				where (index(name,"_dt") NE 0 or name in ('dob' 'dod')) and type='char' and libname="WORK" and memname="ALL_DEDUP";
		%let dt_var_num_DImp=&dt_var_num_DImp1._num_DImp;

	/*Define a list of num counterparts WITH MONTH & DAY IMPUTED WHERE MISSING for each char var storing dates*/ data _null_;
		Proc sql noprint;
				select name into :dt_var_num_mdimp1 separated by '_num_MDImp '
				from dictionary.columns
				where (index(name,"_dt") NE 0 or name in ('dob' 'dod')) and type='char' and libname="WORK" and memname="ALL_DEDUP";
		%let dt_var_num_mdimp=&dt_var_num_mdimp1._num_MDImp;

/*##########################################################################################################################################################*/
/*Recode variables in all_dedup as needed for analysis and output new dataset all_dedup_recode*/ data _null_;
/*##########################################################################################################################################################*/ run;

	data all_dedup_recode; set all_dedup;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create numeric counterparts to dates stored in character variables...*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/

			/********************************************************************************************************************************************************/
			/*...w/out imputation*/
			/********************************************************************************************************************************************************/
				length &dt_var_num. 4;
				array date_var_char 	$ 	&dt_var_char.;
				array date_var_num 			&dt_var_num.;
				
				do over date_var_char;
					if not missing(date_var_char) and index(date_var_char,'.')=0 then date_var_num=mdy(	INPUT(substr(date_var_char,5,2),2.),
																										INPUT(substr(date_var_char,7,2),2.),
																										INPUT(substr(date_var_char,1,4),4.));
					else date_var_num=.;
				end;

			/********************************************************************************************************************************************************/
			/*...imputing day only, where missing*/
			/********************************************************************************************************************************************************/
				length &dt_var_num_DImp. 4;
				array date_var_num_DImp 		&dt_var_num_DImp.;
				
				do over date_var_char;
					if not missing(date_var_char) and date_var_char NE '........' and substr(date_var_char,5,2) NE ".." then do;
						IF substr(date_var_char,7,2)=".." 
							then date_var_num_DImp=mdy(	INPUT(substr(date_var_char,5,2),2.),
														1,
														INPUT(substr(date_var_char,1,4),4.));
						ELSE date_var_num_DImp=mdy(	INPUT(substr(date_var_char,5,2),2.),
													INPUT(substr(date_var_char,7,2),2.),
													INPUT(substr(date_var_char,1,4),4.));
					end;
					else date_var_num_DImp=.;
				end;

			/********************************************************************************************************************************************************/
			/*...imputing month and/or day only, where missing*/
			/********************************************************************************************************************************************************/
				length &dt_var_num_MDImp. 4;
				array date_var_num_MDImp 		&dt_var_num_MDImp.;
				
				do over date_var_char;
					if not missing(date_var_char) and date_var_char NE '........' then do;
						IF substr(date_var_char,5,2)=".." 
							then date_var_num_MDImp=mdy(1,
														1,
														input(substr(date_var_char,1,4),4.));
						ELSE IF substr(date_var_char,7,2)=".." 
							then date_var_num_MDImp=mdy(INPUT(substr(date_var_char,5,2),2.),
														1,
														INPUT(substr(date_var_char,1,4),4.));
						ELSE date_var_num_MDImp=mdy(INPUT(substr(date_var_char,5,2),2.),
													INPUT(substr(date_var_char,7,2),2.),
													INPUT(substr(date_var_char,1,4),4.));
					end;
					else date_var_num_MDImp=.;
				end;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create a variable identifying cases' first HIV diagnosis (HIV non-AIDS, or AIDS); 
					consider those with an AIDS dx date known to be within 30 days/1 month of their HIV dx date as having a first dx of AIDS*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length first_dx $4;
			label first_dx="First known HIV diagnosis (assumed AIDS if AIDS dx within 30 days or 1 month of HIV dx)";

			if missing(aids_dx_dt_num_MDImp) and missing(hiv_dx_dt_num_MDImp) then first_dx='UNK';
			else if missing(aids_dx_dt_num_MDImp) and not missing(hiv_dx_dt_num_MDImp) then first_dx='HIV'; /*Captures HIV-diagnosed cases that have not yet progressed to AIDS*/
			else if missing(HIV_dx_dt_num_MDImp) and not missing(aids_dx_dt_num_MDImp) then first_dx='AIDS'; /*Captures cases diagnosed with AIDS before tests for HIV were available*/
			else if not missing(aids_dx_dt_num) and not missing(hiv_dx_dt_num) then do;
				if aids_dx_dt_num - hiv_dx_dt_num >30 then first_dx='HIV';
				else first_dx='AIDS';
			end;
			else if not missing(aids_dx_dt_num_DImp) and not missing(hiv_dx_dt_num_DImp) then do;
				if intck('MONTH', hiv_dx_dt_num_DImp, aids_dx_dt_num_DImp) >=1 then first_dx='HIV';
				else first_dx='AIDS';
			end;
			else if not missing(aids_dx_dt_num_MDImp) and not missing(hiv_dx_dt_num_MDImp) then do;
				if year(aids_dx_dt_num_MDImp) >= year(hiv_dx_dt_num_MDImp) then first_dx='HIV';
				else first_dx='AIDS';
			end;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create an indicator for whether the most recent known address is in Alameda County*/
			/*references to *_place variables are commented out ere as they are not created (by spatial join) until after this code is run; 
			this code is re-run WITH these lines AFTER the *_place variables are created*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		length rec_addr_AlCo 3;
		label rec_addr_AlCo='Most recent known address (county and/or city and/or zip) in Alameda County';
		if not missing(cur_county_name) 
			or not missing(cur_city_name) 
			or not missing(cur_state_cd)
			or not missing(cur_zip_cd)
/*			or not missing(cur_place)*/
			then do; 
				if not missing(cur_state_cd) and cur_state_cd NE 'CA' then rec_addr_AlCo=0;
				*else if prxmatch("/&AlCo_cities_vbar./",upcase(put(cur_place,$NewCityFl.))) then rec_addr_AlCo=1;
				else if index(upcase(cur_county_name),'ALAMEDA')
					or prxmatch("/&AlCo_cities_vbar./",upcase(cur_city_name))
					or cur_zip_cd in (&AlCo_only_zips.) 
					then rec_addr_AlCo=1;
				else if missing(cur_county_name) and missing(cur_city_name) and missing(cur_zip_cd) then goto rsa_check;
				else rec_addr_AlCo=0;
				goto done;
			end;
		rsa_check: 
			if not missing(rsa_county_name) 
			or not missing(rsa_city_name) 
			or not missing(rsa_state_cd)
			or not missing(rsa_zip_cd)
/*			or not missing(rsa_place) */
			then do; 
				if not missing(rsa_state_cd) and rsa_state_cd NE 'CA' then rec_addr_AlCo=0;
				*else if prxmatch("/&AlCo_cities_vbar./",upcase(put(rsa_place,$NewCityFl.))) then rec_addr_AlCo=1;
				else if index(upcase(rsa_county_name),'ALAMEDA')
					or prxmatch("/&AlCo_cities_vbar./",upcase(rsa_city_name))
					or rsa_zip_cd in (&AlCo_only_zips.) 
					then rec_addr_AlCo=1;
				else if missing(rsa_county_name) and missing(rsa_city_name) and missing(rsa_zip_cd) then goto rsh_check;
				else rec_addr_AlCo=0;
				goto done;
			end;
		rsh_check: 
			if not missing(rsh_county_name) 
			or not missing(rsh_city_name) 
			or not missing(rsh_state_cd)
			or not missing(rsh_zip_cd)
/*			or not missing(rsh_place) */
			then do; 
				if not missing(rsh_state_cd) and rsh_state_cd NE 'CA' then rec_addr_AlCo=0;
				*else if prxmatch("/&AlCo_cities_vbar./",upcase(put(rsh_place,$NewCityFl.))) then rec_addr_AlCo=1;
				else if index(upcase(rsh_county_name),'ALAMEDA')
					or prxmatch("/&AlCo_cities_vbar./",upcase(rsh_city_name))
					or rsh_zip_cd in (&AlCo_only_zips.) 
					then rec_addr_AlCo=1;
				else rec_addr_AlCo=0;
				goto done;
			end;
		done:

		/********************
			COMMENTED OUT ON 11/06/2013
			REASON: TO STREAMLINE THE CODE
			length hivtestyr 4.; if hiv_dx_dt="........" then hiv_dx_dt=""; hivtestyr = input(SUBSTR(hiv_dx_dt,1,4),4.);
			length hivdxyr 4.; hivdxyr= input(SUBSTR(hiv_aids_dx_dt,1,4),4.); 
			length aidsdxyr 4.; aidsdxyr=input(SUBSTR(aids_dx_dt,1,4), 4.);
			length deathyr 4.; deathyr=input(SUBSTR(dod,1,4),4.);
			length enteryr 4.; enteryr=input(SUBSTR(enter_dt,1,4),4.);
		********************/

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Impute current_gender as birth_sex where missing and update where it was mis-keyed at data entry
		NOTE: according to Frank, this variable stopped being entered at some point unless the case changed gender identity*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			if current_gender="" then current_gender=birth_sex;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
	    /*Create race variables*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length race_eth1 $ 2;
			label race_eth1="Race/ethnicity";
			if race1 in ('R1' 'R2' 'R3' 'R4' 'R5') then race_eth1=race1;
			if race2 NE '' then race_eth1='M';
			if ethnicity1='E1' then race_eth1='E1';
			format race_eth1 $race_eth2Fl.;

			length race_eth $ 2;
			label race_eth="Race/ethnicity (collapsed categories)";
			race_eth=put(race_eth1, race_eth2Fs.);
			format race_eth $race_eth2Fl.;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Calculate age variables*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length current_age 3.;
			label current_age="Current age (i.e., as of 31 DEC %sysevalf(&DUA_yr.-1))";
		    current_age=floor(("31DEC%sysevalf(&DUA_yr.-1)"D - dob_num_MDImp)/365);

			/*used to create age variables (below)--then dropped*/
			hiv_age_yrs1=input(hiv_age_yrs, 3.);
		    aids_age_yrs1=input(aids_age_yrs, 3.);
		    hiv_aids_age_yrs1=input(hiv_aids_age_yrs, 3.);
		    death_age_yrs1=input(death_age_yrs, 3.);

			/********************
				COMMENTED OUT ON 11/06/2013
				REASON: VARS NOT USED
				hiv_aids_age_mos1=input(hiv_aids_age_mos, 3.);
			    death_age_mos1=input(death_age_mos, 3.);	
			********************/

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
	    /*Use defined formats to designate age group according to different grouping schemes (see format for grouping specifications)*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length cur_agegrp1 hiv_agegrp1 aids_agegrp1 $ 2.;
			length cur_agegrp2 hiv_agegrp2 aids_agegrp2 $ 2;

			label 	cur_agegrp1="Current age (as of 31 DEC %sysevalf(&DUA_yr.-1))"
					hiv_agegrp1="Age at HIV diagnosis" 
					aids_agegrp1="Age at AIDS diagnosis"
					hiv_aids_agegrp1="Age at HIV/AIDS diagnosis" 

					cur_agegrp2="Current age (as of 31 DEC %sysevalf(&DUA_yr.-1))"
					hiv_agegrp2="Age at HIV diagnosis" 
					aids_agegrp2="Age at AIDS diagnosis"
					hiv_aids_agegrp2="Age at HIV/AIDS diagnosis";

		    array oldage 	current_age 	hiv_age_yrs1 	aids_age_yrs1 	hiv_aids_age_yrs1;
		    array newage1 	cur_agegrp1 	hiv_agegrp1 	aids_agegrp1 	hiv_aids_agegrp1;
		    array newage2 	cur_agegrp2 	hiv_agegrp2 	aids_agegrp2 	hiv_aids_agegrp2;

		    do over oldage;
				newage1=put(oldage, agegrp1Fs.);
				newage2=put(oldage, agegrp2Fs.);
		    end;

			format  cur_agegrp1 hiv_agegrp1 aids_agegrp1 hiv_aids_agegrp1 $agegrp1Fl.;
			format  cur_agegrp2 hiv_agegrp2 aids_agegrp2 hiv_aids_agegrp2 $agegrp2Fl.;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create collapsed mode of transmission categories*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length mode_trans $ 2;
			label mode_trans='Mode of transmission (collapsed categories)';
			mode_trans=put(trans_categ,trans_categ2Fs.);
			format mode_trans $trans_categ2Fl.;

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create the NewCity var identifying the most recent known city of residence for a case (using the hierarchy: cur->rsa->rsh)*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length NewCity1 $ 36;
			NewCity1=coalescec(cur_city_name, rsa_city_name, rsh_city_name);
			length NewCity $ 2;
			label NewCity="Most recent city of residence";
			NewCity=put(NewCity1,$NewCityFs.);
			format NewCity $NewCityFl.;	
			/********************
				COMMENTED OUT ON 11/05/2013
				REASON: HISTORICAL DEFINITION OF NEWCITY; REPLACED WITH THE CURRENT DEFINITION SINCE THIS DEFINITION FAILED TO REFLECT THE MOST RECENTLY IDENTIFIED 
				ADDRESS FOR A CASE WHEN IT FELL OUTSIDE OF ALAMEDA COUNTY
				if cur_city_name in (&AlCo_cities.) then do; NewCity1=cur_city_name; end; if NewCity1="" and rsa_city_name in (&AlCo_cities.) then do; NewCity1=rsa_city_name; end; if NewCity1="" and rsh_city_name in (&AlCo_cities.) then do; NewCity1=rsh_city_name; end;
			********************/

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create region variables*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length cur_region aids_region hiv_region hiv_aids_region NewRegion $ 2;
			label cur_region="Region of current residence"
				  aids_region="Region of residence at AIDS diagnosis"
				  hiv_region="Region of residence at HIV diagnosis"
				  hiv_aids_region="Region of residence at first HIV diagnoses (HIV or AIDS)"
				  NewRegion="Most recent region of residence";

			array cities 	cur_city_name 	rsa_city_name 	rsh_city_name		NewCity1;
			array regions 	cur_region 		aids_region 	hiv_region			NewRegion;

			do over cities;
				if cities in ('ALBANY', 'BERKELEY') then regions = '23';
				else if cities in('ALAMEDA', 'EMERYVILLE',  'OAKLAND ', 'PIEDMONT') then regions = '24';
				else if cities in ('SAN LEANDRO', 'SAN LORENZO', 'ASHLAND', 'CHERRYLAND', 'FAIRVIEW', 'CASTRO VALLEY', 'HAYWARD') then regions= '25';
				else if cities in ('FREMONT', 'NEWARK', 'UNION CITY', 'SUNOL') then regions = '26';
				else if cities in ('DUBLIN', 'LIVERMORE', 'PLEASANTON') then regions = '27';
				else if not missing(cities) then regions='21';
			end;

			hiv_aids_region=coalescec(hiv_region, aids_region);

			format cur_region aids_region hiv_region hiv_aids_region NewRegion $NewCityFl.;

			/********************
				COMMENTED OUT ON 11/06/2013
				REASON: VAR NOT USED
				length status $ 4; if dx_status in ('1Adult HIV', '3Pediatric HIV') then status = 'HIV'; else if dx_status in ('2Adult AIDS', '4Pediatric AIDS') then status = 'AIDS';	
			********************/

		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
		/*Create filters*/
		/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
			length 	hiv_filter
					aids_filter
					/*Newly_Diag_&year._filter*//*year-specific filters declared, labeled, and defined within a macro loop (below)*/
					PLWH_filter
					PLWA_filter
					all_PLWHA_filter
				3;

			label HIV_filter="Alameda County residents at the time of their first HIV diagnosis (non-AIDS)";
			if status_flag in ('A', 'W') /*status_flag "Active" or "Warning"*/
				and (/*prxmatch("/&AlCo_cities_vbar./",upcase(put(rsh_place,$NewCityFl.))) 
					or missing(rsh_place) and 	*/(
												index(upcase(rsh_county_name),'ALAMEDA') 
												or (upcase(rsh_city_name) in (&AlCo_cities.) and upcase(rsh_state_cd)='CA')
												or rsh_zip_cd in (&AlCo_only_zips.) 
												)
					) /*Alameda County resident at the time of HIV diagnosis*/
				and first_dx='HIV' /*first known dx was HIV*/
				and hiv_categ in ('1', '2') /*definitive or presumptive HIV*/
			then hiv_filter=1; 
			else hiv_filter=0;
			/********************
				COMMENTED OUT ON 01/27/2014
				REASON: DX_STATUS_ENTERED NOT A WELL-DEFINED VARIABLE; ALSO MAY MISS SOME CASES IF ONLY LOOK AT COUNTY_NAME
				label HIV_filter="Alameda County residents at the time of their first HIV diagnosis (non-AIDS)";
				...rsh_county_name='ALAMEDA CO.' and (dx_status_entered in ('1', '4'))... 
					then hiv_filter=1...
			********************/
			/********************
				COMMENTED OUT ON 11/06/2013
				REASON: SUBSETS TO AND SO MISTAKENLY EXCLUDES THOSE WHO HAVE NOT YET PROGRESSED TO AIDS
				...and aids_categ='9' and hivdxyr <= &last_yr.
				then hiv_filter=1...
			********************/

			label aids_filter="Alameda County residents at the time of first HIV diagnosis (AIDS)";
			if status_flag in ('A','W') /*status_flag "Active" or "Warning"*/
				and (/*prxmatch("/&AlCo_cities_vbar./",upcase(put(rsa_place,$NewCityFl.))) 
					or missing(rsa_place) and 	*/(
												index(upcase(rsa_county_name),'ALAMEDA')
												or (upcase(rsa_city_name) in (&AlCo_cities.) and upcase(rsa_state_cd)='CA')
												or rsa_zip_cd in (&AlCo_only_zips.)
												)
					) /*Alameda County resident at the time of AIDS diagnosis*/
				and first_dx='AIDS' /*first known dx was AIDS*/
				and aids_categ in ('7', 'A') /*AIDS cases defined by immunologic test or clinical disease*/
			then aids_filter=1; 
			else aids_filter=0;
			/********************
				COMMENTED OUT ON 01/27/2014
				REASON: DX_STATUS_ENTERED NOT A WELL-DEFINED VARIABLE; ALSO MAY MISS SOME CASES IF ONLY LOOK AT COUNTY_NAME
				...and rsa_county_name='ALAMEDA CO.' and (dx_status_entered in ('2', '5'))...
				...then aids_filter=1...
			********************/

			/********************
				COMMENTED OUT ON 11/06/2013
				REASON: UNNECESSARY
				label HIV_All_filter="Alameda County residents at the time of diagnosis with HIV or AIDS from 2006 through the end of the last year";
				if (hiv_filter=1 or aids_filter=1) 
					and hivdxyr >= 2006 
				then HIV_All_filter=1; 
				else HIV_All_filter=0;
			********************/

			%macro create_newly_diag_filters;
				%do year=2007 %to %sysevalF(&DUA_yr.-2);
					length Newly_Diag_&year._filter 3;
					label Newly_Diag_&year._filter="Cases diagnosed with HIV(non-AIDS)/AIDS in &year.(+/1 1 yr - for 3-yr avg.)";
					if (HIV_filter=1 or AIDS_filter=1)
						and ((&year. - 1) <= year(hiv_aids_dx_dt_num_MDImp)  <= (&year. + 1))
					then Newly_Diag_&year._filter=1; 
					else Newly_Diag_&year._filter=0;
				%end;
			%mend; %create_newly_diag_filters;
			/********************
				COMMENTED OUT ON 11/08/2013
				REASON: REFERS TO AN UNNECESSARY FILTER--HIV_all--THAT HAS SINCE BEEN COMMENTED OUT
				%do year=2006 %to &last_yr.;
					length Newly_Diag_&year._filter $ 1;
					label Newly_Diag_&year._filter="Cases diagnosed with HIV in &year.(+/1 1 yr - for 3-yr avg.)";
					if HIV_ALL_filter=1
						and hivdxyr <= (&year. + 1)
						and hivdxyr >= (&year. - 1)
					then Newly_Diag_&year._filter=1; 
					else Newly_Diag_&year._filter=0;
				%end;
			********************/
			/********************
				COMMENTED OUT ON 09/??/2013
				REASON: NO LONGER NEEDED AS NOW CREATE THE ABOVE, YEAR-SPECIFIC FILTERS FOR THE PURPOSE OF LOOKING AT TRENDS
				if HIV_ALL_filter=1 
					and ((&last_yr.-2) <= hivdxyr <= &last_yr.) 
				then Newly_Diag_filter=1; 
				else Newly_Diag_filter=0;
			********************/

			label PLWH_filter="People living (in Alameda County) with HIV (non-AIDS) as of the end of the last year";
			if status_flag in ('A', 'W') /*status_flag "Active" or "Warning"*/
				and year(hiv_dx_dt_num_MDImp) <= %sysevalF(&DUA_yr.-1) /*diagnosed in or before the last year*/
				and (missing(aids_dx_dt_num_MDImp) or year(aids_dx_dt_num_MDImp) > %sysevalF(&DUA_yr.-1)) /*hadn't progressed to AIDS as of the end of the last year*/
				and hiv_categ in ('1', '2') /*definitive or presumptive HIV*/
				and (
					vital_status NE '2' 
					or vital_status='2' and year(dod_num_MDImp) > %sysevalF(&DUA_yr.-1)
					) /*alive as of the end of the last year*/
				and rec_addr_AlCo=1 /*most recent known county or city of residence in Alameda County*/
			then PLWH_filter=1; 
			else PLWH_filter=0;
			/********************
				COMMENTED OUT ON 01/27/2014
				REASON: MAY MISS SOME CASES IF ONLY LOOK AT CITY_NAMES
				...and (coalescec(cur_city_name, rsa_city_name, rsh_city_name) in (&AlCo_cities.)) 
				then PLWH_filter=1...
			********************/
			/********************
				COMMENTED OUT ON 11/05/2013
				REASON: REPLACED WITH THE ABOVE DEFINITION AS THE BELOW MISTAKENLY INCLUDES PEOPLE WHOSE DIAGNOSIS WAS IN THIS JURISDICTION BUT WHO WERE KNOWN TO 
				HAVE MOVED OUT OF IT AND, ADDITIONALLY, OMITS CASES WHOSE DIAGNOSIS WAS ELSEWHERE BUT WHO HAVE SINCE MOVED TO OUR JURISDICTION
				if hiv_filter=1 and (vital_status NE '2' or (vital_status='2' and deathyr > &last_yr.)) then PLWH_filter=1; else PLWH_filter=0;
			********************/

			label PLWA_filter="People living (in Alameda County) with AIDS as of the end of the last year";
			if status_flag in ('A','W') /*status_flag "Active" or "Warning"*/
				and year(aids_dx_dt_num_MDImp) <= %sysevalF(&DUA_yr.-1) /*diagnosed in or before the last year*/
				and dx_status in ('2', '5') /*adult or pediatric AIDS*/
				and aids_categ in ('7', 'A') /*AIDS cases defined by immunologic test or clinical disease*/
				and (
					vital_status NE '2' 
					or vital_status='2' and year(dod_num_MDImp) > %sysevalF(&DUA_yr.-1)
					) /*alive as of the end of the last year*/
				and rec_addr_AlCo=1 /*most recent known county or city of residence in Alameda County*/
			then PLWA_filter=1; 
			else PLWA_filter=0;
			/********************
				COMMENTED OUT ON 01/27/2014
				REASON: MAY MISS SOME CASES IF ONLY LOOK AT CITY_NAMES
				...and (coalescec(cur_city_name, rsa_city_name, rsh_city_name) in (&AlCo_cities.)) 
				then PLWA_filter=1...
			********************/
			/********************
				COMMENTED OUT ON 11/05/2013
				REASON: REPLACED WITH THE ABOVE DEFINITION AS THE BELOW MISTAKENLY INCLUDES PEOPLE WHOSE DIAGNOSIS WAS IN THIS JURISDICTION BUT WHO WERE KNOWN TO 
				HAVE MOVED OUT OF IT AND, ADDITIONALLY, OMITS CASES WHOSE DIAGNOSIS WAS ELSEWHERE BUT WHO HAS SINCE MOVED TO OUR JURISDICTION
				if aids_filter=1 
					and dxyear <=&last_yr. 
					and (vital_status NE '2' or (vital_status='2' and deathyr > &last_yr.)) 
				then PLWA_filter=1...
			********************/

			label ALL_PLWHA_filter="People living (in Alameda County) with HIV (non-AIDS) or AIDS as of the end of the last year";
			if PLWA_filter=1 or PLWH_filter=1 
			then ALL_PLWHA_filter=1; 
			else ALL_PLWHA_filter=0;
			/********************
				COMMENTED OUT ON 11/06/2013
				REASON: UNUSED
				if (aids_filter=1 or hiv_filter=1) 
					and (current_gender in ('MF','FM'))
				then Trans_filter=1; 
				else Trans_filter=0;
			********************/

	run;

/*######################################################################################################################################################*/
/*Check variable construction*/
/*######################################################################################################################################################*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_date_conv_to_num;
		Proc sql;
				select 	name,
						trim(name)||'_num',
						trim(name)||'_num_DImp',
						trim(name)||'_num_MDImp'
					into 	:dt_var_char1-:dt_var_char87, 
							:dt_var_num1-:dt_var_num87,
							:dt_var_num_DImp1-:dt_var_num_DImp87,
							:dt_var_num_MDImp1-:dt_var_num_MDImp87
				from dictionary.columns
				where (index(name,"_dt") NE 0 or name in ('dob' 'dod')) and type='char' and libname="WORK" and memname="ALL_DEDUP";

		ods _all_ close;
			ods TAGSETS.EXCELXP 
				file="S:\DCDCP\HIV-AIDS Analysis\SAS code and output\Output\&sysdate._dt_conversion.xls" 
				style=statistical
				options(/*contents='yes' */
					embedded_titles='yes' 
					embedded_footnotes='yes'
					EMBED_TITLES_ONCE='yes'
					EMBED_FOOTERS_ONCE='yes'
					sheet_interval="table"
					AUTOFILTER='ALL' /*NONE*/);

				%do i=1 %to 87;
					ods tagsets.ExcelXP options (sheet_interval="none" sheet_name="&&dt_var_char&i.");

					Proc sql;
							select distinct &&dt_var_char&i.,
											&&dt_var_num&i. format=YYMMDD6.,
											&&dt_var_num_DImp&i. format=YYMMDD6.,
											&&dt_var_num_MDImp&i. format=YYMMDD6.
							from all_dedup_recode
							order by &&dt_var_num&i., &&dt_var_num_DImp&i., &&dt_var_num_MDImp&i.;
				%end;

		ods _all_ close;
		ods listing;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_first_dx;
	Proc sql;
		select distinct hiv_dx_dt, AIDS_dx_dt,
						/*hiv_dx_dt_num format=date9., AIDS_dx_dt_num format=date9.,*/ AIDS_dx_dt_num-HIV_dx_dt_num as days,
						/*hiv_dx_dt_num_DImp format=date9., AIDS_dx_dt_num_DImp format=date9.,*/ intck('MONTH', hiv_dx_dt_num_DImp, aids_dx_dt_num_DImp) as months, 
						/*hiv_dx_dt_num_MDImp format=date9., AIDS_dx_dt_num_MDImp format=date9.,*/ year(aids_dx_dt_num_MDImp) - year(hiv_dx_dt_num_MDImp) as years,
						first_dx
		from new_filters
		order by first_dx, years, months, days;

		select distinct hiv_dx_dt, AIDS_dx_dt,
						/*hiv_dx_dt_num format=date9., AIDS_dx_dt_num format=date9.,*/ AIDS_dx_dt_num-HIV_dx_dt_num as days,
						/*hiv_dx_dt_num_DImp format=date9., AIDS_dx_dt_num_DImp format=date9.,*/ intck('MONTH', hiv_dx_dt_num_DImp, aids_dx_dt_num_DImp) as months, 
						/*hiv_dx_dt_num_MDImp format=date9., AIDS_dx_dt_num_MDImp format=date9.,*/ year(aids_dx_dt_num_MDImp) - year(hiv_dx_dt_num_MDImp) as years,
						first_dx
		from new_filters
		where missing(AIDS_dx_dt_num-HIV_dx_dt_num) and intck('MONTH', hiv_dx_dt_num_DImp, aids_dx_dt_num_DImp)=1
		order by first_dx, years, months, days; /*269 such borderline cases*/
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_rec_addr_AlCo;
	Proc sql;
		select distinct rec_addr_AlCo, rec_addr_type, 		
				cur_place, cur_county_name, cur_city_name, cur_state_cd, cur_zip_cd, 
				rsa_place, rsa_county_name, rsa_city_name, rsa_state_cd, rsa_zip_cd, 
				rsh_place, rsh_county_name, rsh_city_name, rsh_state_cd, rsh_zip_cd, 
				stateno
		from new_filters
		order by 	cur_place, cur_county_name, cur_city_name, cur_state_cd, cur_zip_cd, 
					rsa_place, rsa_county_name, rsa_city_name, rsa_state_cd, rsa_zip_cd, 
					rsh_place, rsh_county_name, rsh_city_name, rsh_state_cd, rsh_zip_cd;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_race_eth;
	Proc sql;
		create table check_race_eth1 as
			select distinct race2, race1, ethnicity1, race_eth1
			from all_dedup_recode
			order by race_eth1, ethnicity1, race1, race2;

		create table check_race_eth as
			select distinct race_eth1, race_eth
			from all_dedup_recode
			order by race_eth, race_eth1;

	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_age_yrs;
	Proc sql;
		create table check_current_age as
			select distinct dob, input(substr(DOB,1,4),4.) as yob, current_age
			from all_dedup_recode
			order by current_age;

		create table check_hiv_age_yrs1 as
			select distinct input(hiv_age_yrs, 3.) as aah, hiv_age_yrs1
			from all_dedup_recode
			order by hiv_age_yrs1;

		create table check_aids_age_yrs1 as
			select distinct input(aids_age_yrs, 3.) as aaa, aids_age_yrs1
			from all_dedup_recode
			order by aids_age_yrs1;

		create table check_hiv_aids_age_yrs1 as
			select distinct input(hiv_aids_age_yrs, 3.) as aaha, hiv_aids_age_yrs1
			from all_dedup_recode
			order by hiv_aids_age_yrs1;
			
		create table check_death_age_yrs1 as
			select distinct input(death_age_yrs, 3.) as aad, death_age_yrs1
			from all_dedup_recode
			order by death_age_yrs1;

	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_agegrps;
	Proc sql;
		create table check_cur_agegrp as
			select distinct current_age, cur_agegrp1, cur_agegrp2
			from all_dedup_recode
			order by current_age, cur_agegrp1, cur_agegrp2;

		create table check_hiv_agegrp as
			select distinct hiv_age_yrs1, hiv_agegrp1, hiv_agegrp2
			from all_dedup_recode
			order by hiv_age_yrs1, hiv_agegrp1, hiv_agegrp2;

		create table check_aids_agegrp as
			select distinct aids_age_yrs1, aids_agegrp1, aids_agegrp2
			from all_dedup_recode
			order by aids_age_yrs1, aids_agegrp1, aids_agegrp2;

		create table check_hiv_aids_agegrp as
			select distinct hiv_aids_age_yrs1, hiv_aids_agegrp1, hiv_aids_agegrp2
			from all_dedup_recode
			order by hiv_aids_age_yrs1, hiv_aids_agegrp1, hiv_aids_agegrp2;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_mode_trans;
	Proc sql;
		create table check_mode_trans as
			select distinct trans_categ, mode_trans
			from all_dedup_recode
			order by trans_categ, mode_trans;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_newcity;
	Proc sql;
		create table check_newcity1 as
			select distinct rsh_city_name, 
							rsa_city_name, 
							cur_city_name, 
							NewCity1
			from all_dedup_recode
			order by 	
						cur_city_name,
						rsa_city_name, 
						rsh_city_name;

		create table check_newcity as
			select distinct NewCity1, 
							NewCity
			from all_dedup_recode
			order by 	NewCity, 
						NewCity1;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_regions;
	Proc sql;
		create table check_cur_region as
			select distinct cur_city_name, 
							cur_region
			from all_dedup_recode
			order by 	cur_region, 
						cur_city_name;

		create table check_aids_region as
			select distinct rsa_city_name, 
							aids_region
			from all_dedup_recode
			order by 	aids_region, 
						rsa_city_name;

		create table check_hiv_region as
			select distinct rsh_city_name, 
							hiv_region
			from all_dedup_recode
			order by 	hiv_region, 
						rsh_city_name;

		create table check_Newregion as
			select distinct NewCity1, 
							NewRegion
			from all_dedup_recode
			order by 	NewRegion, 
						NewCity1;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_HIV_filter;
	Proc sql;
		select status_flag, 
				rsh_place, 
				rsh_county_name, rsh_city_name, rsh_state_cd, rsh_zip_cd, 
				first_dx, 
				HIV_categ, 
				hiv_filter, 
				stateno
		from new_filters;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_AIDS_filter;
	Proc sql;
		select status_flag, 
				rsa_place, 
				rsa_county_name, 
				rsa_city_name, rsa_state_cd, rsa_zip_cd, 
				first_dx, 
				AIDS_categ, 
				aids_filter, 
				stateno
		from new_filters;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_Newly_Diag_2010_filter;
	Proc sql;
		create table check_Newly_Diag_2010_filter as
			select distinct HIV_filter, 
							year(hiv_dx_dt_num_MDImp) as hivdxyr, 
							AIDS_filter, 
							year(aids_dx_dt_num_MDImp) as aidsdxyr, 
							year(hiv_aids_dx_dt_num_MDImp) as hiv_aids_dxyr,
							dx_status_entered,
							Newly_Diag_2010_filter
			from all_dedup_recode;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_PLWH_filter;
	Proc sql;
		select distinct status_flag, 
						aids_dx_dt_num_MDImp format=date9., 
						HIV_categ, 
						hiv_dx_dt_num_MDImp format=date9., 
						vital_status, 
						dod_num_MDImp format=date9., 
						rec_addr_AlCo, 
						PLWH_filter
		from new_filters
		order by aids_dx_dt_num_MDImp, hiv_dx_dt_num_MDImp, dod_num_MDImp;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_PLWA_filter;
	Proc sql;
		select distinct status_flag, 
						aids_dx_dt_num_MDImp format=date9., 
						dx_status, 
						aids_categ, 
						vital_status, 
						dod_num_MDImp format=date9., 
						rec_addr_AlCo, 
						PLWA_filter
		from new_filters
		order by aids_dx_dt_num_MDImp, dod_num_MDImp;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_all_PLWHA_filter;
	Proc sql;
		create table check_all_PLWHA_filter as
			select distinct PLWH_filter, PLWA_filter, all_PLWHA_filter
			from all_dedup_recode;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/
	/*%-%-%*/%macro check_hiv_aids_dx_lag;
	Proc sql;
		create table check_hiv_aids_dx_lag as
			select distinct hiv_aids_dx_dt_num_MDImp format=mmddyy8., aids_dx_dt_num_MDImp format=mmddyy8., hiv_aids_dx_lag
			from all_dedup_recode;
	/*%-%-%*/%mend;
	/*%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%-%*/

/*	%check_date_conv_to_num; run; *last checked 11/25/2013;*/
	/*%check_age_yrs; run; *last checked 11/06/2013;*/
	/*%check_race_eth; run; *last checked 11/06/2013;*/
	/*%check_agegrps; run; *last checked 11/06/2013;*/
	/*%check_mode_trans; run; *last checked 11/06/2013;*/
	/*%check_newcity; run; *last checked 11/06/2013;*/
	/*%check_regions; run; *last checked 11/06/2013;*/
	/*%check_hiv_aids_dx_lag; run; *last check 11/8/2013;*/

	/*ods _all_ close;*/
/*	ods _all_ close;*/
/*	ods TAGSETS.EXCELXP */
/*		file="&Output_path.\&today_date._check_filters.xls" */
/*		style=statistical*/
/*		options(*/
/*			embedded_titles='yes' */
/*			embedded_footnotes='yes'*/
/*			EMBED_TITLES_ONCE='yes'*/
/*			EMBED_FOOTERS_ONCE='yes'*/
/*			sheet_interval="none"*/
/*			AUTOFILTER='ALL' */
/*			);*/

/*		ods tagsets.ExcelXP options (sheet_interval="none" sheet_name="HIV");*/
/*		%check_HIV_filter; run;*/
/*		proc print data=check_HIV_filter; run;*/
/*	*/
/*		ods tagsets.ExcelXP options (sheet_interval="none" sheet_name="AIDS");*/
/*		%check_AIDS_filter; run;*/
/*		proc print data=check_AIDS_filter; run;*/
/*	*/
	/*	ods tagsets.ExcelXP options (sheet_interval="none" sheet_name="New_dx_2010");*/
	/*	%check_Newly_Diag_2010_filter; run;*/
	/*	proc print data=check_Newly_Diag_2010_filter; run;*/
	/**/
	/*	ods tagsets.ExcelXP options (sheet_interval="none" sheet_name="PLWH");*/
	/*	%check_PLWH_filter; run;*/
	/*	proc print data=check_PLWH_filter; run;*/
	/**/
	/*	ods tagsets.ExcelXP options (sheet_interval="none" sheet_name="PLWA");*/
	/*	%check_PLWA_filter; run;*/
	/*	proc print data=check_PLWA_filter; run;*/
	/**/
/*	ods _all_ close;*/
/*	ods listing;*/