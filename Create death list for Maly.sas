/*######################################################################################################################################################*/
/*Create a listing of deaths based on the DUA and output to Maly's folder*/
/*######################################################################################################################################################*/

	data temp;
		set Q&latest_DUA_qtr._&latest_DUA_yr..AlCo_Q&latest_DUA_qtr._&latest_DUA_yr.;

		/*Add fields indicating whether they were diagnosed with HIV in AlCo, 
		whether they were diagnosed with AIDS in AlCo*/
		if index(upcase(rsh_county_name),'ALAMEDA') 
			or upcase(rsh_city_name) in (&AlCo_cities.) and upcase(rsh_state_cd)='CA'
			or rsh_zip_cd in (&AlCo_zips_all.)
			then rsh_AlCo="Y";
			else rsh_AlCo="N";

		if index(upcase(rsa_county_name),'ALAMEDA') 
			or upcase(rsa_city_name) in (&AlCo_cities.) and upcase(rsa_state_cd)='CA'
			or rsa_zip_cd in (&AlCo_zips_all.)
			then rsa_AlCo="Y";
			else rsa_AlCo="N";
	run;

	/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
	/*Check coding*/
	/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/

		*Proc sql;
			*create table check_rsh_AlCo as
				select distinct rsh_Alco,
					rsh_county_name, rsh_city_name,
					case 
						when rsh_zip_cd in (&AlCo_zips_all.) then 1
						else 0
						end as rsh_zip_in
				from temp
				order by rsh_AlCo, rsh_county_name, rsh_city_name, rsh_zip_in;

			*create table check_rsa_AlCo as
				select distinct rsa_Alco,
					rsa_county_name, rsa_city_name,
					case 
						when rsa_zip_cd in (&AlCo_zips_all.) then 1
						else 0
						end as rsa_zip_in
				from temp
				order by rsa_AlCo, rsa_county_name, rsa_city_name, rsa_zip_in;

	/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/
	/*Extract desired fields and export to Maly Surviellance*/
	/*><=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<=>=<*/

	Proc sql;
		create table Q&latest_DUA_qtr._&latest_DUA_yr._deaths as
			select last_name, first_name, stateno, ssn, 
					dob_num format=date9., birth_sex, dod_num format=date9., 
					rsh_AlCo as HIV_dx_AlCo, rsa_AlCo as AIDS_dx_AlCo
			from temp
			where not missing(dod)
			order by last_name, first_name, stateno, ssn, dob, birth_sex, dod, rsh_AlCo, rsa_AlCo;

	proc export 
		data=Q&latest_DUA_qtr._&latest_DUA_yr._deaths 
		dbms=csv 
		file="S:\All OAA\Program Files\Epi-Surveillance\Maly_Surveillance\Q&latest_DUA_qtr._&latest_DUA_yr._deaths.csv" 
		replace; 
	run;