/*Create lookup dataste for Wei and export to CSV file*/
data Wei_Q&Q._&this_yr._lookup; set all_dedup;
	KEEP stateno		rsh_county_name		rsa_county_name		aids_dx_dt		hiv_aids_dx_dt		dob	
		 birth_sex		current_gender		race				dx_status		dx_status_entered	vital_status	
		ssn	ssn_alias	last_name			first_name			middle_name		last_name_sndx		first_name_sndx	
		status_flag;
run;

proc export 
	data=Wei_Q&Q._&this_yr._lookup
	outfile="\\acphd.ph.local\ph\AppProd\DCDCP - HIV with NAME\DUA\Wei_Q&Q._&this_yr._lookup.csv"
	DBMS=csv
	REPLACE;
run;