/*concatenate Berkeley and AC files*/
data all_AC; 
	set Q&DUA_qtr._&DUA_yr..Q&DUA_qtr._&DUA_yr._person 
		Q&DUA_qtr._&DUA_yr..Q&DUA_qtr._&DUA_yr._berkeley_person; 

	%hardcoded_changes_to_DUAs;
run;

proc sort data=all_AC out=all_dedup nodupkey;
	by cur_street_address1 stateno dob last_name;
run;
