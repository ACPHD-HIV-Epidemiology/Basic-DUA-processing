data AlCo_Q&DUA_qtr._&DUA_yr.; 
	set all_dedup_recode; 
	drop 	aids_age_yrs1
	        hiv_age_yrs1
	        hiv_aids_age_yrs1
	        death_age_yrs1
	        NewCity1;
run;

%replace_dataset(	lib=Q&DUA_qtr._&DUA_yr. /*library in which the replacee resides*/,
					dataset=AlCo_Q&DUA_qtr._&DUA_yr. /*the dataset to be archived and replaced*/,
					old_label= /*label for the replacee (ideally, should document how it differs from its successor*/
					new_label= /*label for the replacer (ideally, should document how it differs from its predecessor*/);