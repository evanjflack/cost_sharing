/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Merges parts a-b and part d bsf files, chooses which variables to keep,
      and defines/formats additional variables
*/

* Librefs;
%include "../sas_librefs.sas";

%let first_year = 2006;
%let last_year = 2013;

/*
Macro: read_in_bsf
Desc: see script desc
Arg:
    pct: sample percentage
    year: calendar year
Ret: Merged and formatted a, b, d bsf file
*/
%macro read_in_bsf(pct, year);

  * Parts A + B;
  data bsfab_&year._&pct;
    set "/disk/agedisk3/medicare/data/&pct/bsf/&year/1/bsfab&year"
      (keep = bene_id rfrnc_yr age sex race bene_dob bene_zip state_cd death_dt
              orec crec);
    * Format date variables;
    birth_mo = month(bene_dob);
    format bene_dob MMDDYYS10.;
    format death_dt MMDDYYS10.;
    birth_yr = year(bene_dob);
    * Define age variable for what the bene would will turn in this year;
    age1 = rfrnc_yr - birth_yr;
  run;

  * Sort on bene_id;
  proc sort data = bsfab_&year._&pct;
    by bene_id;
  run;

  * Part d;
  data bsfd_&year._&pct;
    set "/disk/agedisk3/medicare/data/&pct/bsf/&year/1/bsfd&year"
    (keep = bene_id cntrct01--cntrct12 pbpid01--pbpid12
            cstshr01--cstshr12 dual_01-dual_12 rdsind01--rdsind12);
  run;

  * Sort on bene_id;
  proc sort data = bsfd_&year._&pct;
    by bene_id;
  run;

  * Merge par a-b and d;
  data bsf.bsf_&year._&pct;
    merge bsfab_&year._&pct bsfd_&year._&pct;
    by bene_id;
  run;

%mend;

/*
Macro: loop_years
Desc: Loops over read_in_bsf macro
Arg:
    pct: sample percentage
    first_year: first calendar year
    last_year: last calcendar year
*/
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %read_in_bsf(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
