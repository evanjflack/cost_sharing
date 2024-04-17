/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Reads in and formats part D plan information
*/

* Librefs;
%include "../sas_librefs.sas";

/*
Macro: read_in_pln
Desc: see script desc
Arg:
    pct: sample percentage
    year: calendar year
Ret: Formatted pln file
*/
%macro read_in_pln(pct, year);
  data pln_&year._&pct;
    set "/disk/aging/partd/&pct/pln/&year/pln&year"
    (keep = contract_id plan_id ded_apply ded_amt icl_amt oopt_amt plan_type
    drug_benefit_type snp_type  pre_icl_apply post_oopt_apply gap_coverage_type);
    rfrnc_yr = &year;
  run;

  %let file_name = pln_&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  proc export
    data= pln_&year._&pct dbms=csv
    outfile="&file_path"
    replace;
  run;

%mend;

/*
Macro: loop_years
Desc: Loops over read_in_pln macro
Arg:
    pct: sample percentage
    first_year: first calendar year
    last_year: last calcendar year
*/
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %read_in_pln(&pct, &year);
  %end;
%mend;

* always want to read in 20pct (largest) file and all years on dua;
%loop_years(20pct, 2006, 2013);
