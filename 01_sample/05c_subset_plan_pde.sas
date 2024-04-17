/*
Proj: Behavioral Hazard
Author: Evan Flack (evanjflack@gmail.com)
Desc: Identifies pde claims that were made by people in plans that someone in
      the analytic/falsification sample was in, so next we can calculate plan-
      level costsharing measures.
*/

* librefs;
%include "../sas_librefs.sas";


%let first_year = 2007;
%let last_year = 2012;

/*
Macro: subset_claims_to_sample
Desc: Subsets pde claims that were made by people in plans that someone in
      the analytic/falsification sample was in.
Arg:
  pct: sample percentage
  year: sample year
Out:
  em.plan_pde_&year._&pct.csv: pde caims for people in sample plans
*/
%macro subset_claims_to_sample(pct, year);

  data plans (keep = bene_id);
    set em.plan_bsf_&year._&pct;
    if rfrnc_yr = &year;
  run;

  proc sort data = plans;
    by bene_id;
  run;

  proc sort data = pde.pde_&year._&pct;
    by bene_id;
  run;

  data em.plan_pde_&year._&pct (keep = bene_id lab_prod bnftphas totalcst
                                ptpayamt othtroop lics_amt plro_amt drcvstcd);
    merge pde.pde_&year._&pct (in = pde) plans (in = bsf);
    by bene_id;
    if pde & bsf;
  run;


  %let file_name = plan_pde_&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  proc export
    data = em.plan_pde_&year._&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;

/*
Macro: loop_years
Desc: loops over subset_claims_to_sample
Arg:
  pct
  first_year
  last_year
*/
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %subset_claims_to_sample(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
