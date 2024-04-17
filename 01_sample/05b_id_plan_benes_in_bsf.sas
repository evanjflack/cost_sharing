/*
Proj: Behavioral Hazard
Author: Evan Flack (evanjflack@gmail.com)
Desc: Identifies all beneficaries in plans that individuals in the 65 year old
      sa,mples (non-dual and dual) are in, so next we can use their pde claims
      to calculate plan-level cost sharing measures.
 */

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2007;
%let last_year = 2012;

* List of plans in dual/non-dual samples;
%let file_name = new_enrollee_plans_&pct..csv;
%let file_path = &exp_dir.&file_name.;

proc import datafile = "&file_path"
        out = plans
        dbms=csv
        replace;
run;

/*
Macro: subset_plan_benes
Desc: Identifies beneficaries that were in a plan that somsone in the analytic/
      falsificiation sample was in.
Arg:
  pct: sample percentage
  year: sample year
Out:
  em.plan_bsf_&year._&pct.csv: dataset with bene_id, cntrct, pbp, and rfrnc_yr
 */
%macro subset_plan_benes(pct, year);
  %if &year <= 2012 %then %do;

    data plans1;
      set plans;
      if rfrnc_yr = &year;
    run;

    proc sort data = plans1;
      by cntrct pbp;
    run;

    data em.plan_bsf_&year._&pct;
      set bsf.bsf_&year._&pct (keep = bene_id age1 cntrct12 pbpid12);
      rename cntrct12 = cntrct
             pbpid12 = pbp;
    run;

    proc sort data = em.plan_bsf_&year._&pct;
      by cntrct pbp;
    run;

    data em.plan_bsf_&year._&pct;
      merge em.plan_bsf_&year._&pct (in = bsf) plans1 (in = pln);
      by cntrct pbp;
      if bsf & pln;
      if age1 >= 66;
    run;

    data em.plan_bsf_&year._&pct;
      set em.plan_bsf_&year._&pct;
    run;

  %end;
  %else %do;
  * Different plan format for 2013;
    data plans1;
      set plans;
      if rfrnc_yr = &year;
      pbp1 = put(input(pbp, best.), z3.);
      drop pbp;
      rename pbp1 = pbp;
    run;

    proc sort data = plans1;
      by cntrct pbp;
    run;

    data em.plan_bsf_&year._&pct;
      set bsf.bsf_&year._&pct (keep = bene_id age1 cntrct12 pbpid12);
      rename cntrct12 = cntrct
             pbpid12 = pbp;
    run;

    data em.plan_bsf_&year._&pct;
      set em.plan_bsf_&year._&pct;
      pbp1 = put(input(pbp, best3.), z3.);
      drop pbp;
      rename pbp1 = pbp;
    run;

    proc sort data = em.plan_bsf_&year._&pct;
      by cntrct pbp;
    run;

    data em.plan_bsf_&year._&pct;
      merge em.plan_bsf_&year._&pct (in = bsf) plans1 (in = pln);
      by cntrct pbp;
      if bsf & pln;
      rfrnc_yr = &year;
    run;
  %end;


  %let file_name = plan_bsf_&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  proc export
    data = em.plan_bsf_&year._&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;

/*
Macro: loop_years
Desc: loops over subset_plan_benes
Arg:
  pct
  first_year
  last_year
*/
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %subset_plan_benes(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
