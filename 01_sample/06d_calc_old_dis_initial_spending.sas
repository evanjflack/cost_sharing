/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Calculates Jan-Mar spending for individuals in the falsification samples
 */

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2007;
%let last_year = 2012;

%let file_name = old_false_sample_benes_&pct..csv;
%let file_path = &exp_dir.&file_name.;

proc import datafile = "&file_path"
        out = old_benes
        dbms=csv
        replace;
run;

%macro subset_old_claims(pct, year);

  * subset list of benes to year on hand;
  data old_benes1;
    set old_benes;
    if rfrnc_yr = &year;
  run;

  * Sort bene list/pde by bene_id;
  proc sort data = old_benes1;
    by bene_id;
  run;

  proc sort data = pde.pde_&year._&pct;
    by bene_id;
  run;

  * Merge pde with bene list to subset pde, also subset to claims in Jan-Mar;
  data em.old_false_init_cost_&year._&pct (drop = srvc_dt srvc_mo);
    merge pde.pde_&year._&pct (keep = bene_id srvc_mo srvc_dt totalcst in = pde) old_benes1 (in = bsf);
    by bene_id;
    if pde & bsf;
    if srvc_mo <= 3;
    rfrnc_yr = &year;
  run;

  proc sort data = em.old_false_init_cost_&year._&pct;
    by bene_id;
  run;

  data em.old_false_init_cost_&year._&pct;
    set em.old_false_init_cost_&year._&pct;
    by bene_id;
    if first.bene_id then initial_cost3 = 0;
    initial_cost3 + totalcst;
  run;

  data em.old_false_init_cost_&year._&pct (keep = bene_id rfrnc_yr initial_cost3);
    set em.old_false_init_cost_&year._&pct;
    by bene_id;
    if last.bene_id;
  run;


  %let file_name = old_false_init_cost_&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  proc export
    data = em.old_false_init_cost_&year._&pct dbms=csv
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
    %subset_old_claims(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
