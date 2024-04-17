/*
Proj: Cost-Sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Subsets parts A, D medicare claims to new enrollees identified in
      01a_id_new_enrollees.
 */

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2007;
%let last_year = 2013;

/*
Macro: subset_claims_to_sample
Desc: Subsets parts A-D medicare claims to new enrollees identified in
  01a_id_new_enrollees, by performing inner merges on claims files and list
  of sample beneficaries.
Arg:
  pct: sample percentage
  year: sample year
  type: type of claims (bsf, pde, ip, op, car)
Out:
  sample_&type._&year._&pct: subsetted claims file
*/
%macro subset_claims_to_sample(pct, year, type);

  * Beneficary Summary File (BSF);
  %if &type = bsf %then %do;
    data em.sample_bsf_&year._&pct;
      merge bsf.bsf_&year._&pct (in = bsf) em.pde_benes_&pct (in = bsf1);
      by bene_id;
      if bsf & bsf1;
    run;
  %end;

  * Part D Event (PDE);
  %if &type = pde %then %do;
    data em.sample_pde_&year._&pct;
      merge pde.pde_&year._&pct (in = pde) em.pde_benes_&pct (in = bsf);
      by bene_id;
      if pde & bsf;
    run;
  %end;

  * Inpatient (IP);
  %if &type = ip %then %do;
    data em.sample_ip_&year._&pct;
      merge ip.ip_&year._&pct (in = ip) em.pde_benes_&pct (in = bsf);
      by bene_id;
      if ip & bsf;
    run;
  %end;

  %let file_name = sample_&type._&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  proc export
    data = em.sample_&type._&year._&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;

/*
Macro: loop_years
Desc: loops over subset_claims_to_sample by calendar year
Arg:
  type
  pct
  first_year
  last_year
Out:
  output from subset_claims_to_sample
*/
%macro loop_years(pct, first_year, last_year, type);
  %do year = &first_year %to &last_year;
    %subset_claims_to_sample(&pct, &year, &type);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year, bsf);

%loop_years(&pct, &first_year, &last_year, pde);

%loop_years(&pct, &first_year, &last_year, ip);
