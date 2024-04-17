/* -----------------------------------------------------------------------------
Proj: Cost-ssharing
Author: Evan Flack (evanjflack@gmail.com)
Desc: Subsets Parts A-D medicare claims to dual eligible benes identified in
      01b_id_dual_benes (for)
----------------------------------------------------------------------------- */

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2007;
%let last_year = 2013;

data dual_benes;
  set em.dual_benes_&pct (keep = bene_id);
run;

* For merging, only keep list of unique beneficares (we want all of their claims);
proc sort data = dual_benes nodupkey;
  by bene_id;
run;

/* -----------------------------------------------------------------------------
Macro: subset_claims_to_sample
Desc: Subsets parts a-d medicare claims to new enrollees identified in
      01b_id_dual_benes
Arg:
  pct: sample percentage
  year: sample year
  type: type of claims (bsf, pde, ip, op, car)
Out:
  dual_&type._&year._&pct: subsetted claims file
----------------------------------------------------------------------------- */
%macro subset_claims_to_sample(pct, year, type);

  * Part D Event (PDE);
  %if &type = pde %then %do;
    data em.dual_pde_&year._&pct;
      merge pde.pde_&year._&pct (in = pde) dual_benes (in = bsf);
      by bene_id;
      if pde & bsf;
    run;
  %end;

  * Inpatient (IP);
  %if &type = ip %then %do;
    data em.dual_ip_&year._&pct;
      merge ip.ip_&year._&pct (in = ip) dual_benes (in = bsf);
      by bene_id;
      if ip & bsf;
    run;
  %end;


  %let file_name = dual_&type._&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;
  * Export to CSV;
  proc export
    data = em.dual_&type._&year._&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;
/* -----------------------------------------------------------------------------
Macro: loop_and_export
Desc: loops over subset_claims_to_sample by year
Arg:
  type
  pct
  first_year
  last_year
Out:
  output from subset_claims_to_sample
----------------------------------------------------------------------------- */
%macro loop_years(pct, first_year, last_year, type);
  %do year = &first_year %to &last_year;
    %subset_claims_to_sample(&pct, &year, &type);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year, pde);

%loop_years(&pct, &first_year, &last_year, ip);
