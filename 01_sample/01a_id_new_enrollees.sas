/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Identifies OASI beneficiaries that enroll in part D at age 65 or 66. This
      is the start of the primary analytic sample
*/

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2006;
%let last_year = 2013;

/*
Macro: subset_oasi
Desc: Subsets bsf file to oasi beneficiaries that are enrolled in part D at age
     65 or 66
Arg:
  pct: sample percentage
  year: sample year
Out:
  pde_benes_&year._&pct: dataset of new enrollees + the year/month they enroll
 */
%macro subset_oasi(pct, year);
  data em.oasi_bsf_&year._&pct;
    set bsf.bsf_&year._&pct (keep = bene_id rfrnc_yr age1 age birth_mo orec
                                    cntrct01--cntrct12);

    * only keep if it is the year they turn 65 or 66;
    if age1 = 65 | age1 = 66;
    * only keep if original reason for eligibility is OASI;
    if orec = 0;

    * Part D coverage indicators;
    array cntrct cntrct01--cntrct12;
    array  part_d_ind{12} part_d_ind1-part_d_ind12;
    do i = 1 to 12;
      part_d_ind{i} = ((rfrnc_yr <= 2009 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                       cntrct{i} ^= "N" & cntrct{i} ^= "*" & cntrct{i} ^= "X") |
                       (rfrnc_yr >= 2010 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                        cntrct{i} ^= "N" & cntrct{i} ^= "*"));
    end;
    part_d_mo = sum(of part_d_ind{*});

  run;



  * Only keep years with any part D coverage;

  data em.oasi_bsf_&year._&pct;
    set em.oasi_bsf_&year._&pct;
      if part_d_mo > 0;
    run;


  * Transpose data to long (bene/month level);
  proc transpose data = em.oasi_bsf_&year._&pct out = em.oasi_bsf_&year._&pct;
    by bene_id age1;
    var part_d_ind1-part_d_ind12;
  run;

  * Create month variables;
  data em.oasi_bsf_&year._&pct;
    set em.oasi_bsf_&year._&pct;
    by bene_id;
    if first.bene_id then month = 0; month+1;
  run;



  * Only keep months with part d coverage (col1=1);
  data em.pde_benes_&year._&pct (keep = bene_id month age1);
    set em.oasi_bsf_&year._&pct;
    if col1 = 1;
  run;

  * Only keep the first month of part d coverage in that year;
  data em.pde_benes_&year._&pct (rename = (month = first_mo));
    set em.pde_benes_&year._&pct;
    by bene_id;
    if first.bene_id;
    join_year = &year;
  run;
%mend;

/* -----------------------------------------------------------------------------
Macro: loop_years
Desc: Loops over subset_oasi, combines each year's pde_benes data set, and
      subsets to the first year the beneficary had coverage.
Arg:
  pct
  first_year
  last_year
Out:
  pde_benes_&pct:
----------------------------------------------------------------------------- */
%macro loop_years(pct, first_year, last_year);
  * loop over subset oasi and combine pde_benes;
  %do year = &first_year %to &last_year;
    %subset_oasi(&pct, &year);
    %if &year = &first_year %then %do;
      data em.pde_benes_&pct;
        set em.pde_benes_&year._&pct;
      run;
    %end;
    %else %do;
      data em.pde_benes_&pct;
        set em.pde_benes_&pct
        em.pde_benes_&year._&pct;
      run;
    %end;
  %end;

  * Sort by bene_id/age, and then only keep first year of enrollment (if that
  year is 2007 or later);
  proc sort data = em.pde_benes_&pct;
    by bene_id age1;
  run;

  data em.pde_benes_&pct;
    set em.pde_benes_&pct;
    by bene_id;
    if first.bene_id then obs = 1; else obs = 2;
    if obs = 1;
    if join_year >= 2007;
  run;


  %let file_name = pde_benes_&pct..csv;
  %let file_path = &exp_dir.&file_name.;
  * Export to CSV;
  proc export
    data = em.pde_benes_&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;

%loop_years(&pct, &first_year, &last_year);
