/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Identifies beneficiaries for the falsification samples
*/

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2007;
%let last_year = 2012;

/*
Macro: subset_old_oasi
Desc: Subsets bsf file to oasi beneficiaries that are ages 66-85 or diabled
      beneficiareis younger than 50-64, all with at least one month of coverage in
      the year
Arg:
  pct: sample percentage
  year: sample year
Out:
  old_bsf_&year._&pct: subsetted bsf file
 */
%macro subset_old_dis_bsf(pct, year);
  data em.old_dis_bsf_&year._&pct (drop = part_d_ind1-part_d_ind12 i);
    set bsf.bsf_&year._&pct;

    * Only keep OASI 66-85 or diabled <64 year olds;
    if (age1 >= 66 & age1 <= 85 & orec = 0) | (age1 <= 64 & orec = 1 & crec = 1);

    * Part D coverage indicators;
    array cntrct cntrct01--cntrct12;
    array  part_d_ind{12} part_d_ind1-part_d_ind12;
    do i = 1 to 12;
      part_d_ind{i} = ((rfrnc_yr <= 2009 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                       cntrct{i} ^= "N" & cntrct{i} ^= "*" & cntrct{i} ^= "X") |
                       (rfrnc_yr >= 2010 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                        cntrct{i} ^= "N" & cntrct{i} ^= "*"));
    end;
    * Number of part d months in year;
    part_d_mo = sum(of part_d_ind{*});
    * Only keep years with any part D coverage;
    if part_d_mo > 0;

  run;

  %let file_name = old_dis_bsf_&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  * Export to csv;
  proc export
    data = em.old_dis_bsf_&year._&pct
    dbms=csv
    outfile = "&file_path"
    replace;
  run;

%mend;

/* -----------------------------------------------------------------------------
Macro: loop_years
Desc: Loops over subset_old_oasi
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
    %subset_old_dis_bsf(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
