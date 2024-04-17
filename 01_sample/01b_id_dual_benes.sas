/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Identifies OASI beneficiaries that are dually eligible for medicaid or LIS
      subsidies for use in the prediction sample
*/

* librefs;
%include "../sas_librefs.sas";

%let first_year = 2007;
%let last_year = 2013;

/*
Macro: subset_dual
Desc: Subsets bsf file to oasi beneficiaries that that are dually eligible for
  medicaid, have 12 full months of coverage, and do not die during the year
Arg:
  pct: sample percentage
  year: sample year
Out:
  dual_bsf_&year._&pct: subsetted bsf file
*/
%macro subset_dual(pct, year);
  data em.dual_bsf_&year._&pct (drop = part_d_ind1-part_d_ind12
                                       dual_ind1-dual_ind12
                                       cstshr_ind1-cstshr_ind12);
    set bsf.bsf_&year._&pct;

    * Create indicators for part D coverage;
    array cntrct cntrct01--cntrct12;
    array  part_d_ind{12} part_d_ind1-part_d_ind12;
    do i = 1 to 12;
      part_d_ind{i} = ((rfrnc_yr <= 2009 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                       cntrct{i} ^= "N" & cntrct{i} ^= "*" & cntrct{i} ^= "X") |
                       (rfrnc_yr >= 2010 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                        cntrct{i} ^= "N" & cntrct{i} ^= "*"));
    end;
    part_d_mo = sum(of part_d_ind{*});

    * PDP indicators;
    array pdp_ind{12} pdp_ind1-pdp_ind12;
    do i = 1 to 12;
      pdp_ind{i} = (substr(cntrct{i}, 1, 1) = "S");
    end;
    pdp_mo = sum(of pdp_ind{*});

    * Dual eligibility indicators;
    array dual dual_01--dual_12;
    array  dual_ind{12} dual_ind1-dual_ind12;
    do i = 1 to 12;
      dual_ind{i} = (dual{i} = "02" | dual{i} = "04" | dual{i} = "08" |
                     dual{i} = "01" | dual{i} = "03" | dual{i} = "05" |
                     dual{i} = "06");
    end;
    dual_mo = sum(of dual_ind{*});

    * LIS eligibility indicators;
    array cstshr cstshr01--cstshr12;
    array  cstshr_ind{12} cstshr_ind1-cstshr_ind12;
    do i = 1 to 12;
      cstshr_ind{i} = (cstshr{i} = "01" | cstshr{i} = "02" | cstshr{i} = "03" |
                       cstshr{i} = "04" | cstshr{i} = "05" | cstshr{i} = "06" |
                       cstshr{i} = "07" | cstshr{i} = "08");
    end;
    cstshr_mo = sum(of cstshr_ind{*});

    * Indicator for any dual/LIS eligibility;
    any_dual_cstshr = (dual_mo > 0 | cstshr_mo > 0);

    * Indicator for whether or not beneficary dies during the year;
    death_mo = month(death_dt);
    death_yr = year(death_dt);
    if death_yr = rfrnc_yr then end_mo = death_mo; else end_mo = 12;
    if death_yr = rfrnc_yr then mort = 1; else mort = 0;

    * Keep only benes that are OASI, 66+, have any dual/LIS eligibility
    throughout the year, are born before september, have a coverage until they
    die, and do not die until at least 2 months after their birth month;
    if orec = 0;
    if age1 >= 66;
    if any_dual_cstshr = 1;
    if birth_mo <= 9;

  run;

  %let file_name = dual_bsf_&year._&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  * Export subsetted bsf file to CSV;
  proc export
    data = em.dual_bsf_&year._&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;

/*
Macro: loop_years
Desc: loops over subset_dual and makes a simple list of beneficiaries, calendar
      years, and birth months present in the subsetted bsf file
Arg:
  pct
  first_year
  last_year
Out:
  dual_benes_&pct
*/
%macro loop_years(pct, first_year, last_year);
  * Lopp over subset_dual and combine;
  %do year = &first_year %to &last_year;
    %subset_dual(&pct, &year);
    %if &year = &first_year %then %do;
      data em.dual_benes_&pct;
        set em.dual_bsf_&year._&pct (keep = bene_id rfrnc_yr birth_mo);
      run;
    %end;
    %else %do;
      data em.dual_benes_&pct;
        set em.dual_benes_&pct em.dual_bsf_&year._&pct (keep = bene_id rfrnc_yr
                                                              birth_mo);
      run;
    %end;
  %end;

  * Rename birth_mo to first_mo;
  data em.dual_benes_&pct (rename = (birth_mo = first_mo));
    set em.dual_benes_&pct;
  run;

  %let file_name = dual_benes_&pct..csv;
  %let file_path = &exp_dir.&file_name.;

  * Export to CSV;
  proc export
    data = em.dual_benes_&pct dbms=csv
    outfile = "&file_path"
    replace;
  run;
%mend;

%loop_years(&pct, &first_year, &last_year);
