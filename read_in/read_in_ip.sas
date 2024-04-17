/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Reads in/formarts inpatient (ip) claims
*/

* Librefs;
%include "../sas_librefs.sas";

%let first_year = 2010;
%let last_year = 2013;

/*
Macro: read_in_ip
Desc: see script desc
Arg:
    pct: sample percentage
    year: calendar year
Ret: Merged and formatted carrier/line file
*/
%macro read_in_ip(pct, year);
  * Naming conventions changed in 2009;
  %if &year <= 2009 %then %do;

    * Inpatient claims file;
    data ip.ipc_&year._&pct;
      set "/disk/aging/medicare/data/&pct/ip/&year/ipc&year"
      (keep = bene_id clm_id from_dt thru_dt pmt_amt per_diem util_day
              dgnscd1-dgnscd10 prcdrcd1-prcdrcd6 prcdrdt1-prcdrdt6);
      rfrnc_yr = &year;
      total_cost = pmt_amt + per_diem*util_day;
      format from_dt  MMDDYYS10.;
      format thru_dt MMDDYYS10.;
      format prcdrdt1-prcdrdt6 MMDDYYS10.;
    run;

    proc sort data = ip.ipc_&year._&pct;
      by clm_id;
    run;

    * Revenue center file;
    data ip.ipr_&year._&pct;
        set "/disk/aging/medicare/data/&pct/ip/&year/ipr&year"
        (keep = clm_id clm_ln hcpcs_cd);
    run;

    proc sort data = ip.ipr_&year._&pct;
      by clm_id;
    run;

    data ip.ip_&year._&pct;
      merge ip.ipc_&year._&pct ip.ipr_&year._&pct;
      by clm_id;
    run;

  %end;
  %else %do;
    * Same as above but with 2010 onward naming conventions;
    data ip.ipc_&year._&pct;
        set "/disk/aging/medicare/data/&pct/ip/&year/ipc&year"
        (keep = bene_id clm_id from_dt thru_dt thru_dt pmt_amt per_diem util_day
                   icd_dgns_cd1-icd_dgns_cd10 icd_prcdr_cd1-icd_prcdr_cd6
                   prcdr_dt1-prcdr_dt6
        rename = (icd_dgns_cd1-icd_dgns_cd10 = dgnscd1-dgnscd10
                  icd_prcdr_cd1-icd_prcdr_cd6 = prcdrcd1-prcdrcd6
                  prcdr_dt1-prcdr_dt6 = prcdrdt1-prcdrdt6));
        rfrnc_yr = &year;
        total_cost = pmt_amt + per_diem*util_day;
        format from_dt MMDDYYS10.;
        format thru_dt MMDDYYS10.;
        format prcdrdt1-prcdrdt6 MMDDYYS10.;
    run;

    proc sort data = ip.ipc_&year._&pct;
      by clm_id;
    run;

    data ip.ipr_&year._&pct;
        set "/disk/aging/medicare/data/&pct/ip/&year/ipr&year"
        (keep = clm_id clm_ln hcpcs_cd);
    run;

    proc sort data = ip.ipr_&year._&pct;
      by clm_id;
    run;

    data ip.ip_&year._&pct;
      merge ip.ipc_&year._&pct ip.ipr_&year._&pct;
      by clm_id;
    run;
  %end;

  proc sort data = ip.ip_&year._&pct;
    by bene_id;
  run;

  data ip.ip_short_&year._&pct;
    set ip.ip_&year._&pct;
    if clm_ln = 1;
  run;

%mend;

/*
Macro: loop_years
Desc: Loops over read_in_ip macro
Arg:
    pct: sample percentage
    first_year: first calendar year
    last_year: last calcendar year
*/
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %read_in_ip(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
