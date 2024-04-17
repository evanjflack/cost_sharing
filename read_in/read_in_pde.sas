/*
Proj: Cost-sharing
Author: Evan Flack (flack@stanford.edu)
Desc: Reads in part d event (pde) data, chooses which variables to keep,
      and defines/formats additional variables. Data from 2012 is formatted
      differently, hence the different code for 2012.
*/

* Librefs;
%include "../sas_librefs.sas";

%let first_year = 2006;
%let last_year = 2013;

* year cutoff option for date variables.;
OPTIONS YEARCUTOFF=1950;

/*
Macro: read_in_pde
Desc: see script desc
Arg:
    pct: sample percentage
    year: calendar year
Ret: Fomated pde file
*/
%macro read_in_pde(pct, year);
  * different variable names in 2012;
  %if &year = 2012 %then %do;
    data pde.pde_&year._&pct;
      set "/disk/aging/partd/&pct/pde/&year/pde&year"
      (keep = pde_id bene_id srvc_dt prod_srvc_id  days_suply_num BENEFIT_PHASE
              tot_rx_cst_amt PTNT_PAY_AMT OTHR_TROOP_AMT LICS_AMT PLRO_AMT
              prvdqlfr prvdr_id  prscqlfr prscrbid drug_cvrg_stus_cd
              ncvrd_plan_pd_amt  cvrd_d_plan_pd_amt
      rename = (prod_srvc_id=prdsrvid  days_suply_num=dayssply
                tot_rx_cst_amt=totalcst PTNT_PAY_AMT = ptpayamt
                OTHR_TROOP_AMT = othtroop BENEFIT_PHASE = bnftphas
                drug_cvrg_stus_cd = drcvstcd  ncvrd_plan_pd_amt = npp_amt
                cvrd_d_plan_pd_amt = cpp_amt));
      rfrnc_yr = &year;
      * 9 digit ndc labler/product code;
      lab_prod = substr(prdsrvid,1,9);
      srvc_mo = month(srvc_dt);
      format srvc_dt MMDDYYS10.;
    run;
  %end;
  %else %do;
    data pde.pde_&year._&pct;
      set "/disk/aging/partd/&pct/pde/&year/pde&year"
      (keep = pde_id bene_id srvc_dt prdsrvid dayssply bnftphas totalcst
              ptpayamt othtroop lics_amt plro_amt  prvdqlfr prvdr_id  prscqlfr
              prscrbid drcvstcd npp_amt cpp_amt);
      * Same as above;
      rfrnc_yr = &year;
      lab_prod = substr(prdsrvid,1,9);
      srvc_mo = month(srvc_dt);
      format srvc_dt MMDDYYS10.;
    run;
  %end;

  * Sort (for merging in a different sas script);
  proc sort data = pde.pde_&year._&pct;
    by bene_id;
  run;

%mend;

/*
Macro: loop_years
Desc: Loops over read_in_pde macro
Arg:
    pct: sample percentage
    first_year: first calendar year
    last_year: last calcendar year
*/
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %read_in_pde(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
