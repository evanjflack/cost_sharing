### The Health Costs of Cost-Sharing

**Authors**: Amitabh Chandra, Evan Flack, Ziad Obermeyer

**Contact**: Evan Flack (flack@stanford.edu)

#### Code Directories

* `read_in`: Scripts to read-in the raw Medicare .sas files, subsets/formats variables

* `01_sample`: Scripts to create the main analytic sample, along with prediction and falsification samples

* `02_features`: Scripts to create features used in the analysis, such as mortality outcomes and initial-90 days spending

* `03_model`
  * `01_prediction`: Scripts used to predict risk outcomes in the prediction sample of dual eligible beneficiaries 66+
  * `02_iv`: Scripts to make tables/figures for the IV analysis in the main text and appendix
  
* `sup_data`: Data used in addition to the Medicare claims stored on the NBER aging servers


#### Data

This project used Medicare claims/administrative data made available though a data use agreement (DUA) with the Center for Medicare and Medicaid Services (CMS). It is stored on the NBER aging servers. The raw data is stored in SAS files; the aging servers are set up such that only a few are able to run SAS programs. Because of this, the analysis cannot be run with one shell script. Instead, there are a series of shell scripts, alternating between .SAS, and .R programs.

#### Workflow

First, you will need to set the directories where you would like data to be stored. This is done in `sas_librefs.sas` and `00_pre_process/start_script.R`. The "lib_base_data" object from `start_script.R` must be the same as both "em" and "exp_dir" in `sas_librefs.sas`. The "lib_base" object in `00_pre_process/start_script.R` should be the root directory of the scripts. Also move the data stored in `sup_data` to the "lib_base_data" directory.

* `read_in/00_run_sas_scripts.sh`
* `01_sample/00a_run_sas_scripts.sh`
* `01_sample/00b_run_r_scripts.sh`
* `01_sample/00c_run_sas_scripts.sh`
* `02_features/get_drug_info/00_get_drug_info.sh`
* `02_features/00_run_r_scripts.sh`
* `03_model/01_prediction/00_predict_risk.sh`
* `03_model/02_iv/00_run_iv_scripts.sh`
