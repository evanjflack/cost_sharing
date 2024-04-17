# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: List all diabetes outcome codes (to be used in
# 01e_id_dual_acute_outcomes.R)

all_combo <- function(icd9_code) {
  if (str_length(icd9_code) == 4) {
    expand5 <- paste0(icd9_code, seq(0, 9))
    code_out <- c(icd9_code, expand5)
  } else if (str_length(icd9_code) == 3) {
    expand4 <- paste0(icd9_code, seq(0, 9))
    expand5 <- paste0(icd9_code, str_pad(seq(0, 99), 2, pad = "0"))
    code_out <- c(icd9_code, expand4, expand5)
  } else if (str_length(icd9_code) == 5) {
    code_out <- icd9_code
  }
  return(code_out)
}

# Retinoparthy -----------------------------------------------------------------
# Diabetic ophthalmologic disease	250.5x •
diab_op <- all_combo("2505")
# Background retinopathy	362.01 •
b_ret <- all_combo("36201")
# Other retinopathy	362.1	•
o_ret <- all_combo("3621")
# Retinal edema	362.83	•
ret_ed <- all_combo("36283")
# CSME	362.53	•
csme <- all_combo("36253")
# Other retinal disorders	362.81, 362.82	•
o_ret_dis <- c(all_combo("36281"), all_combo("36282"))
# Proliferative retinopathy	362.02		••
prol_ret <- all_combo("36202")
# Retinal detachment	361.xx		••
ret_det <- all_combo("362")
# Blindness	369.xx .00-.99		••
blind <- all_combo("369")
# Vitreous hemorrhage 379.23		••
vit_hem <- all_combo("37923")

retinopathy_codes <- c(diab_op, b_ret, o_ret, ret_ed, csme, o_ret_dis, prol_ret,
                       ret_det, blind, vit_hem)

rm(diab_op, b_ret, o_ret, ret_ed, csme, o_ret_dis, prol_ret, ret_det,
   blind, vit_hem)

# Nephropathy ------------------------------------------------------------------
# Diabetic nephropathy	250.4	•
diab_nep <- all_combo("2504")
# Acute glomerulonephritis	580	•
acu_glo <- all_combo("580")
# Nephrotic syndrome	581	•
neph_syn <- all_combo("581")
# Hypertension, nephrosis	581.81	•
hpyer_neph <- all_combo("58181")
# Chronic glomerulonephritis	582	•
chrin_glo <- all_combo("582")
# Nephritis/nephropathy	583	•
neph <- all_combo("583")
# Chronic renal failure	585		••
chron_ren_fail <- all_combo("585")
# Renal failure NOS	586		••
ren_fail_nos <- all_combo("586")
# Renal insufficiency 593.9 ••
ren_insuf <- all_combo("5939")

nephropathy_codes <- c(diab_nep, acu_glo, neph_syn, hpyer_neph, chrin_glo, neph,
                       chron_ren_fail, ren_fail_nos, ren_insuf)

rm(diab_nep, acu_glo, neph_syn, hpyer_neph, chrin_glo, neph,
   chron_ren_fail, ren_fail_nos, ren_insuf)

# Neuropathy -------------------------------------------------------------------
# Diabetic neuropathy	356.9, 250.6	•
diab_neuro <- c(all_combo("3569"), all_combo("2506"))
# Amyotrophy	358.1	•
amy <- all_combo("3581")
# Cranial nerve palsy	951.0, 951.1, 951.3	•
cran_nrv_pal <- c(all_combo("9510"), all_combo("9511"), all_combo("9513"))
# Mononeuropathy	354.0-355.9	•
mono <- c(all_combo("354"), all_combo("355"))
# Charcot’s arthropathy	713.5	•
char_arth <- all_combo("7135")
# Polyneuropathy	357.2	•
poly_neuro <- all_combo("3572")
# Neurogenic bladder	596.54	•
neuro_bladder <- all_combo("596")
# Autonomic neuropathy	337.0, 337.1	•
auto_neuro <- c(all_combo("3370"), all_combo("3371"))
# Gastroparesis/diarrhea	564.5, 536.3	•
gastro <- c(all_combo("5645"), all_combo("5363"))
# Orthostatic hypotension	458.0
ortho_hypo <- all_combo("4580")

neuropath_codes <- c(diab_neuro, amy, cran_nrv_pal, mono, char_arth, poly_neuro,
                     neuro_bladder, auto_neuro, gastro, ortho_hypo)

rm(diab_neuro, amy, cran_nrv_pal, mono, char_arth, poly_neuro,
   neuro_bladder, auto_neuro, gastro, ortho_hypo)

# Cerebrovascular --------------------------------------------------------------
# TIA	435
tia <- all_combo("435")
# Stroke	431, 433, 434, 436
stroke <- c(all_combo("431"), all_combo("433"), all_combo("434"), all_combo("436"))

cerebrovascular_codes <- c(tia, stroke)

rm(tia, stroke)

# Cardiovasular ----------------------------------------------------------------
# Atherosclerosis	440.xx	•
athero <- all_combo("440")
# Other IHD	411	•
other_ihd <- all_combo("411")
# Angina pectoris	413	•
ang_pec <- all_combo("413")
# Other chronic IHD	414	•
other_chron_ihd <- all_combo("414")
# Myocardial infarction	410		••
mi <- all_combo("410")
# Ventricular fibrillation, arrest	427.1, 427.3		••
cent_fib <- c(all_combo("4271"), all_combo("4273"))
# Atrial fibrillation, arrest	427.4, 427.5		••
art_fib <- c(all_combo("4274"), all_combo("4275"))
# Other ASCVD	429.2	•
other_asvcd <- all_combo("4292")
# Old myocardial infarction	412		••
old_mi <- all_combo("412")
# Heart failure	428		••
heart_fail <- all_combo("428")
# Atherosclerosis, severe	440.23, 440.24		••
atherosclerosis <- c(all_combo("44023"), all_combo("44024"))
# Aortic aneurysm/dissection	441
aortic_an <- all_combo("441")

cardiovascular_codes <- c(athero, other_ihd, ang_pec, other_chron_ihd, mi,
                          cent_fib, art_fib, other_asvcd, old_mi, heart_fail,
                          atherosclerosis, aortic_an)

rm(athero, other_ihd, ang_pec, other_chron_ihd, mi,
   cent_fib, art_fib, other_asvcd, old_mi, heart_fail,
   atherosclerosis, aortic_an)

# Peripheral Vasular Disease ---------------------------------------------------
# Diabetic PVD	250.7	•
diab_pvd <- all_combo("2507")
# Other aneurysm, LE	442.3	•
other_an_le <- all_combo("4423")
# PVD	443.81, 443.9	•
pvd <- c(all_combo("44381"), all_combo("4439"))
# Foot wound + complication	892.1	•
foot_wound <- all_combo("8921")
# Claudication, intermittent	443.9	•
claud <- all_combo("4439")
# Embolism/thrombosis (LE)	444.22		••
embolism <- all_combo("44422")
# Gangrene	785.4		••
gang <- all_combo("7854")
# Gas gangrene	0.40		••
gas_gang <- all_combo("00040")
# Ulcer of lower limbs	707.1		••
ulcer <- all_combo("7071")

pvd_codes <- c(diab_pvd, other_an_le, pvd, foot_wound, claud, embolism,
               gang, gas_gang, ulcer)

rm(diab_pvd, other_an_le, pvd, foot_wound, claud, embolism,
   gang, gas_gang, ulcer)

# Metabolic --------------------------------------------------------------------
# Metabolic	Ketoacidosis	250.1		••
meta_keto <- all_combo("2501")
# Hyperosmolar 250.2 ••
hyperosmolar <- all_combo("2502")
# Other coma 250.3 ••
other_coma <- all_combo("2503")

metabolic_codes <- c(meta_keto, hyperosmolar, other_coma)

rm(meta_keto, hyperosmolar, other_coma)

# All Codes --------------------------------------------------------------------
all_diab_codes <- c(retinopathy_codes, nephropathy_codes, neuropath_codes,
                    cerebrovascular_codes, cardiovascular_codes, pvd_codes,
                    metabolic_codes)

rm(retinopathy_codes, nephropathy_codes, neuropath_codes,
   cerebrovascular_codes, cardiovascular_codes, pvd_codes,
   metabolic_codes, all_combo)
