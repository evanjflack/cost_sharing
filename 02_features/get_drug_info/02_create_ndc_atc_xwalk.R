# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes xwalk of ndc9 to g_rxcuis of interest, pill strength, full
#       generic name, and branded status.

library(stringr)
library(httr)

source("../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

start_log_file(paste0("log/02_create_ndc_atc_xwalk_", pct))

# Define Functions -------------------------------------------------------------

# Fxn: get_rxcui
# Desc: queries rxnorm for the rxcui currently associated with the ndc11 code
#       (or most recently associated with in the case of inactive ndcs)
# Arg:
#   ndc: ndc11 code (string of length 11, make sure to incldue leading 0s)
# Return:
#   value: rxcui associated with the ndc11 code
get_rxcui <- function(ndc) {
  result <- GET(
    url = "https://rxnav.nlm.nih.gov/",
    path = paste0("REST/ndcstatus?ndc=", ndc)
  )
  content(result)$ndcStatus$rxcui
}

# Fxn: get_atc
# Desc: queries rxclass for the atc4 codes associated with an rxcui
# Args:
#   rxcui: rxcui code (string or integer)
# Returns:
#   value: list with three elements
#     atc4: string of all atc4 codes associated with the rxcui (if there are
#           multiple, codes are separated by semicolons)
#     g_rxcui: string of all generic rxcui codes associated with the rxcui
#             (if there are multiple, codes are separated by semicolons)
#     name: string of all generic names associated with the rxcui (if there
#           are multiple, names are separated by semicolons)
get_atc <- function(rxcui) {

  if (is.na(rxcui) | rxcui == "") {
    r_list <- list(g_rxcui = "", name = "", atc4 = "")
  } else {
    # Queries RxClass for all ATC information associated wit the rxcui
    result <- GET(
      url = "https://rxnav.nlm.nih.gov/",
      path = paste0("REST/rxclass/class/byRxcui"),
      query = list(rxcui = rxcui, relaSource = "ATC")
    )

    # Extracts the 4th level ATC codes, if there are multiple, they are returned
    # in a single sting, separated by semicolons
    atc4 <- content(result)$rxclassDrugInfoList$rxclassDrugInfo %>%
      lapply(function(x) x$rxclassMinConceptItem$classId) %>%
      paste0(collapse = "; ")

    # Extracts what I call the "generic" rxcui code. Same procedure if there are
    # multiple.
    g_rxcui <- content(result)$rxclassDrugInfoList$rxclassDrugInfo %>%
      lapply(function(x) x$minConcept$rxcui)
    if (length(g_rxcui) > 0) {
      g_rxcui <- paste(unlist(g_rxcui), collapse = "; ")
    } else {
      g_rxcui <- ""
    }

    # Extracts the generic name of the drug. Same procedure if there are multiple.
    name <- content(result)$rxclassDrugInfoList$rxclassDrugInfo %>%
      lapply(function(x) x$minConcept$name)
    if (length(name) > 0) {
      name <- paste(unlist(name), collapse = "; ")
    } else {
      name <- ""
    }
    r_list <- list(g_rxcui = g_rxcui, name = name, atc4 = atc4)
  }
  return(r_list)
}

get_other_rxcui <- function(rxcui) {
  result <- GET(paste0("https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui,
                       "/historystatus"))
  ret <- content(result)$rxcuiStatusHistory$derivedConcepts$
    ingredientConcept[[1]]$ingredientRxcui
  return(ret)
}

get_concept_name <- function(rxcui) {
  result <- suppressMessages(content(GET(paste0(
    "https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui,
    "/historystatus"))))
  if (result == "Not Found") {
    ret <- ""
  } else {
    ret <-result$rxcuiStatusHistory$attributes$name
  }
  return(ret)
}

# Read In Data -----------------------------------------------------------------

# All unique ndc9 codes in the data
unique_ndc9 <- fread(paste0(lib_base_data, "unique_ndc9_", "20pct", ".csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
  .[, ndc := str_pad(ndc, 11, pad = "0")]

# If testing (aka using 0001pct sample) just use first 100 rows
if (pct == "0001pct") {
  unique_ndc9 %<>%
    .[sample(1:nrow(.), 100), ]
}

# Query RxNav ------------------------------------------------------------------
message("Querying for rxcuis...")
xwalk <- copy(unique_ndc9) %>%
  .[, rxcui := sapply(ndc, get_rxcui)]

perc_no_rxcui <- mean(is.na(xwalk$rxcui) | (xwalk$rxcui == ""))
message(round(mean(xwalk$rxcui == ""), 4)*100,
        "% of all ndc9s without an rxcui")

message("Querying for g_rxcuis/namess/atc4s...")
xwalk %<>%
  .[, r_list := lapply(rxcui, get_atc)] %>%
  # Unpack list
  .[, atc4 := lapply(r_list, function(x) x$atc4)] %>%
  .[, g_rxcui := lapply(r_list, function(x) x$g_rxcui)] %>%
  .[, name := lapply(r_list, function(x) x$name)] %>%
  .[, r_list := NULL]

message("PRE CORRECTION")
message(round(mean(xwalk$atc4 == ""), 4)*100, " % of all ndc9s without atc4")
message(round(mean(xwalk[rxcui != "", atc4] == ""), 4)*100,
        " % of ndc9s with an rxcui but without atc4")

# ndc9s with an rxcui but no atc info
xwalk1 <- copy(xwalk) %>%
  .[(!(rxcui == "")), ] %>%
  .[atc4 == "", ] %>%
  .[, .(rxcui)] %>%
  unique()

# Try to get the other rxcui associated with this rxcui, only keep those for
# for which this is returned.
xwalk1 %<>%
  .[, rxcui1 := sapply(rxcui, get_other_rxcui)] %>%
  .[!(rxcui1 == "NULL")]

# Now try to get the atc4 associated with this new rxcui
xwalk1 %<>%
  .[, r_list := lapply(rxcui1, get_atc)] %>%
  .[, atc4 := sapply(r_list, function(x) x$atc4)] %>%
  .[, g_rxcui := sapply(r_list, function(x) x$g_rxcui)] %>%
  .[, name := sapply(r_list, function(x) x$name)] %>%
  .[, r_list := NULL] %>%
  setnames(c("atc4", "g_rxcui", "name"), c("atc4_1", "g_rxcui_1", "name_1"))

# Remove those for which atc4 (and other fields) is still missing
xwalk1 %<>%
  .[!(atc4_1 == ""), ] %>%
  .[, rxcui1 := NULL]

# Merge back into original xwalk and record atc4/other info
xwalk %<>%
  merge(xwalk1, by = "rxcui", all.x = TRUE)
xwalk[is.na(xwalk)] <- ""
xwalk %<>%
  .[, atc4 := ifelse(atc4 == "", atc4_1, atc4)] %>%
  .[, g_rxcui := ifelse(g_rxcui == "", g_rxcui_1, g_rxcui)] %>%
  .[, name := ifelse(name == "", name_1, name)] %>%
  .[, `:=`(atc4_1 = NULL, g_rxcui_1 = NULL, name_1 = NULL)]

# Get the concept name (to look at for all those without an atc4 still)
xwalk %<>%
  .[, concept_name := sapply(rxcui, get_concept_name)]

xwalk %<>%
  .[, .(lab_prod, ndc, rxcui, atc4, g_rxcui, name, concept_name)]

message("POST CORRECTION")
message(round(mean(xwalk$atc4 == ""), 4)*100, " % of all ndc9s without atc4")
message(round(mean(xwalk[rxcui != "", atc4] == ""), 4)*100,
        " % of ndc9s with an rxcui but without atc4")

# Export -----------------------------------------------------------------------

fwrite(xwalk, paste0(lib_base_data, "ndc9_atc4_xwalk_", pct, ".csv"))

end_log_file()
