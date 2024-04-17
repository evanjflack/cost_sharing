# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: User-defined functions used in analysis

# Fxn: start_log_file
# Desc: Creates a log file for script output
start_log_file <- function(file_name = NULL, log_file = TRUE, print = TRUE) {
  if (log_file == TRUE) {
    if (is.null(file_name)) {
      stop("File name required")
    }
    full_name <- paste0(file_name, ".log")
    con <- file(full_name)
    sink(con)
    sink(con, type = "message")
    message(paste(rep("-", 80), collapse = ""))
    message(full_name)
    tic()
    message(Sys.time())
    message("")
  }
  if (print == TRUE) {
    if (print == T) {
      if (exists("pct")) {
        message("pct = ", pct)
      }
      if (exists("first_year") & exists("last_year")) {
        message("years = ", first_year, "-", last_year)
      }
      if (exists("resp_var")) {
        message("resp_var = ", resp_var)
      }
      if (exists("id_strat")) {
        message("id_strat = ", id_strat)
      }
      if (exists("pred_cat")) {
        message("pred_cat = ", pred_cat)
      }
      if (exists("initial_days")) {
        message("initial_days = ", initial_days)
      }
    }
    message("")
  }
}

# Fxn: end_log_file
# Desc: Stiop connection to open .log file
end_log_file <- function() {
  if (length(showConnections(all = FALSE))) {
    message("")
    toc()
    message(Sys.time())
    message(paste(rep("-", 80), collapse = ""))
    sink()
    sink(type="message")
  }
}

# Fxn: Unpack opt
# Desc: unpacks command line arguments
unpack_opt <- function(option_list) {
  opt <- suppressWarnings(parse_args(OptionParser(option_list=option_list)))
  opt$help <- NULL
  invisible(list2env(opt, .GlobalEnv))
  if (exists("first_year") & exists("last_year")) {
    years <- list(years = seq(opt$first_year, opt$last_year))
    invisible(list2env(years, .GlobalEnv))
  }
  # Print Options
}

# Fxn: calc_cmean
# Desc: Calculates conditional means by covariate
calc_cmean <- function(DT, y, x, se = F){
  if (!is.data.table(DT)) stop("'DT' must be class data.table")
  if (se == T) {
    DT_cmean <- DT[, lapply(.SD, mean_se),  by = x, .SDcols = y] %>%
      .[, measure := rep(c("mean", "se", "obs"), nrow(.)/3)] %>%
      melt(id.var = c(x, "measure")) %>%
      dcast(as.formula(paste(paste(x, collapse  = " + "),
                             "+ variable ~ measure")),
            value.var = "value") %>%
      # 95% CI interval bounds
      .[, `:=`(lb = mean - 1.96*se, ub = mean + 1.96*se)]
  } else {
    DT_cmean <- DT[, lapply(.SD, mean), by = x, .SDcols = y] %>%
      melt(id.var = x, value.name = "mean")
  }
}

mean_se <- function(x) {
  c(mean = mean(x, na.rm = T), se = sd(x, na.rm = T)/sqrt(sum(!is.na(x))),
    obs = sum(!is.na(x)))
}

# Fxn: read_and_combine()
# Desc: Combines files from multiple years
read_and_combine <- function(lib_base_data, file, years, pct, loud = FALSE) {
  DT <- data.table()
  for (i in years) {
    if (loud == T) {
      print(i)
    }
    DT1 <- fread(paste0(lib_base_data, file, "_", i, "_",
                        pct, ".csv")) %>%
      setnames(names(.), tolower(names(.)))
    DT %<>% rbind(DT1)
  }
  return(DT)
}

# Fxn: Sparsify
# Desc: Recurisively makes matricies sparse
sparsify <- function(x) {
  if (prod(dim(x)) < (2^31 - 1)) {
    return(Matrix(data.matrix(x), sparse = TRUE))
  }

  halfway <- floor(nrow(x) / 2)

  x_s <- rbind(sparsify(x[1:halfway, ]),
               sparsify(x[(halfway+1):nrow(x), ]))

  return(x_s)
}

# Fxn: fit_to_dt
# Desc: takes lm object and returns the coefficients of interest in a structed
#.      data.table
fit_to_dt <- function(fit, primary, interacts = NULL) {
  dtp_fit <- tidy(fit) %>%
    as.data.table() %>%
    .[grep(primary, term), ] %>%
    .[, term := gsub(paste0(primary, ":"), "", term)]

  if (!is.null(interacts)) {
    for (i in interacts) {
      dtp_fit %<>%
        .[, term := gsub(paste0("factor\\(", i, "\\)"), "", term)]
    }
    for (i in 1:length(interacts)) {
      dtp_fit %<>%
        .[, interacts[i] := str_split_fixed(term, ":", length(interacts))[, i]]
    }
  }
  dtp_fit %<>%
    .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)]
  return(dtp_fit)
}

# Fxn: subset sample
# Desc: Subsets sample based on keep criteria (keep_var == 1)
subset_sample <- function(DT, subset_vars, progress = F, balance_vars = NULL,
                          inst = "first_mo") {
  DT_subset <- DT
  obs <- data.frame(subset = "all", obs = nrow(DT_subset),
                    u_obs = uniqueN(DT_subset$bene_id))
  dt_fit <- data.table()
  for (var in subset_vars) {
    if (progress == T) {
      print(var)
    }
    DT_subset  <- DT_subset[get(var) == 1, ]
    obs1 <- data.table(subset = var, obs = nrow(DT_subset),
                       u_obs = uniqueN(DT_subset$bene_id))
    obs <- rbind(obs, obs1)
    if (!is.null(balance_vars)) {
      dt_fit1 <- iter_balance_fit(DT_subset, balance_vars, inst = inst) %>%
        .[, subset_var := var]
      dt_fit %<>% rbind(dt_fit1)
    }
  }
  print(obs)
  return_list <- list(DT_subset = DT_subset, obs = obs, dt_bal = dt_fit)
  return(return_list)
}

# Fxn: clean_fit_dt
# Desc: Formats estimate/se for latex
clean_fit_dt <- function(dt, id_vars, est_var = "estimate", se_var = "std.error",
                         p_var = "p.value", dig = 3) {
  dt_fit <- dt %>%
    .[, `:=`(est = get(est_var), se = get(se_var), p_val = get(p_var))] %>%
    .[, c(id_vars, "est", "se", "p_val"), with = F] %>%
    .[, lapply(.SD, function(x) ifelse(x >= 1, round(x, 2), signif(x, digits = dig))),
      by = c(id_vars, "p_val")] %>%
    .[, stars1 := ifelse(p_val <= .01, "***", ifelse(p_val <= .05, "**",
                                                     ifelse(p_val <= .1,
                                                            "*", "")))] %>%
    .[, est := paste0(est, stars1)] %>%
    .[, est_se := paste0("\\begin{tabular}{@{}c@{}}", est,
                         "\\\\ (", se,  ")\\end{tabular}")] %>%
    .[is.na(se), est_se := "-"] %>%
    .[, c(id_vars, "est_se"), with = F]

  return(dt_fit)
}

# Fxn: reshape_month_level
# Desc: Re-formats wide data to long
reshape_month_level <- function(DT, id_vars, reshape_vars, value_name) {
  DT %>%
    .[, c(id_vars, reshape_vars), with = F] %>%
    setnames(reshape_vars, as.character(seq(1, 12))) %>%
    melt(id.var = id_vars,
         variable.name = "month",
         value.name = value_name) %>%
    .[, month := as.numeric(as.character(month))] %>%
    setorderv(c(id_vars, "month"), rep(1, length(id_vars) + 1))
}

# Fxn: bin_variable
# Desc: bins variables by either a specified bin length or qualtiles
bin_variable <- function(x, min = NULL, max = NULL, int = NULL, quant = NULL,
                         center = FALSE) {
  # Bins of equal length
  if (is.null(quant)) {
    if (center == FALSE) {
      cuts <- c(-Inf, seq(min, max, int), Inf)
      labs <- seq(min, max + int, int)
    } else if (center == TRUE) {
      cuts <- c(-Inf, seq(min + int/2, max - int/2, int), Inf)
      labs <- seq(min, max, int)
    }
  } else {
    # Bins of equal size (quantiles)
    cuts <- quantile(x, seq(0, 1, 1/quant))
    cuts[1] <- -Inf
    cuts[length(cuts)] <- Inf
    labs <- seq(1, quant)
  }
  bins <- cut(x, breaks = cuts, labels = labs) %>%
    as.character() %>%
    as.numeric()

  return(bins)
}

# Fxn: unpack_ip
# Desc: Cleans up ip files for used in finding acute events
unpack_ip <- function(ip, num_dgns, num_prcdr) {

  # Principle diagnosis

  dgns_vars <- paste0("dgnscd", seq(1, num_dgns))
  ip_diag <- ip %>%
    .[clm_ln == 1, ] %>%
    .[, c("bene_id", "clm_id", "from_dt", dgns_vars), with = FALSE] %>%
    setnames(dgns_vars, as.character(seq(1, num_dgns))) %>%
    melt(id.var = c("bene_id", "clm_id", "from_dt"),
         variable.name = "code_num",
         value.name = "dgnscd")

  # First "num_prcdr" procedures
  prcdr_vars <- paste0("prcdrcd", seq(1, num_prcdr))
  dt_vars <- paste0("prcdrdt", seq(1, num_prcdr))

  # Reshape procedure codes to long
  ip_prcdrcd <- ip %>%
    .[clm_ln == 1, ] %>%
    .[, c("bene_id", "clm_id", prcdr_vars), with = F] %>%
    melt(id.var = c("bene_id", "clm_id"),
         value.name = "prcdrcd", variable.name = "code_num")

  # Reshape procedure dates to long
  ip_prcdrdt <- ip %>%
    .[clm_ln == 1, ] %>%
    .[, c("bene_id", "clm_id", dt_vars), with = F] %>%
    melt(id.var = c("bene_id", "clm_id"),
         value.name = "prcdrdt", variable.name = "code_num")

  # Combine procedure codes and dates
  ip_prcdr <- cbind(ip_prcdrcd, ip_prcdrdt[, c("prcdrdt"), with = F]) %>%
    .[!(is.na(prcdrcd) | prcdrdt == ""), ]

  return(list(ip_diag = ip_diag, ip_prcdr = ip_prcdr))

}

# Fxn: create_diag_indicators
# Desc: Makes diagnoses indicators for acute events
create_diag_indicators <- function(DT, DT_id, id_var, ami_codes, stroke_codes,
                                   diab_codes, suicide_codes, od_codes,
                                   od_e_codes) {
  # Make indicators
  DT %<>%
    .[, resp_fail := ifelse(substr(dgnscd, 1, 4) == "5188", 1, 0)] %>%
    .[, resp_arr := ifelse(substr(dgnscd, 1, 4) == "7991", 1, 0)] %>%
    .[, ami := ifelse(substr(dgnscd, 1, 3) %in% ami_codes, 1, 0)] %>%
    .[, stroke := ifelse(substr(dgnscd, 1, 3) %in% stroke_codes, 1, 0)] %>%
    .[, comp_diab := ifelse(dgnscd %in% diab_codes, 1, 0)] %>%
    .[, suicide := ifelse(substr(dgnscd, 1, 4) %in% suicide_codes, 1, 0)] %>%
    .[, od := ifelse(substr(dgnscd, 1, 3) %in% od_codes |
                       substr(dgnscd, 1, 4) %in% od_e_codes, 1, 0)]

  # Aggregate by id_vars and fill in 0s
  DT %<>%
    .[, lapply(.SD, max), by = id_var,
      .SDcols = c("ami", "stroke", "resp_fail", "resp_arr", "comp_diab",
                  "suicide", "od")] %>%
    fill_in_zeros(DT_id, id_var)

  return(DT)
}

# Fxn: create_prcdr_indicators
# Desc: Makes indicators for acute event using procedures
create_prcdr_indicators <-  function(DT, DT_id, id_var) {

  DT %<>%
    .[, tube := ifelse(substr(prcdrcd, 1, 4) == "9604", 1, 0)] %>%
    .[, vent := ifelse(substr(prcdrcd, 1, 3) == "967", 1, 0)]

  DT %<>%
    .[, lapply(.SD, max), by = id_var,
      .SDcols = c("tube", "vent")] %>%
    fill_in_zeros(DT_id, id_var)

  return(DT)
}

fill_in_zeros <- function(DT, DT_id, id_name) {
  DT <- DT %>%
    merge(
      DT_id[, id_name, with = F], by = id_name, all.y = T)
  DT[is.na(DT)] <- 0
  return(DT)
}

