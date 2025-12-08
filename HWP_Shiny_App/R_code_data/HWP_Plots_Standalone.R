# HWP_Plots_Standalone.R

# (optional) install.packages("ggpattern")

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridisLite)
library(abind)
library(reshape2)
library(ggpattern)
library(scales)
library(tidyselect)
library(grid)

`%||%` <- function(a,b) if (is.null(a)) b else a

# ---- Build hwp from model.outputs -------------------------------------------
make_hwp <- function(model.outputs) {
  req <- c("eu_array","pu.final_array","swdsCtotal_array","eec_array","ewoec_array")
  miss <- setdiff(req, names(model.outputs))
  if (length(miss)) stop("model.outputs is missing: ", paste(miss, collapse=", "))
  
  hwp <- list(
    eu_array          = model.outputs$eu_array,
    pu.final_array    = model.outputs$pu.final_array,
    swdsCtotal_array  = model.outputs$swdsCtotal_array,
    eec_array         = model.outputs$eec_array,
    ewoec_array       = model.outputs$ewoec_array,
    
    # optional (only needed for certain plots)
    harv.hwp          = model.outputs$harv.hwp %||% NULL,
    eu_half.lives.hwp = model.outputs$eu_half.lives.hwp %||% NULL,
    
    # MC (only if you’ll plot MC)
    mc_plot           = model.outputs$mc_plot %||% NULL,
    mc_PoolsTotalPlot = model.outputs$mc_PoolsTotalPlot %||% NULL,
    mc_iter_results   = model.outputs$mc_iter_results %||% NULL,
    MC.CI.REPORT      = model.outputs$MC.CI.REPORT %||% 0.90,
    N.ITER            = model.outputs$N.ITER %||% NA_integer_,
    C.names           = model.outputs$C.names %||% labeller(.default = label_value)
  )
  
  # ---- sanity: eu_array must be 3D ----
  if (length(dim(hwp$eu_array)) != 3) {
    stop("eu_array must be a 3D array [EndUse, Ownership, Year]; got dim = ",
         paste(dim(hwp$eu_array), collapse="x"))
  }
  
  # ---- ensure dimnames on eu_array ----
  eu_dn <- dimnames(hwp$eu_array)
  if (is.null(eu_dn)) eu_dn <- vector("list", 3)
  if (is.null(eu_dn[[1]])) eu_dn[[1]] <- paste0("EU", seq_len(dim(hwp$eu_array)[1]))
  if (is.null(eu_dn[[2]])) eu_dn[[2]] <- paste0("Owner", seq_len(dim(hwp$eu_array)[2]))
  if (is.null(eu_dn[[3]])) eu_dn[[3]] <- as.character(seq_len(dim(hwp$eu_array)[3]))
  dimnames(hwp$eu_array) <- eu_dn
  
  # ---- add "Total" ownership to eu_array if missing ----
  owns <- dimnames(hwp$eu_array)[[2]]
  if (!("Total" %in% owns)) {
    tot <- apply(hwp$eu_array, c(1,3), sum) # sum over ownerships
    eu2 <- abind(hwp$eu_array,
                 array(tot, dim = c(dim(hwp$eu_array)[1], 1, dim(hwp$eu_array)[3])),
                 along = 2)
    dimnames(eu2)[[2]] <- c(owns, "Total")
    hwp$eu_array <- eu2
  }
  
  # ---- function to align other arrays to eu_array ----
  align_like_eu <- function(arr, name) {
    if (is.null(arr)) return(NULL)
    if (length(dim(arr)) != 3) {
      stop(name, " must be 3D like eu_array; got dim = ", paste(dim(arr), collapse="x"))
    }
    d_eu <- dim(hwp$eu_array); d_a <- dim(arr)
    if (!all(d_a == d_eu)) {
      stop(name, " dims ", paste(d_a, collapse="x"),
           " don't match eu_array dims ", paste(d_eu, collapse="x"))
    }
    dna <- dimnames(arr)
    if (is.null(dna)) dna <- vector("list", 3)
    if (is.null(dna[[1]])) dna[[1]] <- dimnames(hwp$eu_array)[[1]]
    if (is.null(dna[[2]])) dna[[2]] <- dimnames(hwp$eu_array)[[2]]
    if (is.null(dna[[3]])) dna[[3]] <- dimnames(hwp$eu_array)[[3]]
    dimnames(arr) <- dna
    arr
  }
  
  hwp$pu.final_array   <- align_like_eu(hwp$pu.final_array,   "pu.final_array")
  hwp$swdsCtotal_array <- align_like_eu(hwp$swdsCtotal_array, "swdsCtotal_array")
  hwp$eec_array        <- align_like_eu(hwp$eec_array,        "eec_array")
  hwp$ewoec_array      <- align_like_eu(hwp$ewoec_array,      "ewoec_array")
  
  # ---- polish harv.hwp if present ----
  if (!is.null(hwp$harv.hwp)) {
    if (!("Year" %in% names(hwp$harv.hwp))) {
      hwp$harv.hwp$Year <- as.numeric(dimnames(hwp$eu_array)[[3]])
    }
  }
  hwp
}

# After you have model.outputs in memory (from your HWP run):
hwp <- make_hwp(model.outputs)

# ---------- helpers ----------
.axis_pretty <- function(x, positive_only = TRUE) {
  if (length(x) == 0 || all(!is.finite(x))) return(list(min=0,max=1,by=0.2))
  rng <- range(x, na.rm = TRUE)
  if (positive_only) rng[1] <- 0
  span <- diff(rng)
  if (span == 0) span <- abs(rng[2]) %||% 1
  by <- signif(span / 6, 1)
  list(min = if (positive_only) 0 else floor(rng[1]/by)*by,
       max = ceiling(rng[2]/by)*by,
       by = by)
}
.get_years       <- function(hwp) as.numeric(dimnames(hwp$eu_array)[[3]])
.get_ownerships  <- function(hwp) dimnames(hwp$eu_array)[[2]]
.idx_total       <- function(hwp) {
  owns <- .get_ownerships(hwp)
  if ("Total" %in% owns) which(owns=="Total") else length(owns) # fallback
}
.as_co2e <- function(v, metrictype) if (metrictype=="CO2e") v * (44/12) else v
.lab_co2e <- function(metrictype) if (metrictype=="CO2e") expression("Tg C"*O[2]*e) else "Tg C"

# =========================================================
# Domestic stock from totals helper (MMT C)
# mode = "A" => Domestic = Total - Imports
# mode = "B" => Domestic = Total - Imports - Exports (if an export stock exists)
# =========================================================
dom_from_totals_at <- function(hwp, k, mode = c("A","B")) {
  mode <- match.arg(mode)
  pu_arr <- hwp$pu.final_array
  sw_arr <- hwp$swdsCtotal_array
  owners <- trimws(dimnames(pu_arr)[[2]])
  low    <- tolower(owners)
  s <- function(arr, mask) if (any(mask)) sum(arr[, mask, k, drop = FALSE], na.rm = TRUE) else 0
  mask_total   <- (low == "total")
  mask_imports <- (low == "imports")
  mask_exports <- (low == "exports")
  
  total_t   <- s(pu_arr, mask_total)   + s(sw_arr, mask_total)
  imports_t <- s(pu_arr, mask_imports) + s(sw_arr, mask_imports)
  exports_t <- s(pu_arr, mask_exports) + s(sw_arr, mask_exports)
  
  if (mode == "A") {
    dom_tons <- total_t - imports_t
  } else {
    dom_tons <- total_t - imports_t - exports_t
  }
  max(dom_tons, 0) / 1e6
}

# =========================================================
# Domestic Inflow (MMT C/yr) from eu_array
# Sum of all ownerships except Imports/Exports/Total; fallback to residual.
# =========================================================
domestic_inflow_at <- function(hwp, k) {
  eu_arr <- hwp$eu_array
  owners <- dimnames(eu_arr)[[2]]
  low <- tolower(trimws(owners))
  mask_dom <- (low != "imports" & low != "exports" & low != "total")
  s <- function(mask) if (any(mask)) sum(eu_arr[, mask, k, drop = FALSE], na.rm = TRUE) else 0
  dom <- s(mask_dom)
  if (dom == 0) {
    dom <- s(low == "total") - s(low == "imports") - s(low == "exports")
  }
  max(dom, 0) / 1e6
}

# =========================================================
# Domestic inflow by ownership (MMT C/yr)
# Returns a named numeric vector for the requested year index k
# excluding Imports / Exports / Total.
# =========================================================
domestic_inflow_by_owner_at <- function(hwp, k) {
  eu_arr <- hwp$eu_array
  owners <- trimws(dimnames(eu_arr)[[2]])
  low    <- tolower(owners)
  
  # domestic = everything except imports / exports / total
  mask_dom    <- (low != "imports" & low != "exports" & low != "total")
  dom_owners  <- owners[mask_dom]
  if (!any(mask_dom)) return(setNames(numeric(0), character(0)))
  
  idx_dom <- which(mask_dom)
  flows   <- vapply(
    idx_dom,
    function(j) sum(eu_arr[, j, k, drop = FALSE], na.rm = TRUE) / 1e6,
    numeric(1)
  )
  names(flows) <- dom_owners
  flows
}

domestic_inflow_series <- function(hwp, years_numeric) {
  eu_years <- suppressWarnings(as.numeric(dimnames(hwp$eu_array)[[3]]))
  idx <- match(years_numeric, eu_years)
  vapply(idx, function(k) domestic_inflow_at(hwp, k), numeric(1))
}

# =========================================================
# SWDS_Inflow (MMT C/yr): year-over-year change in total SWDS stock
# =========================================================
swds_inflow_at <- function(hwp, k) {
  sw_arr <- hwp$swdsCtotal_array
  owners <- dimnames(sw_arr)[[2]]
  low <- tolower(trimws(owners))
  jT <- which(low == "total")
  owners_no_total <- owners[low != "total"]
  sum_all <- function(kk) {
    if (length(jT)) sum(sw_arr[, jT, kk, drop = FALSE], na.rm = TRUE)
    else            sum(sw_arr[, owners_no_total, kk, drop = FALSE], na.rm = TRUE)
  }
  if (k <= 1) return(NA_real_)
  (sum_all(k) - sum_all(k-1)) / 1e6
}

swds_inflow_series <- function(hwp, years_numeric) {
  dn_years <- suppressWarnings(as.numeric(dimnames(hwp$swdsCtotal_array)[[3]]))
  idx <- match(years_numeric, dn_years)
  vapply(idx, function(k) swds_inflow_at(hwp, k), numeric(1))
}

# =========================================================
# Export Outflow (MMT C/yr)
# Annual quantity that leaves the boundary via any "Exports" ownership in eu_array.
# =========================================================
export_outflow_at <- function(hwp, k) {
  eu_arr <- hwp$eu_array
  owners <- dimnames(eu_arr)[[2]]
  low <- tolower(trimws(owners))
  jExp <- which(low == "exports" | grepl("^export", low))
  if (!length(jExp)) return(0)
  sum(eu_arr[, jExp, k, drop = FALSE], na.rm = TRUE) / 1e6
}

export_outflow_series <- function(hwp, years_numeric) {
  eu_years <- suppressWarnings(as.numeric(dimnames(hwp$eu_array)[[3]]))
  idx <- match(years_numeric, eu_years)
  vapply(idx, function(k) export_outflow_at(hwp, k), numeric(1))
}

# =========================================================
# Cumulative Outflow Total helper (MMT C)
# Preferred: helper table; fallback: cumsum(eec + ewoec)
# =========================================================
cum_outflow_total_series <- function(hwp, years_numeric) {
  if (exists("hwp_totals_from_total", mode = "function")) {
    tbl <- hwp_totals_from_total(
      hwp, metric = "MMTC",
      year_from = min(years_numeric, na.rm = TRUE),
      year_to   = max(years_numeric, na.rm = TRUE),
      do_sanity_check = FALSE
    )
    tbl <- tbl[tbl$Year %in% years_numeric, ]
    if (all(c("Cum_Emitted_EnergyCap","Cum_Emitted_NoEnergyCap") %in% names(tbl))) {
      return(tbl$Cum_Emitted_EnergyCap + tbl$Cum_Emitted_NoEnergyCap)
    }
  }
  eec <- hwp$eec_array; ewo <- hwp$ewoec_array
  if (is.null(eec) || is.null(ewo)) return(rep(NA_real_, length(years_numeric)))
  owners <- dimnames(eec)[[2]]; low <- tolower(owners)
  jT <- which(low == "total"); owners_no_total <- owners[low != "total"]
  s_year <- function(arr, k) {
    if (length(jT)) sum(arr[, jT, k, drop = FALSE], na.rm = TRUE)
    else            sum(arr[, owners_no_total, k, drop = FALSE], na.rm = TRUE)
  }
  dn_years <- suppressWarnings(as.numeric(dimnames(eec)[[3]]))
  idx <- match(years_numeric, dn_years)
  yr_flow_tons <- vapply(idx, function(k) s_year(eec, k) + s_year(ewo, k), numeric(1))
  cumsum(yr_flow_tons) / 1e6
}

# =========================================================
# EndUseID → PRODUCT CATEGORY BIN DEFINITIONS
# (copied from your plot_ann_timber_by_enduse_bins logic)
# =========================================================
hwp_enduse_bins <- list(
  "Fuel" = c(1, 48, 95, 142, 197),
  "Furniture" = c(5,20,28,42,52,68,81,84,99,116,121,139,144,155,165,186),
  "Housing and Construction" = c(
    10,21,27,36,57,65,73,91,104,115,125,132,145,164,170,183,
    12,14,24,37,56,69,78,86,100,108,128,134,146,162,169,184,
    11,15,26,46,58,62,79,85,102,114,133,152,154,168,185,
    8,17,29,39,54,70,75,90,103,111,123,138,147,158,175,181,
    7,18,34,40,59,67,72,93,101,113,127,140,148,160,167,178
  ),
  "Residential Repair and Remodeling"    = c(9,16,33,38,49,66,74,83,105,110,126,130,143,161,171,187),
  "Wood Packaging & Shipping"            = c(4,22,31,44,50,64,77,92,97,112,119,131,150,157,173,182),
  "Manufacturing Misc."                  = c(2,13,30,43,51,60,80,87,106,107,118,136,149,159,166,180),
  "Other Industrial Products"            = c(35,82,129,176),
  "Rail"                                 = c(3,19,25,41,53,63,71,89,96,117,122,137,153,163,172,177),
  "Paper Products"                       = c(47,94,141,188),
  "Softwood Misc."                       = c(206,204,216,222,208,198,220,194,212,214,200,192,190,196,218,224,202,210),
  "Hardwood Misc."                       = c(205,203,215,221,207,197,219,193,211,213,199,191,189,195,217,223,201,209),
  "Other, N.A."                          = c(6,23,32,45,55,61,76,88,98,109,120,135,151,156,174,179)
)

# helper: restrict bin IDs to existing EndUse rows
.get_enduse_rows_for_bin <- function(hwp, bin_name) {
  ids <- hwp_enduse_bins[[bin_name]]
  if (is.null(ids)) return(integer(0))
  n_enduse <- dim(hwp$eu_array)[1]
  intersect(ids, seq_len(n_enduse))
}

# =========================================================
# Annual INPUT by product group (MMT C/yr) at year index k
# Groups: Paper Products, Housing and Construction, Fuel
# Includes DOMESTIC + IMPORTS (all ownerships except Exports & Total;
# falls back to Total only if needed).
# =========================================================
inflow_by_product_group_at <- function(hwp, k) {
  eu_arr  <- hwp$eu_array
  owners  <- trimws(dimnames(eu_arr)[[2]])
  low     <- tolower(owners)
  
  # Use all non-export, non-total ownerships as "inputs" (domestic + imports)
  mask_use <- (low != "exports" & low != "total")
  if (!any(mask_use)) {
    # fallback: only Total exists
    mask_use <- (low == "total")
  }
  if (!any(mask_use)) {
    return(setNames(rep(NA_real_, 3),
                    c("Input_Paper","Input_HousingConstruction","Input_Fuel")))
  }
  
  r_paper <- .get_enduse_rows_for_bin(hwp, "Paper Products")
  r_house <- .get_enduse_rows_for_bin(hwp, "Housing and Construction")
  r_fuel  <- .get_enduse_rows_for_bin(hwp, "Fuel")
  
  sum_group <- function(r_idx) {
    if (!length(r_idx)) return(NA_real_)  # NA if no such rows in this model
    sum(eu_arr[r_idx, mask_use, k, drop = FALSE], na.rm = TRUE) / 1e6
  }
  
  c(
    Input_Paper               = sum_group(r_paper),
    Input_HousingConstruction = sum_group(r_house),
    Input_Fuel                = sum_group(r_fuel)
  )
}

# =========================================================
# HWP stocks for YEAR — includes Domestic_Inflow, SWDS_Inflow,
# Export_Outflow, inflows by domestic ownership, and
# annual inputs by product group (Paper / Housing+Constr. / Fuel)
# =========================================================
print_hwp_stocks_2022_include_exports <- function(hwp, year_target = 2022L,
                                                  domestic_mode = c("A","B")) {
  domestic_mode <- match.arg(domestic_mode)
  stopifnot(!is.null(hwp$pu.final_array), !is.null(hwp$swdsCtotal_array))
  pu_arr <- hwp$pu.final_array
  sw_arr <- hwp$swdsCtotal_array
  
  get_years <- function(arr) {
    yrs <- NULL
    dn  <- dimnames(arr)
    if (!is.null(dn) && length(dn) >= 3 && !is.null(dn[[3]])) {
      cand <- suppressWarnings(as.numeric(dn[[3]]))
      if (length(cand) == dim(arr)[3] && !all(is.na(cand))) yrs <- cand
    }
    if (is.null(yrs) && !is.null(hwp$years)) yrs <- suppressWarnings(as.numeric(hwp$years))
    if (is.null(yrs) && !is.null(hwp$Years))  yrs <- suppressWarnings(as.numeric(hwp$Years))
    if (is.null(yrs)) yrs <- seq_len(dim(arr)[3])
    yrs
  }
  years <- get_years(pu_arr)
  if (!(year_target %in% years)) stop("Year ", year_target, " not present in arrays.")
  yidx <- match(year_target, years)
  
  owners <- trimws(dimnames(pu_arr)[[2]])
  low    <- tolower(owners)
  jT        <- which(low == "total")
  jImports  <- which(low == "imports")
  owners_no_total <- owners[low != "total"]
  
  s_owner_at <- function(arr, j) if (!length(j)) 0 else sum(arr[, j, yidx, drop = FALSE], na.rm = TRUE)
  sum_overall_all_owners <- function(arr) {
    if (length(jT)) sum(arr[, jT, yidx, drop = FALSE], na.rm = TRUE)
    else            sum(arr[, owners_no_total, yidx, drop = FALSE], na.rm = TRUE)
  }
  
  pu_overall  <- sum_overall_all_owners(pu_arr) / 1e6
  sw_overall  <- sum_overall_all_owners(sw_arr) / 1e6
  tot_overall <- pu_overall + sw_overall
  
  stock_domestic <- dom_from_totals_at(hwp, yidx, domestic_mode)
  stock_imports  <- (s_owner_at(pu_arr, jImports) + s_owner_at(sw_arr, jImports)) / 1e6
  
  domestic_inflow <- domestic_inflow_at(hwp, yidx)
  swds_inflow     <- swds_inflow_at(hwp, yidx)
  export_outflow  <- export_outflow_at(hwp, yidx)   # FLOW ONLY
  
  # ---- inflows by ownership (domestic only) ----
  inflow_by_owner <- domestic_inflow_by_owner_at(hwp, yidx)
  inflow_owner_list <- list()
  if (length(inflow_by_owner)) {
    nm <- names(inflow_by_owner)
    inflow_owner_list <- as.list(round(inflow_by_owner, 3))
    names(inflow_owner_list) <- paste0("Inflow_", nm)
  }
  
  # ---- annual inputs by product group (domestic + imports) ----
  inflow_groups <- inflow_by_product_group_at(hwp, yidx)
  inflow_group_list <- list()
  if (length(inflow_groups)) {
    inflow_group_list <- as.list(round(inflow_groups, 3))
  }
  
  # cumulative via helper / fallback
  cum_tbl <- if (exists("hwp_totals_from_total", mode = "function")) {
    hwp_totals_from_total(
      hwp, metric = "MMTC",
      year_from = min(2001L, min(years, na.rm = TRUE)),
      year_to   = year_target,
      do_sanity_check = FALSE
    )
  } else NULL
  
  if (!is.null(cum_tbl)) {
    cum_row <- cum_tbl[cum_tbl$Year == year_target, ]
    if (!nrow(cum_row)) stop("No cumulative row found for requested year.")
    cum_inflow         <- cum_row$Cum_Inflow_Total
    cum_e_cap          <- cum_row$Cum_Emitted_EnergyCap
    cum_e_no_cap       <- cum_row$Cum_Emitted_NoEnergyCap
    cum_outflow_total  <- cum_e_cap + cum_e_no_cap
  } else {
    Y <- years[years <= year_target]
    cum_outflow_total <- tail(cum_outflow_total_series(hwp, Y), 1)
    cum_inflow <- NA_real_; cum_e_cap <- NA_real_; cum_e_no_cap <- NA_real_
  }
  
  df_report <- data.frame(
    Year                      = year_target,
    Stock_Total               = round(tot_overall, 3),
    Stock_PIU                 = round(pu_overall, 3),
    Stock_SWDS                = round(sw_overall, 3),
    Stock_Domestic            = round(stock_domestic, 3),
    Stock_Imports             = round(stock_imports, 3),
    Domestic_Inflow           = round(domestic_inflow, 3),
    SWDS_Inflow               = round(swds_inflow, 3),
    Export_Outflow            = round(export_outflow, 3),
    Cum_Inflow_Total          = cum_inflow,
    Cum_Outflow_Total         = cum_outflow_total,
    Cum_Emitted_EnergyCap     = cum_e_cap,
    Cum_Emitted_NoEnergyCap   = cum_e_no_cap,
    check.names = FALSE
  )
  
  if (length(inflow_owner_list)) {
    df_report <- cbind(
      df_report,
      as.data.frame(inflow_owner_list, check.names = FALSE)
    )
  }
  
  if (length(inflow_group_list)) {
    df_report <- cbind(
      df_report,
      as.data.frame(inflow_group_list, check.names = FALSE)
    )
  }
  
  cat("=== HWP Carbon Stocks (MMT C) — Year", year_target,
      "===\n(Domestic stock = Total − Imports",
      if (domestic_mode == "B") " − Exports" else "",
      "; flows in MMT C/yr; Export_Outflow = C leaving CA;\n",
      "Inflow_<Ownership> columns report domestic inflows by ownership;\n",
      "Input_Paper / Input_HousingConstruction / Input_Fuel include domestic + imports.)\n\n", sep = "")
  print(df_report, row.names = FALSE)
  invisible(df_report)
}

# ================================
# Report table for a range of years
# Includes Domestic_Inflow, SWDS_Inflow, Export_Outflow flows,
# inflows by domestic ownership, and annual inputs by product group
# ================================
hwp_report_table <- function(hwp,
                             year_from = 2001L,
                             year_to   = 2022L,
                             metric_cum = c("MMTC","CO2e"),
                             domestic_mode = c("A","B"),
                             cap_dom_to_total = TRUE) {
  metric_cum    <- match.arg(metric_cum)
  domestic_mode <- match.arg(domestic_mode)
  
  stopifnot(!is.null(hwp$pu.final_array), !is.null(hwp$swdsCtotal_array))
  pu_arr <- hwp$pu.final_array
  sw_arr <- hwp$swdsCtotal_array
  eu_arr <- hwp$eu_array
  
  to_mmt <- function(x_tons) (if (length(x_tons)) x_tons else 0) / 1e6
  nz0    <- function(x) { x[is.na(x) | !is.finite(x)] <- 0; x }
  
  get_years <- function(arr) {
    yrs <- NULL
    dn  <- dimnames(arr)
    if (!is.null(dn) && length(dn) >= 3 && !is.null(dn[[3]])) {
      cand <- suppressWarnings(as.numeric(dn[[3]]))
      if (length(cand) == dim(arr)[3] && !all(is.na(cand))) yrs <- cand
    }
    if (is.null(yrs) && !is.null(hwp$years)) yrs <- suppressWarnings(as.numeric(hwp$years))
    if (is.null(yrs) && !is.null(hwp$Years))  yrs <- suppressWarnings(as.numeric(hwp$Years))
    if (is.null(yrs)) yrs <- seq_len(dim(arr)[3])
    yrs
  }
  years <- get_years(pu_arr)
  rng   <- which(years >= year_from & years <= year_to)
  if (!length(rng)) stop("No years in requested window.")
  Y  <- years[rng]
  nY <- length(Y)
  
  owners <- trimws(dimnames(pu_arr)[[2]])
  low    <- tolower(owners)
  jT        <- which(low == "total")
  jImports  <- which(low == "imports")
  owners_no_total <- owners[low != "total"]
  
  mask_dom    <- (low != "imports" & low != "exports" & low != "total")
  dom_owners  <- owners[mask_dom]
  idx_dom     <- which(mask_dom)
  nDom        <- length(dom_owners)
  
  sum_overall_all_owners_at <- function(arr, k) {
    if (length(jT)) sum(arr[, jT, k, drop = FALSE], na.rm = TRUE)
    else            sum(arr[, owners_no_total, k, drop = FALSE], na.rm = TRUE)
  }
  sum_owner_at <- function(arr, j, k) {
    if (!length(j)) return(0)
    sum(arr[, j, k, drop = FALSE], na.rm = TRUE)
  }
  
  stock_piu   <- stock_sw <- stock_tot <- numeric(nY)
  stock_imp   <- numeric(nY)
  stock_dom   <- numeric(nY)
  inflow_dom  <- numeric(nY)
  inflow_swds <- numeric(nY)
  export_out  <- numeric(nY)
  
  inflow_by_owner_mat <- if (nDom) {
    mat <- matrix(0, nrow = nY, ncol = nDom)
    colnames(mat) <- dom_owners
    mat
  } else {
    NULL
  }
  
  group_names <- c("Input_Paper","Input_HousingConstruction","Input_Fuel")
  inflow_by_group_mat <- matrix(NA_real_, nrow = nY, ncol = length(group_names))
  colnames(inflow_by_group_mat) <- group_names
  
  for (i in seq_along(rng)) {
    k <- rng[i]
    pu_tot_tons <- sum_overall_all_owners_at(pu_arr, k)
    sw_tot_tons <- sum_overall_all_owners_at(sw_arr, k)
    stock_piu[i] <- to_mmt(pu_tot_tons)
    stock_sw[i]  <- to_mmt(sw_tot_tons)
    stock_tot[i] <- to_mmt(pu_tot_tons + sw_tot_tons)
    
    imp_tons <- sum_owner_at(pu_arr, jImports, k) + sum_owner_at(sw_arr, jImports, k)
    stock_imp[i] <- to_mmt(imp_tons)
    
    stock_dom[i]   <- dom_from_totals_at(hwp, k, domestic_mode)
    inflow_dom[i]  <- domestic_inflow_at(hwp, k)
    inflow_swds[i] <- swds_inflow_at(hwp, k)
    export_out[i]  <- export_outflow_at(hwp, k)
    
    if (nDom) {
      inflows_k <- vapply(
        idx_dom,
        function(j) sum(eu_arr[, j, k, drop = FALSE], na.rm = TRUE) / 1e6,
        numeric(1)
      )
      inflow_by_owner_mat[i, ] <- inflows_k
    }
    
    grp_vals <- inflow_by_product_group_at(hwp, k)
    inflow_by_group_mat[i, ] <- as.numeric(grp_vals)
  }
  
  stock_dom[!is.finite(stock_dom) | stock_dom < 0] <- 0
  if (isTRUE(cap_dom_to_total)) stock_dom <- pmin(stock_dom, stock_tot)
  
  if (exists("hwp_totals_from_total", mode = "function")) {
    cum_tbl <- hwp_totals_from_total(
      hwp, metric = metric_cum,
      year_from = min(Y), year_to = max(Y),
      do_sanity_check = FALSE
    )
    cum_tbl <- cum_tbl[cum_tbl$Year %in% Y, ]
    cum_outflow_total <- cum_tbl$Cum_Emitted_EnergyCap + cum_tbl$Cum_Emitted_NoEnergyCap
    
    out <- data.frame(
      Year                       = Y,
      Stock_Total                = round(stock_tot, 3),
      Stock_PIU                  = round(stock_piu, 3),
      Stock_SWDS                 = round(stock_sw, 3),
      Stock_Domestic             = round(stock_dom, 3),
      Stock_Imports              = round(nz0(stock_imp), 3),
      Domestic_Inflow            = round(inflow_dom, 3),
      SWDS_Inflow                = round(inflow_swds, 3),
      Export_Outflow             = round(export_out, 3),
      Cum_Inflow_Total           = cum_tbl$Cum_Inflow_Total,
      Cum_Outflow_Total          = cum_outflow_total,
      Cum_Emitted_EnergyCap      = cum_tbl$Cum_Emitted_EnergyCap,
      Cum_Emitted_NoEnergyCap    = cum_tbl$Cum_Emitted_NoEnergyCap,
      check.names = FALSE
    )
  } else {
    cum_outflow_total <- cum_outflow_total_series(hwp, Y)
    
    out <- data.frame(
      Year                       = Y,
      Stock_Total                = round(stock_tot, 3),
      Stock_PIU                  = round(stock_piu, 3),
      Stock_SWDS                 = round(stock_sw, 3),
      Stock_Domestic             = round(stock_dom, 3),
      Stock_Imports              = round(nz0(stock_imp), 3),
      Domestic_Inflow            = round(inflow_dom, 3),
      SWDS_Inflow                = round(inflow_swds, 3),
      Export_Outflow             = round(export_out, 3),
      Cum_Inflow_Total           = NA_real_,
      Cum_Outflow_Total          = cum_outflow_total,
      Cum_Emitted_EnergyCap      = NA_real_,
      Cum_Emitted_NoEnergyCap    = NA_real_,
      check.names = FALSE
    )
  }
  
  if (!is.null(inflow_by_owner_mat) && nDom) {
    owner_cols <- as.data.frame(round(inflow_by_owner_mat, 3))
    colnames(owner_cols) <- paste0("Inflow_", dom_owners)
    out <- cbind(out, owner_cols)
  }
  
  if (!is.null(inflow_by_group_mat)) {
    group_cols <- as.data.frame(round(inflow_by_group_mat, 3))
    colnames(group_cols) <- colnames(inflow_by_group_mat)
    out <- cbind(out, group_cols)
  }
  
  jump_idx <- which(diff(out$Stock_Domestic) < -1e-6) + 1L
  if (length(jump_idx)) {
    message("Note: Stock_Domestic decreases at years: ",
            paste(out$Year[jump_idx], collapse = ", "),
            " (valid if retirements > inflows).")
  }
  
  print(out, row.names = FALSE)
  invisible(out)
}

# ---- Example one-year & full table ----
out_2022 <- print_hwp_stocks_2022_include_exports(hwp, 2022, domestic_mode = "A")
tbl_report <- hwp_report_table(hwp, year_from = 1940, year_to = 1980, metric_cum = "MMTC",
                               domestic_mode = "A")



# =========================================================
# Annual timber harvest by PRODUCT CATEGORY (EndUseID bins)
#   metric:  "MMTC" | "CO2e" | "BBF"  (arrays assumed in MMT C)
#   summary: "annual" | "cumulative"
#   mode:    "category_total" | "category" | "total"
#   keep_exports: if TRUE, Exports are kept and negated; if FALSE, removed
# Assumes helpers: .get_years(hwp), .get_ownerships(hwp)
# =========================================================

plot_ann_timber_by_enduse_bins <- function(
    hwp,
    metric       = c("MMTC","CO2e","BBF"),
    summary      = c("annual","cumulative"),
    mode         = c("category_total","category","total"),
    keep_exports = FALSE
) {
  metric  <- match.arg(metric)
  summary <- match.arg(summary)
  mode    <- match.arg(mode)
  
  # ---- Theme (local to function) ----
  common_theme <- ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title       = ggplot2::element_text(size = 24),
      axis.text        = ggplot2::element_text(size = 18),
      panel.border     = ggplot2::element_rect(color = "grey60", fill = NA, linewidth = 0.6),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.spacing    = grid::unit(1, "lines"),
      legend.position  = "top",
      legend.justification = "center",
      legend.box.just       = "center",
      legend.box            = "vertical",
      legend.title          = ggplot2::element_blank(),
      legend.text           = ggplot2::element_text(size = 16),
      legend.key.size       = grid::unit(22, "pt"),
      legend.key.width      = grid::unit(25, "pt"),
      legend.box.margin     = ggplot2::margin(8, 10, 0, 10),
      legend.spacing.x      = grid::unit(14, "pt"),
      plot.margin           = ggplot2::margin(22, 18, 10, 14)
    )
  
  # ---- axis helper ----
  .axis_pretty_local <- function(x, positive_only = FALSE, n = 6) {
    x <- x[is.finite(x)]
    if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
    rng <- range(x, na.rm = TRUE)
    if (!positive_only) rng[1] <- min(0, rng[1])
    br <- pretty(rng, n = n)
    list(min = min(br), max = max(br), by = diff(br)[1])
  }
  
  # ---- years ----
  years <- .get_years(hwp)
  
  # ----- collapse ownerships and get mat_enduse ----- #
  owns_raw    <- .get_ownerships(hwp)
  owns_clean  <- gsub("\\.", " ", owns_raw)
  idx_total   <- which(owns_clean == "Total")
  idx_exports <- which(owns_clean == "Exports")
  
  idx_keep <- setdiff(seq_along(owns_clean), idx_total)
  if (!keep_exports) idx_keep <- setdiff(idx_keep, idx_exports)
  
  eu <- hwp$eu_array[, idx_keep, , drop = FALSE]
  
  if (keep_exports && length(idx_exports)) {
    exp_in_keep <- which(owns_clean[idx_keep] == "Exports")
    if (length(exp_in_keep)) {
      eu[, exp_in_keep, ] <- -eu[, exp_in_keep, , drop = FALSE]
    }
  }
  
  mat_enduse <- t(apply(eu, c(1, 3), sum)) / 1e6
  
  if (length(idx_total)) {
    mat_total <- t(apply(hwp$eu_array[, idx_total, , drop = FALSE], c(1, 3), sum)) / 1e6
    pre_mask        <- years <= 1951
    zero_rows       <- rowSums(mat_enduse, na.rm = TRUE) == 0
    rows_to_replace <- pre_mask & zero_rows
    if (any(rows_to_replace)) {
      mat_enduse[rows_to_replace, ] <- mat_total[rows_to_replace, ]
    }
  }
  
  # ----- EndUseID → bins & Aggregation ----- #
  bin_defs <- list(
    "Fuel" = c(1, 48, 95, 142, 197),
    "Furniture" = c(5,20,28,42,52,68,81,84,99,116,121,139,144,155,165,186),
    "Housing and Construction" = c(
      10,21,27,36,57,65,73,91,104,115,125,132,145,164,170,183,
      12,14,24,37,56,69,78,86,100,108,128,134,146,162,169,184,
      11,15,26,46,58,62,79,85,102,114,133,152,154,168,185,
      8,17,29,39,54,70,75,90,103,111,123,138,147,158,175,181,
      7,18,34,40,59,67,72,93,101,113,127,140,148,160,167,178
    ),
    "Residential Repair and Remodeling"    = c(9,16,33,38,49,66,74,83,105,110,126,130,143,161,171,187),
    "Wood Packaging & Shipping"            = c(4,22,31,44,50,64,77,92,97,112,119,131,150,157,173,182),
    "Manufacturing Misc."                  = c(2,13,30,43,51,60,80,87,106,107,118,136,149,159,166,180),
    "Other Industrial Products"            = c(35,82,129,176),
    "Rail"                                 = c(3,19,25,41,53,63,71,89,96,117,122,137,153,163,172,177),
    "Paper Products"                       = c(47,94,141,188),
    "Softwood Misc."                       = c(206,204,216,222,208,198,220,194,212,214,200,192,190,196,218,224,202,210),
    "Hardwood Misc."                       = c(205,203,215,221,207,197,219,193,211,213,199,191,189,195,217,223,201,209),
    "Other, N.A."                          = c(6,23,32,45,55,61,76,88,98,109,120,135,151,156,174,179)
  )
  
  n_ids <- ncol(mat_enduse)
  if (is.null(n_ids)) n_ids <- 0
  if (n_ids == 0) stop("eu_array appears empty or has unexpected dimensions.")
  
  id_to_cat <- rep(NA_character_, n_ids)
  for (cat in names(bin_defs)) {
    ids  <- intersect(bin_defs[[cat]], seq_len(n_ids))
    if (!length(ids)) next
    free <- ids[is.na(id_to_cat[ids])]
    if (!length(free)) next
    
    if (cat %in% c("Other, N.A.", "Softwood Misc.", "Hardwood Misc.", "Rail")) {
      id_to_cat[free] <- "Other"
    } else {
      id_to_cat[free] <- cat
    }
  }
  
  keep_cols  <- which(!is.na(id_to_cat))
  if (!length(keep_cols)) stop("No EndUse IDs matched any bins.")
  
  id_to_cat  <- id_to_cat[keep_cols]
  mat_enduse <- mat_enduse[, keep_cols, drop = FALSE]
  
  present_cats <- unique(id_to_cat)
  cat_mat <- sapply(present_cats, function(cat) {
    cols <- which(id_to_cat == cat)
    rowSums(mat_enduse[, cols, drop = FALSE], na.rm = TRUE)
  })
  
  cat_mat <- as.matrix(cat_mat)
  rownames(cat_mat) <- years
  
  df <- as.data.frame(cat_mat)
  df$Year <- years
  
  long <- tidyr::pivot_longer(df, -Year, names_to = "Category", values_to = "Value")
  
  # cumulative option
  if (summary == "cumulative") {
    long <- long |>
      dplyr::group_by(Category) |>
      dplyr::mutate(Value = cumsum(Value)) |>
      dplyr::ungroup()
  }
  
  # metric conversion / label
  ylab <- "Annual HWP Carbon Input\n(MMT C)"
  if (metric == "CO2e") {
    long$Value <- long$Value * (44/12)
    ylab <- expression("MMT C"*O[2]*e)
  }
  if (metric == "BBF") {
    ylab <- "BBF"
  }
  
  # ---------- Data prep & REVERSED STACKING ORDER ---------- #
  plot_levels <- c(
    "Fuel", "Furniture", "Housing and Construction",
    "Residential Repair and Remodeling",
    "Wood Packaging & Shipping", "Manufacturing Misc.",
    "Other Industrial Products", "Paper Products", "Other"
  )
  present_levels <- intersect(plot_levels, unique(as.character(long$Category)))
  
  # Complete the grid (all Year × Category combos)
  long_base <- long |>
    tidyr::complete(Year, Category = present_levels, fill = list(Value = 0)) |>
    dplyr::filter(Category %in% present_levels)
  
  # Use *reversed* order for stacking (bottom → top)
  stack_levels <- rev(present_levels)
  
  long_stacked <- long_base |>
    dplyr::mutate(Category_stack = factor(Category, levels = stack_levels)) |>
    dplyr::arrange(Year, Category_stack) |>
    dplyr::group_by(Year) |>
    dplyr::mutate(
      ymax = cumsum(Value),
      ymin = ymax - Value
    ) |>
    dplyr::ungroup() |>
    # For aesthetics & legend we keep the original (non-reversed) order:
    dplyr::mutate(Category = factor(Category, levels = present_levels))
  
  # Small epsilon to keep ribbons visually connected when a band hits zero
  epsilon <- 1e-4
  idx_zero <- abs(long_stacked$ymax - long_stacked$ymin) < epsilon
  long_stacked$ymax[idx_zero] <- long_stacked$ymin[idx_zero] + epsilon
  
  # totals & axis env (use stacked data for axis limits)
  df_total <- long_stacked |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop")
  
  ax <- .axis_pretty_local(c(long_stacked$ymax, long_stacked$ymin), positive_only = FALSE)
  
  # Palette definitions
  pal_cat <- c(
    "Fuel"                              = "#F4D7A1",
    "Furniture"                         = "#E2B880",
    "Housing and Construction"          = "#D1965E",
    "Residential Repair and Remodeling" = "#B6763E",
    "Wood Packaging & Shipping"         = "#995521",
    "Manufacturing Misc."               = "#7B3A14",
    "Other Industrial Products"         = "#5C260C",
    "Paper Products"                    = "#3F1807",
    "Other"                             = "#1F0C03"
  )
  
  pat_cat <- c(
    "Fuel"                              = "none",
    "Furniture"                         = "circle",
    "Housing and Construction"          = "stripe",
    "Residential Repair and Remodeling" = "none",
    "Wood Packaging & Shipping"         = "circle",
    "Manufacturing Misc."               = "stripe",
    "Other Industrial Products"         = "none",
    "Paper Products"                    = "crosshatch",
    "Other"                             = "stripe"
  )
  
  pal_present <- pal_cat[present_levels]
  pat_present <- pat_cat[present_levels]
  pat_present[is.na(pat_present)] <- "none"
  
  # ---- Plotting ----
  if (mode %in% c("category","category_total")) {
    
    p <- ggplot2::ggplot() +
      ggpattern::geom_ribbon_pattern(
        data = long_stacked,
        ggplot2::aes(
          x       = Year,
          ymin    = ymin,
          ymax    = ymax,
          fill    = Category,
          pattern = Category
        ),
        pattern_fill    = "black",
        pattern_colour  = "black",
        pattern_alpha   = 0.8,
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        alpha           = 0.85,
        color           = "black",
        linewidth       = 0.2
      ) +
      ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
      ggplot2::scale_fill_manual(
        values = pal_present,
        breaks = present_levels
      ) +
      ggpattern::scale_pattern_manual(
        values = pat_present,
        breaks = present_levels
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(ax$min, ax$max, by = ax$by),
        limits = c(ax$min, ax$max),
        expand = c(0, 0)
      ) +
      ggplot2::scale_x_continuous(
        limits = c(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
      ) +
      ggplot2::labs(
        x = "Harvest Year",
        y = ylab,
        title = paste0(
          if (summary == "cumulative") "Cumulative" else "Annual",
          " C influx by product category"
        )
      ) +
      common_theme
    
    return(p)
  }
  
  # mode == "total"
  ggplot2::ggplot(df_total, ggplot2::aes(Year, Total)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::scale_y_continuous(
      breaks = seq(ax$min, ax$max, by = ax$by),
      limits = c(ax$min, ax$max),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
    ) +
    ggplot2::labs(
      x = "Harvest Year",
      y = ylab,
      title = paste0(
        if (summary == "cumulative") "Cumulative" else "Annual",
        " C influx (total)"
      )
    ) +
    common_theme
}


# Example call
p_cat <- plot_ann_timber_by_enduse_bins(
  hwp,
  metric  = "MMTC",
  summary = "annual",
  mode    = "category_total"
)
print(p_cat)







# =========================================================
# 1) Annual Harvest and Trade (AnnHarvestandTrade)
#    metric: "MMTC" | "CO2e" | "BBF"
#    summary: "annual" | "cumulative"
#    mode: "ownership_total" (stack owners + total line),
#          "ownership"       (stack owners only),
#          "total"           (total line only)
# Uses ggpattern + manual stacking (no white gaps).
# Positive stacks are in REVERSED ownership order,
# legend order stays Imports → ... → Exports.
# =========================================================

plot_ann_timber_harvest <- function(hwp,
                                    metric  = c("MMTC","CO2e","BBF"),
                                    summary = c("annual","cumulative"),
                                    mode    = c("ownership_total","ownership","total"),
                                    ownership_start_year = NULL,
                                    trade_start_year     = NULL) {
  
  metric  <- match.arg(metric)
  summary <- match.arg(summary)
  mode    <- match.arg(mode)
  
  # ---- Theme (internal) ----
  common_theme <- ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 38),
      axis.text  = ggplot2::element_text(size = 28),
      panel.border = ggplot2::element_rect(color = "grey60", fill = NA, linewidth = 0.6),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.spacing = grid::unit(1, "lines"),
      legend.position = "top",
      legend.justification = "center",
      legend.box.just = "center",
      legend.box = "horizontal",     # one-row legends
      legend.title = ggplot2::element_blank(),
      legend.text  = ggplot2::element_text(size = 26),
      legend.key.size = grid::unit(22, "pt"),
      legend.key.width = grid::unit(25, "pt"),
      legend.box.margin = ggplot2::margin(8, 10, 0, 10),
      legend.spacing.x = grid::unit(14, "pt"),
      plot.margin = ggplot2::margin(22, 18, 10, 14)
    )
  
  years    <- .get_years(hwp)
  owns_raw <- .get_ownerships(hwp)
  
  # Labels & ordering
  owns_lab          <- gsub("\\.", " ", owns_raw)
  owns_lab_no_total <- setdiff(owns_lab, "Total")
  presplit_name     <- "All ownerships (pre-1952)"
  
  base_order   <- c("Imports","BLM","USFS","State","Private and Tribal", presplit_name, "Exports")
  legend_order <- intersect(base_order, c(owns_lab_no_total, presplit_name))
  
  # ---- Build ownership matrix as NUMERIC MATRIX (MMT C) ----
  mat <- t(apply(hwp$eu_array, c(2,3), sum)) / 1e6    # [year, owner]
  colnames(mat) <- owns_lab
  storage.mode(mat) <- "double"
  
  # ---- Infer / override start years ----
  owners_cols_no_total_no_exports <- setdiff(colnames(mat), c("Total","Exports"))
  owners_sum <- if (length(owners_cols_no_total_no_exports)) {
    rowSums(mat[, owners_cols_no_total_no_exports, drop = FALSE], na.rm = TRUE)
  } else rep(0, length(years))
  
  inferred_own_start <- if (any(owners_sum > 0, na.rm = TRUE)) min(years[owners_sum > 0], na.rm = TRUE) else Inf
  inferred_trade_start <- if ("Imports" %in% colnames(mat) && any(mat[, "Imports"] > 0, na.rm = TRUE)) {
    min(years[mat[, "Imports"] > 0], na.rm = TRUE)
  } else Inf
  
  own_start   <- ownership_start_year %||% inferred_own_start
  trade_start <- trade_start_year     %||% inferred_trade_start
  if (!is.finite(own_start))   own_start   <- Inf
  if (!is.finite(trade_start)) trade_start <- Inf
  
  # ---- Ensure Exports column; derive only for valid years if Total exists ----
  if (!("Exports" %in% colnames(mat))) {
    mat <- cbind(mat, Exports = numeric(nrow(mat)))
  } else {
    mat[, "Exports"] <- 0
  }
  
  if ("Total" %in% colnames(mat)) {
    total_owner <- mat[, "Total"]
    others      <- setdiff(colnames(mat), c("Total","Exports"))
    idx_others  <- match(others, colnames(mat)); idx_others <- idx_others[!is.na(idx_others)]
    sum_others  <- if (length(idx_others)) rowSums(mat[, idx_others, drop = FALSE], na.rm = TRUE) else rep(0, nrow(mat))
    valid_year  <- years >= max(own_start, trade_start)
    derived_exports <- pmax(total_owner - sum_others, 0)
    derived_exports[!valid_year] <- 0
    mat[, "Exports"] <- derived_exports
  }
  
  # ---- Add pre-1952 band as a NUMERIC column; compute remainder safely ----
  if (!(presplit_name %in% colnames(mat))) {
    presplit_col <- matrix(0, nrow(mat), 1)
    colnames(presplit_col) <- presplit_name
    mat <- cbind(mat, presplit_col)            # stays a numeric matrix
  } else {
    mat[, presplit_name] <- as.numeric(mat[, presplit_name])
  }
  
  known_cols <- setdiff(colnames(mat), c("Total", presplit_name))
  idx_known  <- match(known_cols, colnames(mat)); idx_known <- idx_known[!is.na(idx_known)]
  sum_known  <- if (length(idx_known)) rowSums(mat[, idx_known, drop = FALSE], na.rm = TRUE) else rep(0, nrow(mat))
  
  total_for_remainder <- if ("Total" %in% colnames(mat)) {
    mat[, "Total"]
  } else {
    idx_base <- which(colnames(mat) != presplit_name)
    rowSums(mat[, idx_base, drop = FALSE], na.rm = TRUE)
  }
  
  remainder      <- pmax(total_for_remainder - sum_known, 0)
  mask_presplit  <- years < own_start
  mat[mask_presplit, presplit_name] <- remainder[mask_presplit]
  
  # ---- Long df (drop Total, drop NA ownership) ----
  own_stack <- as.data.frame(mat)
  own_stack$Year <- years
  own_stack <- tidyr::pivot_longer(own_stack, -Year,
                                   names_to = "Ownership",
                                   values_to = "Value") |>
    dplyr::filter(!is.na(Ownership), Ownership != "Total")
  
  # cumulative option
  if (summary == "cumulative") {
    own_stack <- own_stack |>
      dplyr::group_by(Ownership) |>
      dplyr::mutate(Value = cumsum(Value)) |>
      dplyr::ungroup()
  }
  
  # Exports below zero
  if ("Exports" %in% unique(own_stack$Ownership)) {
    own_stack$Value[own_stack$Ownership == "Exports"] <-
      -own_stack$Value[own_stack$Ownership == "Exports"]
  }
  
  # Metric label/convert if CO2e
  ylab <- "MMT C"
  if (metric == "CO2e") {
    own_stack$Value <- own_stack$Value * (44/12)
    ylab <- expression("MMT C"*O[2]*e)
  }
  
  # Split: pre-1952 band vs. ownerships with categories
  avail_all   <- intersect(legend_order, unique(own_stack$Ownership))
  main_levels <- setdiff(avail_all, presplit_name)
  
  df_pre  <- dplyr::filter(own_stack, Ownership == presplit_name)
  df_main <- dplyr::filter(own_stack, Ownership != presplit_name)
  
  # Ensure main Ownership factor for legend/palettes
  if (nrow(df_main)) {
    df_main$Ownership <- factor(df_main$Ownership, levels = main_levels)
  }
  
  # ---- CUSTOM OWNERSHIP PALETTE AND PATTERNS ----
  pal_base <- c(
    "#DCC7A0", # Imports (light)
    "#C08A4A", # BLM
    "#9B5A18", # USFS
    "#723A10", # State
    "#4C260B", # Private and Tribal
    "#2E1507"  # Exports
  )
  pal <- setNames(pal_base[seq_along(main_levels)], main_levels)
  
  pattern_vec <- c(
    "Imports"          = "none",
    "BLM"              = "crosshatch",
    "USFS"             = "stripe",
    "State"            = "circle",
    "Private and Tribal" = "crosshatch",
    "Exports"          = "none"
  )
  pattern_scheme <- pattern_vec[main_levels]
  pattern_scheme[is.na(pattern_scheme)] <- "none"
  
  # Reconciled total (sum of stack with negatives)
  df_total <- own_stack |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop")
  
  # Axis allowing negatives
  yr_env <- own_stack |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      pos = sum(pmax(Value, 0), na.rm = TRUE),
      neg = sum(pmin(Value, 0), na.rm = TRUE),
      .groups = "drop"
    )
  ax <- .axis_pretty(c(yr_env$pos, yr_env$neg), positive_only = FALSE)
  
  # ===== Manual stacking for patterns =====
  
  # Split positive vs negative contributions
  df_pos <- df_main |> dplyr::filter(Value >= 0)
  df_neg <- df_main |> dplyr::filter(Value <  0)
  
  # --- Positive stack: REVERSED order relative to main_levels ---
  pos_order <- rev(main_levels)  # <- controls visual stacking only
  if (nrow(df_pos)) {
    df_pos <- df_pos |>
      dplyr::mutate(Ownership = factor(Ownership, levels = pos_order)) |>
      dplyr::arrange(Year, Ownership) |>
      dplyr::group_by(Year) |>
      dplyr::mutate(
        ymax = cumsum(Value),
        ymin = ymax - Value
      ) |>
      dplyr::ungroup()
  }
  
  # --- Negative stack (e.g., Exports) ---
  if (nrow(df_neg)) {
    df_neg <- df_neg |>
      dplyr::mutate(
        Ownership = factor(Ownership, levels = main_levels),
        pos = -Value
      ) |>
      dplyr::arrange(Year, Ownership) |>
      dplyr::group_by(Year) |>
      dplyr::mutate(
        cum  = cumsum(pos),
        ymin = -cum,
        ymax = ymin + pos
      ) |>
      dplyr::ungroup()
  }
  
  df_ribbons <- dplyr::bind_rows(df_pos, df_neg)
  
  # ---- Plot ----
  if (mode %in% c("ownership","ownership_total")) {
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.25)
    
    # pre-1952 band: invisible (line only)
    if (nrow(df_pre)) {
      p <- p +
        ggplot2::geom_area(
          data = df_pre,
          ggplot2::aes(Year, Value),
          inherit.aes = FALSE,
          fill = "transparent",
          color = NA,
          alpha = 0
        )
    }
    
    # patterned ownership ribbons
    if (nrow(df_ribbons)) {
      p <- p +
        ggpattern::geom_ribbon_pattern(
          data = df_ribbons,
          ggplot2::aes(
            x       = Year,
            ymin    = ymin,
            ymax    = ymax,
            fill    = Ownership,
            pattern = Ownership
          ),
          pattern_fill    = "black",
          pattern_colour  = "black",
          pattern_alpha   = 0.8,
          pattern_density = 0.1,
          pattern_spacing = 0.02,
          alpha           = 0.85,
          color           = "white",
          linewidth       = 0.2
        ) +
        ggplot2::scale_fill_manual(
          values = pal,
          breaks = main_levels,
          name   = "Ownership"
        ) +
        ggpattern::scale_pattern_manual(
          values = pattern_scheme,
          breaks = main_levels,
          name   = "Ownership"
        )
    }
    
    p <- p +
      ggplot2::scale_y_continuous(
        breaks = seq(ax$min, ax$max, by = ax$by),
        limits = c(ax$min, ax$max),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        x = "Harvest Year",
        y = ylab,
        title = paste(
          summary,
          "timber harvest",
          if (mode == "ownership_total") "by ownership + total" else "by ownership"
        )
      ) +
      common_theme
    
    if (mode == "ownership_total") {
      p <- p +
        ggplot2::geom_line(
          data = df_total,
          ggplot2::aes(Year, Total, color = "Total"),
          linewidth = 1.25,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_color_manual(values = c(Total = "black"), name = NULL)
    }
    
    return(p)
  }
  
  # mode == "total"
  ggplot2::ggplot(df_total, ggplot2::aes(Year, Total)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.0) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::scale_y_continuous(
      breaks = seq(ax$min, ax$max, by = ax$by),
      limits = c(ax$min, ax$max),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Harvest Year",
      y = ylab,
      title = paste(summary, "timber harvest (total)")
    ) +
    common_theme
}


# Example call
p1 <- plot_ann_timber_harvest(
  hwp,
  metric  = "MMTC",
  summary = "annual",
  mode    = "ownership_total",
  ownership_start_year = 1952,
  trade_start_year     = 1965
)
print(p1)




# =========================================================
# 2) Annual Net Change in Carbon Storage (Production/Simple Decay)
#    approach:   "production" or "simple_decay"
#    metrictype: "MMTC" | "CO2e"
#    Notes:
#      • Production approach = show stock changes (PU + SWDS) only.
#      • Simple-decay approach = stack annual inflow (consumption) vs. emissions.
#        Consumption inflow = Domestic harvest (+ Imports − Exports) from eu_array.
#      • Imports are positive; Exports are negative in the stack (drawn below zero).
# =========================================================

plot_annual_net_change <- function(
    hwp,
    approach   = c("production","simple_decay"),
    metrictype = c("MMTC","CO2e"),
    include_net_line = TRUE
) {
  approach   <- match.arg(approach)
  metrictype <- match.arg(metrictype)
  
  if (!exists(".axis_pretty", mode = "function")) {
    .axis_pretty <- function(x, positive_only = FALSE, n = 6) {
      x <- x[is.finite(x)]
      if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
      rng <- range(x, na.rm = TRUE)
      if (positive_only) rng[1] <- min(0, rng[1])
      br <- pretty(rng, n = n)
      list(min = min(br), max = max(br), by = diff(br)[1])
    }
  }
  
  years    <- .get_years(hwp)
  id_total <- .idx_total(hwp)
  
  psum_dim2 <- function(arr) apply(arr[, id_total, , drop = FALSE], 3, sum, na.rm = TRUE)
  
  pu  <- psum_dim2(hwp$pu.final_array)   / 1e6
  sw  <- psum_dim2(hwp$swdsCtotal_array) / 1e6
  eec <- psum_dim2(hwp$eec_array)        / 1e6
  ewo <- psum_dim2(hwp$ewoec_array)      / 1e6
  
  pu_ch    <- diff(pu)
  sw_ch    <- diff(sw)
  eec_ch   <- eec[-1]
  ewoec_ch <- ewo[-1]
  
  df <- data.frame(
    Year        = years[-1],
    SWDSchange  = sw_ch,
    PUchange    = pu_ch,
    EECchange   = -eec_ch,
    EWOECchange = -ewoec_ch,
    stringsAsFactors = FALSE
  )
  
  df$Net    <- df$SWDSchange + df$PUchange
  df$Inflow <- df$PUchange + df$SWDSchange - df$EECchange - df$EWOECchange
  
  ylab <- if (metrictype == "CO2e") {
    df[, setdiff(names(df), "Year")] <- df[, setdiff(names(df), "Year")] * (44/12)
    "MMT CO2e"
  } else "MMT C"
  
  if (approach == "production") {
    # Build stacked data with PIU then SWDS
    bar <- rbind(
      data.frame(Year = df$Year, series = "PUchange",   val = df$PUchange,   stringsAsFactors = FALSE),
      data.frame(Year = df$Year, series = "SWDSchange", val = df$SWDSchange, stringsAsFactors = FALSE)
    )
    # Factor order controls bottom->top when we use reverse=TRUE below
    bar$series <- factor(bar$series, levels = c("PUchange","SWDSchange"))
    
    ax <- .axis_pretty(c(bar$val, if (isTRUE(include_net_line)) df$Net), positive_only = FALSE)
    
    p <- ggplot(bar, aes(Year, val, fill = series)) +
      geom_col(position = position_stack(reverse = TRUE)) +  # <-- PIU bottom, SWDS top
      geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
      scale_fill_manual(
        limits = c("PUchange","SWDSchange"),                 # keeps intended order
        values = c(SWDSchange = "#B42E8D", PUchange = "#7801A8"),
        breaks = c("SWDSchange","PUchange"),
        labels = c("Solid Waste Disposal Sites", "Products in Use"),
        name   = NULL
      ) +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                         limits = c(ax$min, ax$max), expand = c(0, 0)) +
      labs(x = "Year", y = ylab,
           title = "Annual Net Change in Carbon Storage -- Production Approach") +
      theme_bw(base_size = 14)
    
    if (isTRUE(include_net_line)) {
      p <- p +
        geom_line(data = df, aes(Year, Net, color = "Net"),
                  linewidth = 1.1, inherit.aes = FALSE) +
        scale_color_manual(values = c(Net = "#3CB371"), name = NULL)
    }
    return(p)
  }
  
  # Simple-decay branch
  bar <- rbind(
    data.frame(Year = df$Year, series = "Inflow",        val = df$Inflow,        stringsAsFactors = FALSE),
    data.frame(Year = df$Year, series = "EWOECchange",   val = df$EWOECchange,   stringsAsFactors = FALSE),
    data.frame(Year = df$Year, series = "EECchange",     val = df$EECchange,     stringsAsFactors = FALSE)
  )
  bar$series <- factor(bar$series, levels = c("Inflow","EECchange","EWOECchange"))
  
  ax <- .axis_pretty(c(bar$val, if (isTRUE(include_net_line)) df$Net), positive_only = FALSE)
  
  p <- ggplot(bar, aes(Year, val, fill = series)) +
    geom_col() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    scale_fill_manual(
      values = c(Inflow = "#00CED1", EWOECchange = "#F99A3E", EECchange = "#F8DF25"),
      breaks = c("Inflow","EECchange","EWOECchange"),
      labels = c("Consumption inflow (Domestic + Imports - Exports)",
                 "Emitted with Energy Capture",
                 "Emitted without Energy Capture"),
      name = NULL
    ) +
    scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                       limits = c(ax$min, ax$max), expand = c(0, 0)) +
    labs(x = "Year", y = ylab,
         title = "Annual Net Change in Carbon Storage -- Simple-decay Approach") +
    theme_bw(base_size = 14)
  
  if (isTRUE(include_net_line)) {
    p <- p +
      geom_line(data = df, aes(Year, Net, color = "Net"),
                linewidth = 1.1, inherit.aes = FALSE) +
      scale_color_manual(values = c(Net = "#3CB371"), name = NULL)
  }
  return(p)
}



# ---- Example: Production approach with Net line ----
p2 <- plot_annual_net_change(
  hwp,
  approach       = "production",
  metrictype     = "MMTC",
  include_net_line = TRUE
)
print(p2)

# ---- Example: Simple Decay approach with Net line ----



p2 <- plot_annual_net_change(
  hwp,
  approach         = "simple_decay",
  metrictype       = "MMTC",
  include_net_line = TRUE
) +
  coord_cartesian(ylim = c(-15, 30)) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6)  # optional: nice tick spacing
  )
print(p2); save_plot_png(p2, "Plot_AnNetChCStor_SimpleDecay.png")





# ============= TRANSPARENT PRE-2001 ===========
plot_annual_net_change <- function(
    hwp,
    approach   = c("production","simple_decay"),
    metrictype = c("MMTC","CO2e"),
    include_net_line = TRUE,
    transparent_before = 1965L
) {
  approach   <- match.arg(approach)
  metrictype <- match.arg(metrictype)
  cut_year   <- as.integer(transparent_before)
  
  # --- private helpers (avoid clashes) ---
  .axis_pretty_local <- function(x, n = 6) {
    x <- x[is.finite(x)]
    if (!length(x)) x <- 0
    rng <- range(c(0, x), na.rm = TRUE)
    br  <- pretty(rng, n = n)
    list(min = min(br), max = max(br), by = diff(br)[1])
  }
  .get_total_idx <- function(arr) {
    nm <- try(dimnames(arr)[[2]], silent = TRUE)
    if (!inherits(nm, "try-error") && !is.null(nm)) {
      w <- which(trimws(nm) == "Total")
      if (length(w)) return(w)
    }
    ncol(arr) # fallback
  }
  .psum_dim2_total <- function(arr, id_total) {
    apply(arr[, id_total, , drop = FALSE], 3, sum, na.rm = TRUE)
  }
  
  # --- pull series (MMT C) ---
  years <- .get_years(hwp)
  stopifnot(!is.null(hwp$pu.final_array), !is.null(hwp$swdsCtotal_array),
            !is.null(hwp$eec_array), !is.null(hwp$ewoec_array))
  id_total <- .get_total_idx(hwp$pu.final_array)
  
  pu  <- .psum_dim2_total(hwp$pu.final_array,   id_total) / 1e6
  sw  <- .psum_dim2_total(hwp$swdsCtotal_array, id_total) / 1e6
  eec <- .psum_dim2_total(hwp$eec_array,        id_total) / 1e6
  ewo <- .psum_dim2_total(hwp$ewoec_array,      id_total) / 1e6
  
  # year-over-year deltas align to years[-1]
  df <- data.frame(
    Year        = years[-1],
    SWDSchange  = diff(sw),
    PUchange    = diff(pu),
    EECchange   = -eec[-1],
    EWOECchange = -ewo[-1],
    stringsAsFactors = FALSE
  )
  df$Net    <- df$SWDSchange + df$PUchange
  df$Inflow <- df$PUchange + df$SWDSchange - df$EECchange - df$EWOECchange
  
  # metric conversion & label
  ylab <- if (metrictype == "CO2e") {
    df[names(df) != "Year"] <- lapply(df[names(df) != "Year"], `*`, 44/12)
    "MMT CO2e"
  } else "MMT C"
  
  if (approach == "production") {
    bar <- rbind(
      data.frame(Year = df$Year, series = "PUchange",   val = df$PUchange),
      data.frame(Year = df$Year, series = "SWDSchange", val = df$SWDSchange)
    )
    bar$series <- factor(bar$series, levels = c("PUchange","SWDSchange"))
    bar_pre    <- subset(bar, Year <  cut_year)
    bar_post   <- subset(bar, Year >= cut_year)
    
    ax <- .axis_pretty_local(c(bar$val, if (isTRUE(include_net_line)) df$Net))
    
    p <- ggplot2::ggplot(bar, ggplot2::aes(Year, val, fill = series)) +
      # transparent pre-2001 bars (no legend)
      ggplot2::geom_col(data = bar_pre,  position = ggplot2::position_stack(reverse = TRUE),
                        alpha = 0, color = NA, show.legend = FALSE) +
      # opaque 2001+ bars
      ggplot2::geom_col(data = bar_post, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
      ggplot2::scale_fill_manual(
        limits = c("PUchange","SWDSchange"),
        values = c(SWDSchange = "#B42E8D", PUchange = "#7801A8"),
        breaks = c("SWDSchange","PUchange"),
        labels = c("Solid Waste Disposal Sites", "Products in Use"),
        name   = NULL
      ) +
      ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                  limits = c(ax$min, ax$max), expand = c(0, 0)) +
      ggplot2::labs(x = "Year", y = ylab,
                    title = "Annual Net Change in Carbon Storage -- Production Approach") +
      ggplot2::theme_bw(base_size = 14)
    
    if (isTRUE(include_net_line)) {
      p <- p +
        ggplot2::geom_line(data = df, ggplot2::aes(Year, Net, color = "Net"),
                           linewidth = 1.1, inherit.aes = FALSE) +
        ggplot2::scale_color_manual(values = c(Net = "#3CB371"), name = NULL)
    }
    return(p)
  }
  
  # simple_decay
  bar <- rbind(
    data.frame(Year = df$Year, series = "Inflow",        val = df$Inflow),
    data.frame(Year = df$Year, series = "EWOECchange",   val = df$EWOECchange),
    data.frame(Year = df$Year, series = "EECchange",     val = df$EECchange)
  )
  bar$series <- factor(bar$series, levels = c("Inflow","EECchange","EWOECchange"))
  bar_pre    <- subset(bar, Year <  cut_year)
  bar_post   <- subset(bar, Year >= cut_year)
  
  ax <- .axis_pretty_local(c(bar$val, if (isTRUE(include_net_line)) df$Net))
  
  p <- ggplot2::ggplot(bar, ggplot2::aes(Year, val, fill = series)) +
    ggplot2::geom_col(data = bar_pre,  alpha = 0, color = NA, show.legend = FALSE) +
    ggplot2::geom_col(data = bar_post) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::scale_fill_manual(
      values = c(Inflow = "#00CED1", EWOECchange = "#F99A3E", EECchange = "#F8DF25"),
      breaks = c("Inflow","EECchange","EWOECchange"),
      labels = c("Consumption inflow (Domestic + Imports - Exports)",
                 "Emitted with Energy Capture",
                 "Emitted without Energy Capture"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                limits = c(ax$min, ax$max), expand = c(0, 0)) +
    ggplot2::labs(x = "Year", y = ylab,
                  title = "Annual Net Change in Carbon Storage -- Simple-decay Approach") +
    ggplot2::theme_bw(base_size = 14)
  
  if (isTRUE(include_net_line)) {
    p <- p +
      ggplot2::geom_line(data = df, ggplot2::aes(Year, Net, color = "Net"),
                         linewidth = 1.1, inherit.aes = FALSE) +
      ggplot2::scale_color_manual(values = c(Net = "#3CB371"), name = NULL)
  }
  return(p)
}



p_prod <- plot_annual_net_change(hwp, approach="production",   metrictype="MMTC", include_net_line=TRUE)
p_sd   <- plot_annual_net_change(hwp, approach="simple_decay", metrictype="MMTC", include_net_line=TRUE)
print(p_prod); print(p_sd)





# =========================================================
# 3) Harvest by Functional Lifespan
#    type: "absolute" (MMTC/CO2e) | "proportion"
# =========================================================
plot_harvest_by_functional_lifespan <- function(
    hwp = NULL,
    eu_half_lives = NULL,   # data.frame like EU_HalfLives (cols: EndUseID, EU_HalfLife, ...)
    eu_array      = NULL,   # 3D array [EndUse, Ownership, Year] in metric tonnes C
    years         = NULL,   # numeric vector of model years (dim 3 of eu_array)
    ownership.names = NULL, # character vector of ownership names (dim 2 of eu_array)
    type = c("absolute","proportion"),
    metrictype = c("MMTC","CO2e")
) {
  type <- match.arg(type)
  metrictype <- match.arg(metrictype)
  
  # ---- Resolve inputs ----
  if (!is.null(hwp)) {
    if (is.null(eu_half_lives))     eu_half_lives   <- hwp$eu_half.lives.hwp
    if (is.null(eu_array))          eu_array        <- hwp$eu_array
    if (is.null(years))             years           <- hwp$years
    if (is.null(ownership.names))   ownership.names <- hwp$ownership.names
  }
  if (is.null(eu_half_lives))     eu_half_lives   <- get0("eu_half.lives.hwp", ifnotfound = NULL)
  if (is.null(eu_array))          eu_array        <- get0("model.outputs", ifnotfound = list())$eu_array
  if (is.null(years))             years           <- get0("years", ifnotfound = NULL)
  if (is.null(ownership.names))   ownership.names <- get0("ownership.names", ifnotfound = NULL)
  
  # ---- Sanity checks ----
  if (is.null(eu_half_lives))   stop("eu_half_lives (EU_HalfLives) is required.")
  if (is.null(eu_array))        stop("eu_array (model.outputs$eu_array) is required.")
  if (is.null(years))           stop("years vector is required.")
  if (is.null(ownership.names)) stop("ownership.names is required.")
  if (!all(c("EndUseID","EU_HalfLife") %in% names(eu_half_lives))) {
    stop("EU_HalfLives must contain columns: EndUseID and EU_HalfLife.")
  }
  
  # ---- Index of 'Total' ownership ----
  id_total <- match("Total", ownership.names)
  if (is.na(id_total)) stop("'Total' ownership not found in ownership.names.")
  
  # ---- Classify EU half-lives ----
  sml <- eu_half_lives %>%
    mutate(
      ShortMedLong = case_when(
        EU_HalfLife <= 0            ~ "Fuel",
        EU_HalfLife <= 6            ~ "Short",
        EU_HalfLife <= 30           ~ "Medium",
        EU_HalfLife > 30            ~ "Long",
        TRUE                        ~ "Fuel"
      )
    )
  
  # ---- Attach EU flows for Total ownership ----
  eu_tot <- as.data.frame(eu_array[, id_total, , drop = FALSE][, , seq_along(years)])
  colnames(eu_tot) <- years
  
  # ---- Long form & aggregate ----
  dat <- bind_cols(sml, eu_tot) %>%
    pivot_longer(cols = all_of(as.character(years)),
                 names_to = "Year", values_to = "MMTC") %>%
    mutate(Year = as.numeric(Year)) %>%
    group_by(ShortMedLong, Year) %>%
    summarise(MMTC = sum(MMTC, na.rm = TRUE), .groups = "drop")
  
  totals <- dat %>%
    group_by(Year) %>%
    summarise(Total = sum(MMTC, na.rm = TRUE), .groups = "drop")
  
  # ---- Units helpers ----
  convert_units <- function(val, out = c("MMTC","CO2e")) {
    out <- match.arg(out)
    if (out == "CO2e") (val/1e6) * (44/12) else val/1e6
  }
  lab_units <- function(out = c("MMTC","CO2e")) {
    out <- match.arg(out)
    if (out == "CO2e") "MMT CO\u2082e / yr" else "MMT C / yr"
  }
  axis_pretty <- function(x) {
    rng <- range(x, na.rm = TRUE)
    brk <- pretty(rng)
    by  <- if (length(brk) >= 2) brk[2] - brk[1] else max(rng, 1)
    list(min = 0, max = max(0, ceiling(max(rng, 0)/by) * by), by = by)
  }
  
  # ---- Final plotting data ----
  out <- dat %>%
    left_join(totals, by = "Year") %>%
    mutate(
      value_abs  = convert_units(MMTC, metrictype),
      value_prop = if_else(Total > 0, MMTC/Total, 0),
      ShortMedLong = factor(ShortMedLong, levels = c("Fuel","Short","Medium","Long"))
    )
  
  # ---- Plot ----
  if (type == "absolute") {
    ax <- out %>%
      group_by(Year) %>%
      summarise(Tot = sum(value_abs, na.rm = TRUE), .groups = "drop") %>%
      pull(Tot) %>%
      axis_pretty()
    
    ggplot(out, aes(Year, value_abs, fill = ShortMedLong)) +
      geom_area(color = "white", linewidth = 0.3) +
      scale_fill_viridis_d(
        option = "D",
        end = 1,
        name = "Functional Lifespan",
        labels = c("Fuel (instant)", "Short (1–6 yr)", "Medium (7–30 yr)", "Long (31+ yr)")
      ) +
      scale_y_continuous(
        breaks = seq(ax$min, ax$max, by = ax$by),
        limits = c(ax$min, ax$max),
        expand = c(0, 0)
      ) +
      labs(
        x = "Harvest Year",
        y = lab_units(metrictype),
        title = "Annual carbon allocation by lifespan (harvest + imports)"
      ) +
      theme_bw(base_size = 14)
  } else {
    ggplot(out, aes(Year, value_prop, fill = ShortMedLong)) +
      geom_area(color = "white", linewidth = 0.3) +
      scale_fill_viridis_d(
        option = "D",
        end = 1,
        name = "Functional Lifespan",
        labels = c("Fuel (instant)", "Short (1–6 yr)", "Medium (7–30 yr)", "Long (31+ yr)")
      ) +
      scale_y_continuous(labels = percent, limits = c(0, 1), expand = c(0, 0)) +
      labs(
        x = "Harvest Year",
        y = "Share of annual harvest",
        title = "Proportional allocation of harvested carbon by functional lifespan"
      ) +
      theme_bw(base_size = 14)
  }
}

# Example call
p3 <- plot_harvest_by_functional_lifespan(
  eu_half_lives   = eu_half.lives.hwp,
  eu_array        = model.outputs$eu_array,
  years           = years,
  ownership.names = ownership.names,
  type            = "absolute",
  metrictype      = "MMTC"   # <-- use MMTC
)
print(p3)




# =========================================================
# 4) Cumulative carbon stored by PRODUCT TYPE (End Use) — MMT C
# pools: "both" | "piu" | "swds"
# Notes:
# - Uses the "Total" ownership slice if present; otherwise sums across all owners.
# - Colors are paired: PIU = base, SWDS = lighter shade of the same color.
# - Expects arrays in metric tonnes C; converts to MMT C.
# Arrays used from `hwp`: pu.final_array [EndUse, Ownership, Year]
#                      swdsCtotal_array [EndUse, Ownership, Year]
# =========================================================

# ** REQUIRED LIBRARY **
# library(ggpattern)

# ---- Theme ----
common_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 24),
    axis.text  = element_text(size = 18),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(22, "pt"),
    legend.key.width = unit(25, "pt"),
    legend.box.margin = margin(8, 10, 0, 10),
    legend.spacing.x = unit(14, "pt"),
    plot.margin = margin(22, 18, 10, 14)
  )

plot_carbon_storage_by_product_category <- function(
    hwp,
    pools  = c("both","piu","swds"),
    metric = c("MMTC","CO2e")
) {
  pools  <- match.arg(pools)
  metric <- match.arg(metric)
  
  # ---- inputs ----
  pu   <- hwp$pu.final_array
  swds <- hwp$swdsCtotal_array
  if (is.null(pu) || is.null(swds))
    stop("hwp$pu.final_array and hwp$swdsCtotal_array are required.")
  
  # ---- years: use the same helper as other plotting fns ----
  years <- .get_years(hwp)
  
  # helper to clean ownership names
  clean <- function(x) gsub("\\.", " ", trimws(x))
  
  # Sum over ownership; prefer 'Total' if present
  sum_total_or_owners <- function(arr) {
    own <- clean(dimnames(arr)[[2]])
    idx_total <- which(own == "Total")
    mat <- if (length(idx_total)) {
      # [EndUseID, Year]
      apply(arr[, idx_total, , drop = FALSE], c(1, 3), sum, na.rm = TRUE)
    } else {
      apply(arr, c(1, 3), sum, na.rm = TRUE)
    }
    mat / 1e6
  }
  
  eu_piu  <- sum_total_or_owners(pu)    # [EndUseID, Year]
  eu_swds <- sum_total_or_owners(swds)  # [EndUseID, Year]
  
  # ---- Combine pools if requested ----
  eu_year <- switch(
    pools,
    piu  = eu_piu,
    swds = eu_swds,
    both = eu_piu + eu_swds
  )
  
  # Align years length with number of time steps if needed
  if (length(years) != ncol(eu_year)) {
    if (length(years) > ncol(eu_year)) {
      years <- years[seq_len(ncol(eu_year))]
    } else {
      stop("Length of 'years' does not match number of time steps in eu_year.")
    }
  }
  
  # ---- bins (Fuel, Rail, Softwood/Hardwood Misc. removed) ----
  n_eu  <- nrow(eu_year)         # number of EndUse IDs available
  clamp <- function(v) intersect(v, seq_len(n_eu))
  
  bin_defs <- list(
    "Furniture" = c(5,20,28,42,52,68,81,84,99,116,121,139,144,155,165,186),
    "Housing and Construction" = c(
      10,21,27,36,57,65,73,91,104,115,125,132,145,164,170,183,
      12,14,24,37,56,69,78,86,100,108,128,134,146,162,169,184,
      11,15,26,46,58,62,79,85,102,114,133,152,154,168,185,
      8,17,29,39,54,70,75,90,103,111,123,138,147,158,175,181,
      7,18,34,40,59,67,72,93,101,113,127,140,148,160,167,178
    ),
    "Residential Repair and Remodeling" = c(9,16,33,38,49,66,74,83,105,110,126,130,143,161,171,187),
    "Wood Packaging & Shipping" = c(4,22,31,44,50,64,77,92,97,112,119,131,150,157,173,182),
    "Manufacturing Misc." = c(2,13,30,43,51,60,80,87,106,107,118,136,149,159,166,180),
    "Other Industrial Products" = c(35,82,129,176),
    "Paper Products" = c(47,94,141,188),
    "Other" = c(6,23,32,45,55,61,76,88,98,109,120,135,151,156,174,179)
  )
  
  bin_defs <- lapply(bin_defs, clamp)
  bin_defs <- bin_defs[vapply(bin_defs, length, 1L) > 0]
  if (!length(bin_defs)) stop("No EndUse IDs matched.")
  
  cat_mat <- sapply(names(bin_defs), function(cat) {
    colSums(eu_year[bin_defs[[cat]], , drop = FALSE], na.rm = TRUE)
  })
  
  cat_df      <- as.data.frame(cat_mat)
  cat_df$Year <- years
  long <- tidyr::pivot_longer(cat_df, -Year, names_to = "Category", values_to = "Value")
  
  # ---- metric ----
  ylab <- "HWP Carbon Stock (MMT C)"
  if (metric == "CO2e") {
    long$Value <- long$Value * (44/12)
    ylab <- expression("MMT C"*O[2]*e)
  }
  
  axis_pretty <- function(x) {
    x <- x[is.finite(x)]
    br <- pretty(range(x, na.rm = TRUE))
    list(min = min(br), max = max(br), by = diff(br)[1])
  }
  
  ax <- long |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Tot = sum(Value, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(Tot) |>
    axis_pretty()
  
  plot_levels <- c(
    "Furniture","Housing and Construction","Residential Repair and Remodeling",
    "Wood Packaging & Shipping","Manufacturing Misc.","Other Industrial Products",
    "Paper Products","Other"
  )
  
  # --- Colors ---
  pal_cat <- c(
    "Furniture"                         = "#D8C097",
    "Housing and Construction"          = "#C99A6C",
    "Residential Repair and Remodeling" = "#BA7A44",
    "Wood Packaging & Shipping"         = "#A25B24",
    "Manufacturing Misc."               = "#8A4415",
    "Other Industrial Products"         = "#753411",
    "Paper Products"                    = "#5F280D",
    "Other"                             = "#3A1708"
  )
  
  # --- Patterns ---
  pat_cat <- c(
    "Furniture"                         = "crosshatch",
    "Housing and Construction"          = "circle",
    "Residential Repair and Remodeling" = "none",
    "Wood Packaging & Shipping"         = "stripe",
    "Manufacturing Misc."               = "crosshatch",
    "Other Industrial Products"         = "circle",
    "Paper Products"                    = "none",
    "Other"                             = "stripe"
  )
  
  present <- intersect(plot_levels, unique(long$Category))
  long$Category <- factor(long$Category, levels = present)
  
  pal_present <- pal_cat[present]
  pat_present <- pat_cat[present]
  
  ggplot2::ggplot(long, ggplot2::aes(Year, Value, fill = Category)) +
    ggpattern::geom_area_pattern(
      ggplot2::aes(pattern = Category),
      pattern_fill    = "black",
      pattern_colour  = "black",
      pattern_alpha   = 0.8,
      pattern_density = 0.1,
      pattern_spacing = 0.02,
      alpha           = 0.9,
      color           = "black",
      linewidth       = 0.25
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = pal_present, breaks = present) +
    ggpattern::scale_pattern_manual(values = pat_present, breaks = present) +
    ggplot2::scale_y_continuous(
      breaks = seq(ax$min, ax$max, by = ax$by),
      limits = c(ax$min, ax$max),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Harvest Year",
      y = ylab,
      title = paste0(
        "Cumulative carbon stored in ",
        if (pools == "piu") "PIU"
        else if (pools == "swds") "SWDS"
        else "PIU and SWDS",
        " by product category"
      )
    ) +
    common_theme
}


# ---- Your call (unchanged) ----
p4 <- plot_carbon_storage_by_product_category(
  hwp,
  pools  = "both",
  metric = "MMTC"
)
print(p4)


# =========================================================
# Cumulative carbon stocks by pool (PIU vs. SWDS)
#    approach: "production" | "simple_decay"
#    metric:   "MMTC" | "CO2e"
# Notes
#  • Production: uses hwp$pu.final_array and hwp$swdsCtotal_array (Total slice).
#  • Simple-decay: integrates (Domestic + Imports − Exports) − (EEC + EWOEC).
#    If pool-level simple-decay stocks aren't available, splits the total
#    using the Production pool shares for that year.

# ---- Theme ----
common_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 24),
    axis.text  = element_text(size = 18),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(22, "pt"),
    legend.key.width = unit(25, "pt"),
    legend.box.margin = margin(8, 10, 0, 10),
    legend.spacing.x = unit(14, "pt"),
    plot.margin = margin(22, 18, 10, 14)
  )

.axis_pretty <- function(x, n = 6) {
  x <- x[is.finite(x)]
  if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
  br <- pretty(range(x, na.rm = TRUE), n = n)
  list(min = min(br), max = max(br), by = diff(br)[1])
}
.clean_names <- function(x) if (is.null(x)) character(0) else gsub("\\.", " ", trimws(x))

# Sum across EndUse + owners to a Year vector, preferring the "Total" owner slice.
.series_total_or_sum <- function(arr, exclude_exports = FALSE) {
  stopifnot(!is.null(arr))
  dn2  <- .clean_names(dimnames(arr)[[2]])
  nOwn <- dim(arr)[2]
  if (!length(dn2)) dn2 <- paste0("Owner", seq_len(nOwn))
  idx_total   <- which(dn2 == "Total")
  idx_exports <- which(dn2 == "Exports")
  if (length(idx_total)) {
    apply(arr[, idx_total, , drop = FALSE], 3, sum, na.rm = TRUE)
  } else {
    keep <- seq_len(nOwn)
    if (exclude_exports && length(idx_exports)) keep <- setdiff(keep, idx_exports)
    apply(arr[, keep, , drop = FALSE], 3, sum, na.rm = TRUE)
  }
}

# Safe years retrieval
.get_years <- function(hwp) {
  y <- hwp$years
  if (!is.null(y)) return(as.numeric(y))
  for (nm in c("eu_array", "pu.final_array", "swdsCtotal_array", "eec_array")) {
    if (!is.null(hwp[[nm]])) {
      yy <- suppressWarnings(as.numeric(dimnames(hwp[[nm]])[[3]]))
      if (!all(is.na(yy))) return(yy)
    }
  }
  stop("hwp$years is required or must be derivable from array dimnames[[3]].")
}

# =========================================================
# A) Cumulative carbon stocks by pool (PIU vs. SWDS) — PRODUCTION
#    approach reads pu.final_array & swdsCtotal_array (Total slice).
# =========================================================
plot_cumulative_stocks_by_pool <- function(
    hwp,
    metric             = c("MMTC","CO2e"),
    include_total_line = TRUE,
    y_min = NA,
    y_max = NA
) {
  metric <- match.arg(metric)
  
  years <- .get_years(hwp)
  if (is.null(hwp$pu.final_array) || is.null(hwp$swdsCtotal_array)) {
    stop("Production approach needs hwp$pu.final_array and hwp$swdsCtotal_array.")
  }
  
  pu   <- .series_total_or_sum(hwp$pu.final_array)   / 1e6  # MMT C
  swds <- .series_total_or_sum(hwp$swdsCtotal_array) / 1e6
  df <- data.frame(Year = years, PIU = pu, SWDS = swds, Total = pu + swds)
  
  # Metric conversion
  if (metric == "CO2e") {
    df[c("PIU","SWDS","Total")] <- lapply(
      df[c("PIU","SWDS","Total")],
      function(v) v * (44/12)
    )
  }
  
  # Updated y-axis title
  ylab <- if (metric == "CO2e") {
    "HWP Carbon Stock (MMT CO2e)"
  } else {
    "HWP Carbon Stock (MMT C)"
  }
  
  # Axis
  if (is.na(y_min) || is.na(y_max)) {
    ax <- .axis_pretty(df$Total)
    if (is.na(y_min)) y_min <- ax$min
    if (is.na(y_max)) y_max <- ax$max
  }
  
  long <- rbind(
    data.frame(Year = df$Year, pool = "Products in Use",            Value = df$PIU),
    data.frame(Year = df$Year, pool = "Solid Waste Disposal Sites", Value = df$SWDS)
  )
  
  p <- ggplot2::ggplot(long, ggplot2::aes(Year, Value, fill = pool)) +
    ggplot2::geom_area(color = "black", linewidth = 0.2, alpha = 0.95) +
    ggplot2::scale_fill_manual(
      values = c(
        "Products in Use"            = "#C99A6C",
        "Solid Waste Disposal Sites" = "#5F280D"
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x     = "Year",
      y     = ylab,
      title = "Cumulative carbon stocks by pool — Production approach"
    ) +
    common_theme
  
  p
}


# =========================================================
# B) Cumulative simple-decay accounting
#    Stacks cumulative inflow (Domestic + Imports − Exports) vs.
#    cumulative EEC and EWOEC; overlays net cumulative stock.
#    Use y_min/y_max to force axis (e.g., y_min = -500, y_max = 900).
# =========================================================
plot_cumulative_simple_decay <- function(
    hwp,
    metric = c("MMTC","CO2e"),
    include_net_line  = TRUE,
    show_eec_outlines = TRUE,
    y_min = -600,        # <- force this lower bound
    y_max = NULL         # <- optional upper bound (NULL = auto)
) {
  metric <- match.arg(metric)
  
  # ---- helpers ----
  clean_names <- function(x) if (is.null(x)) character(0) else gsub("\\.", " ", trimws(x))
  years_from  <- function(arr) suppressWarnings(as.numeric(dimnames(arr)[[3]]))
  
  # ---- years ----
  years <- hwp$years
  if (is.null(years)) {
    cand <- c(years_from(hwp$eu_array), years_from(hwp$eec_array), years_from(hwp$ewoec_array))
    cand <- cand[is.finite(cand)]
    if (!length(cand)) stop("Could not determine years from 'hwp'.")
    years <- sort(unique(cand))
  }
  
  # ---- inputs ----
  if (is.null(hwp$eu_array) || is.null(hwp$eec_array) || is.null(hwp$ewoec_array)) {
    stop("Need eu_array, eec_array, and ewoec_array in 'hwp'.")
  }
  
  # Consumption inflow = Domestic + Imports − Exports
  owns <- clean_names(dimnames(hwp$eu_array)[[2]])
  nOwn <- dim(hwp$eu_array)[2]; if (!length(owns)) owns <- paste0("Owner", seq_len(nOwn))
  idx_total   <- which(owns == "Total")
  idx_imports <- which(owns == "Imports")
  idx_exports <- which(owns == "Exports")
  idx_dom     <- setdiff(seq_len(nOwn), c(idx_total, idx_imports, idx_exports))
  
  sum_owner <- function(arr, idx) {
    if (!length(idx)) return(rep(0, length(years)))
    as.numeric(apply(arr[, idx, , drop = FALSE], 3, sum, na.rm = TRUE))
  }
  
  eu_total   <- if (length(idx_total))   sum_owner(hwp$eu_array, idx_total)   else rep(NA_real_, length(years))
  eu_imports <- if (length(idx_imports)) sum_owner(hwp$eu_array, idx_imports) else rep(0, length(years))
  eu_exports <- if (length(idx_exports)) sum_owner(hwp$eu_array, idx_exports) else rep(0, length(years))
  eu_dom     <- if (length(idx_dom))     sum_owner(hwp$eu_array, idx_dom)     else rep(0, length(years))
  
  need_fix <- (eu_dom == 0) & is.finite(eu_total)
  if (any(need_fix)) eu_dom[need_fix] <- eu_total[need_fix] - eu_imports[need_fix] - eu_exports[need_fix]
  
  inflow <- (eu_dom + eu_imports - eu_exports) / 1e6  # MMT C
  
  # Emissions (Total if present; else owners excluding Exports)
  sum_total_or_exclude_exports <- function(arr) {
    o <- clean_names(dimnames(arr)[[2]]); n <- dim(arr)[2]
    if (!length(o)) o <- paste0("Owner", seq_len(n))
    i_total   <- which(o == "Total")
    i_exports <- which(o == "Exports")
    if (length(i_total)) {
      as.numeric(apply(arr[, i_total, , drop = FALSE], 3, sum, na.rm = TRUE))
    } else {
      keep <- setdiff(seq_len(n), i_exports)
      as.numeric(apply(arr[, keep, , drop = FALSE], 3, sum, na.rm = TRUE))
    }
  }
  eec   <- sum_total_or_exclude_exports(hwp$eec_array)   / 1e6
  ewoec <- sum_total_or_exclude_exports(hwp$ewoec_array) / 1e6
  
  # Cumulative series
  cin   <- cumsum(inflow)
  cEEC  <- cumsum(eec)
  cEWO  <- cumsum(ewoec)
  net   <- cin - cEEC - cEWO
  
  # Metric conversion
  conv  <- if (metric == "CO2e") 44/12 else 1
  cin  <- cin  * conv; cEEC <- cEEC * conv; cEWO <- cEWO * conv; net <- net * conv
  ylab <- if (metric == "CO2e") "MMT CO2e" else "MMT C"
  
  # Long data for stacked areas
  pos_df <- data.frame(
    Year = years,
    series = factor("Consumption inflow (cum.)",
                    levels = c("Consumption inflow (cum.)",
                               "Emitted with Energy Capture (cum.)",
                               "Emitted without Energy Capture (cum.)")),
    Value = cin
  )
  neg_df <- rbind(
    data.frame(Year = years, series = "Emitted with Energy Capture (cum.)",    Value = -cEEC),
    data.frame(Year = years, series = "Emitted without Energy Capture (cum.)", Value = -cEWO)
  )
  neg_df$series <- factor(neg_df$series,
                          levels = c("Emitted with Energy Capture (cum.)",
                                     "Emitted without Energy Capture (cum.)"))
  
  # ---- hard axis limits ----
  pos_max   <- max(pos_df$Value, 0, na.rm = TRUE)
  y_lo <- y_min
  y_hi <- if (is.null(y_max)) max(pos_max, 0) else y_max
  y_breaks <- pretty(c(y_lo, y_hi), n = 7)
  
  # ---- plot ----
  p <- ggplot2::ggplot() +
    ggplot2::geom_area(data = pos_df,
                       ggplot2::aes(Year, Value, fill = series)) +
    ggplot2::geom_area(data = neg_df,
                       ggplot2::aes(Year, Value, fill = series),
                       position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::scale_fill_manual(
      values = c("Consumption inflow (cum.)"             = "#00CED1",
                 "Emitted with Energy Capture (cum.)"    = "#F8DF25",
                 "Emitted without Energy Capture (cum.)" = "#F99A3E"),
      breaks = c("Consumption inflow (cum.)",
                 "Emitted with Energy Capture (cum.)",
                 "Emitted without Energy Capture (cum.)"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(limits = c(y_lo, y_hi),
                                breaks = y_breaks,
                                expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(y_lo, y_hi), expand = FALSE, clip = "on") +
    ggplot2::labs(x = "Year", y = ylab,
                  title = "Cumulative carbon accounting — Simple-decay approach") +
    common_theme
  
  if (isTRUE(show_eec_outlines)) {
    p <- p +
      ggplot2::geom_line(data = data.frame(Year = years, y = -cEEC),
                         ggplot2::aes(Year, y),
                         color = "#C1B000", linewidth = 0.6, inherit.aes = FALSE) +
      ggplot2::geom_line(data = data.frame(Year = years, y = -(cEEC + cEWO)),
                         ggplot2::aes(Year, y),
                         color = "#D66A1A", linewidth = 0.6, inherit.aes = FALSE)
  }
  
  if (isTRUE(include_net_line)) {
    p <- p +
      ggplot2::geom_line(data = data.frame(Year = years, Net = net),
                         ggplot2::aes(Year, Net, color = "Net cumulative stock"),
                         linewidth = 0.9) +
      ggplot2::scale_color_manual(values = c("Net cumulative stock" = "#2E8B57"),
                                  name = NULL)
  }
  
  p
}

# =========================
# Example usage
# =========================
p_prod <- plot_cumulative_stocks_by_pool(hwp, metric = "MMTC", include_total_line = TRUE)
print(p_prod)

p_sd <- plot_cumulative_simple_decay(
  hwp,
  metric = "MMTC",
  include_net_line = TRUE,
  y_min = -700,
  y_max = 1350
)
print(p_sd)



# === CUMULATIVE W/ TRENDLINE ===========
# --- Libraries ---
suppressWarnings(suppressMessages(require(ggplot2)))

# --- Helpers (unchanged) ---
.axis_pretty <- function(x, n = 6) {
  x <- x[is.finite(x)]
  if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
  br <- pretty(range(x, na.rm = TRUE), n = n)
  list(min = min(br), max = max(br), by = diff(br)[1])
}
.clean_names <- function(x) if (is.null(x)) character(0) else gsub("\\.", " ", trimws(x))

.series_total_or_sum <- function(arr, exclude_exports = FALSE) {
  stopifnot(!is.null(arr))
  dn2  <- .clean_names(dimnames(arr)[[2]])
  nOwn <- dim(arr)[2]
  if (!length(dn2)) dn2 <- paste0("Owner", seq_len(nOwn))
  idx_total   <- which(dn2 == "Total")
  idx_exports <- which(dn2 == "Exports")
  if (length(idx_total)) {
    apply(arr[, idx_total, , drop = FALSE], 3, sum, na.rm = TRUE)
  } else {
    keep <- seq_len(nOwn)
    if (exclude_exports && length(idx_exports)) keep <- setdiff(keep, idx_exports)
    apply(arr[, keep, , drop = FALSE], 3, sum, na.rm = TRUE)
  }
}
.get_years <- function(hwp) {
  y <- hwp$years
  if (!is.null(y)) return(as.numeric(y))
  for (nm in c("eu_array", "pu.final_array", "swdsCtotal_array", "eec_array")) {
    if (!is.null(hwp[[nm]])) {
      yy <- suppressWarnings(as.numeric(dimnames(hwp[[nm]])[[3]]))
      if (!all(is.na(yy))) return(yy)
    }
  }
  stop("hwp$years is required or must be derivable from array dimnames[[3]].")
}

# =========================================================
# Production approach (2001–2023 trendline + label, shifted left)
# Args:
#   metric: "MMTC" | "CO2e"
#   include_total_line: overlay black total line
#   add_trend: add linear trend for `trend_years` on Total
#   trend_years: c(start,end) years for trend (default 2001–2023)
#   y_min/y_max: optional fixed y-axis
# =========================================================
plot_cumulative_stocks_by_pool <- function(
    hwp,
    metric             = c("MMTC","CO2e"),
    include_total_line = TRUE,
    add_trend          = TRUE,
    trend_years        = c(2001L, 2023L),
    y_min = NA,
    y_max = NA
) {
  metric <- match.arg(metric)
  
  years <- .get_years(hwp)
  if (is.null(hwp$pu.final_array) || is.null(hwp$swdsCtotal_array)) {
    stop("Production approach needs hwp$pu.final_array and hwp$swdsCtotal_array.")
  }
  
  pu   <- .series_total_or_sum(hwp$pu.final_array)   / 1e6  # MMT C
  swds <- .series_total_or_sum(hwp$swdsCtotal_array) / 1e6
  df <- data.frame(Year = years, PIU = pu, SWDS = swds, Total = pu + swds)
  
  # Metric conversion
  conv <- if (metric == "CO2e") 44/12 else 1
  if (conv != 1) df[c("PIU","SWDS","Total")] <- lapply(df[c("PIU","SWDS","Total")], `*`, conv)
  ylab  <- if (metric == "CO2e") expression("MMT CO"[2]*"e") else "MMT C"
  yunit <- if (metric == "CO2e") "MMT CO2e" else "MMT C"
  
  # Axis
  if (is.na(y_min) || is.na(y_max)) {
    ax <- .axis_pretty(df$Total)
    if (is.na(y_min)) y_min <- ax$min
    if (is.na(y_max)) y_max <- ax$max
  }
  
  long <- rbind(
    data.frame(Year = df$Year, pool = "Products in Use",            Value = df$PIU),
    data.frame(Year = df$Year, pool = "Solid Waste Disposal Sites", Value = df$SWDS)
  )
  
  p <- ggplot2::ggplot(long, ggplot2::aes(Year, Value, fill = pool)) +
    ggplot2::geom_area(color = "black", linewidth = 0.2, alpha = 0.95) +
    ggplot2::scale_fill_manual(
      values = c("Products in Use" = "#6F00A8", "Solid Waste Disposal Sites" = "#C6508F")
    ) +
    ggplot2::scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +
    ggplot2::labs(
      x = "Year", y = ylab,
      title = "Cumulative carbon stocks by pool — Production approach"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      plot.margin  = grid::unit(c(8, 20, 8, 8), "pt")  # add a bit of right margin
    )
  
  if (isTRUE(include_total_line)) {
    p <- p +
      ggplot2::geom_line(
        data = df, ggplot2::aes(Year, Total, color = "Total"),
        linewidth = 0.8, inherit.aes = FALSE
      ) +
      ggplot2::scale_color_manual(values = c(Total = "black"), name = NULL)
  }
  
  # ---- Trendline & annotation for 2001–2023 (or available overlap) ----
  if (isTRUE(add_trend) && length(trend_years) == 2) {
    y1 <- max(min(trend_years), min(df$Year, na.rm = TRUE))
    y2 <- min(max(trend_years), max(df$Year, na.rm = TRUE))
    df_fit <- subset(df, Year >= y1 & Year <= y2 & is.finite(Total))
    
    if (nrow(df_fit) >= 2) {
      mdl   <- stats::lm(Total ~ Year, data = df_fit)
      slope <- unname(coef(mdl)[["Year"]])     # units per year (MMT C or CO2e)
      r2    <- summary(mdl)$r.squared
      
      # draw trendline for that window only
      p <- p +
        ggplot2::geom_smooth(
          data = df_fit,
          ggplot2::aes(Year, Total),
          method = "lm", se = FALSE, color = "#D62728", linetype = "dashed",
          linewidth = 0.9, inherit.aes = FALSE
        )
      
      # annotate slope & R^2 — 2% in from left of the window, near the top
      x_span <- max(1, y2 - y1)
      x_anno <- y1 + 0.02 * x_span
      y_anno <- y_max - 0.05 * (y_max - y_min)
      signch <- ifelse(slope >= 0, "+", "")
      lbl <- sprintf("Trend %d–%d: %s%.2f %s/yr  (R\u00B2 = %.3f)", y1, y2, signch, slope, yunit, r2)
      
      p <- p + ggplot2::annotate("text", x = x_anno, y = y_anno, label = lbl,
                                 hjust = 0, vjust = 1, size = 4.2, color = "#D62728")
    }
  }
  
  p
}

# ---- Example usage ----
p_prod <- plot_cumulative_stocks_by_pool(hwp, metric = "MMTC", include_total_line = TRUE)
print(p_prod)






# =========================================================
# 5) Carbon Storage by Ownership — MMT C
#    pools: "both" | "piu" | "swds"
#    ownership_start_year: mask (no color) before this year
#    x_years_full: full x-axis years to display (e.g., harv.hwp$Year)
# =========================================================
plot_carbon_storage_by_ownership <- function(
    hwp,
    pools = c("both","piu","swds"),
    owners = NULL,
    ownership_start_year = NULL,
    x_years_full = NULL
) {
  pools <- match.arg(pools)
  
  axis_pretty <- function(x) {
    rng <- range(x, na.rm = TRUE)
    brk <- pretty(rng)
    by  <- if (length(brk) >= 2) brk[2] - brk[1] else max(rng, 1)
    list(min = 0, max = max(0, ceiling(max(rng, 0)/by) * by), by = by)
  }
  y_lab <- "MMT C"
  
  pu_arr   <- hwp$pu.final_array
  swds_arr <- hwp$swdsCtotal_array
  if (is.null(pu_arr) || is.null(swds_arr)) stop("pu.final_array and swdsCtotal_array must be present in `hwp`.")
  
  years <- hwp$years
  arr_years <- suppressWarnings(as.numeric(dimnames(pu_arr)[[3]]))
  if (!any(is.na(arr_years)) && length(arr_years) == dim(pu_arr)[3]) years <- arr_years
  
  owners_pu   <- trimws(if (!is.null(dimnames(pu_arr)[[2]])) dimnames(pu_arr)[[2]] else character())
  owners_swds <- trimws(if (!is.null(dimnames(swds_arr)[[2]])) dimnames(swds_arr)[[2]] else character())
  declared <- trimws(if (!is.null(hwp$ownership.names)) hwp$ownership.names else intersect(owners_pu, owners_swds))
  
  if (is.null(owners) || !length(owners)) {
    owners <- setdiff(declared, "Total")
    if (!length(owners)) owners <- declared
  }
  owners <- intersect(trimws(owners), intersect(owners_pu, owners_swds))
  if (!length(owners)) stop("None of the requested owners are present in both arrays.")
  
  if (is.null(x_years_full)) {
    if (exists("harv.hwp", inherits = TRUE) && is.data.frame(harv.hwp) && "Year" %in% names(harv.hwp)) {
      x_years_full <- sort(unique(as.numeric(harv.hwp$Year)))
    } else {
      x_years_full <- sort(unique(as.numeric(years)))
    }
  }
  x_years_full <- sort(unique(as.numeric(x_years_full)))
  xmin <- min(x_years_full, na.rm = TRUE)
  xmax <- max(x_years_full, na.rm = TRUE)
  
  if (is.null(ownership_start_year)) {
    osy <- get0("OWNERSHIP_STARTYEAR", ifnotfound = NA_real_)
    ownership_start_year <- if (is.na(osy)) min(years, na.rm = TRUE) else osy
  }
  
  sum_by_year <- function(arr, owner_name) {
    odim <- trimws(dimnames(arr)[[2]])
    oi <- which(odim == owner_name)
    if (!length(oi)) return(rep(NA_real_, dim(arr)[3]))
    as.numeric(apply(arr[, oi, , drop = FALSE], 3, sum, na.rm = TRUE))
  }
  
  piu_mat  <- sapply(owners, function(o) sum_by_year(pu_arr,   o) / 1e6)
  swds_mat <- sapply(owners, function(o) sum_by_year(swds_arr, o) / 1e6)
  piu_mat  <- `storage.mode<-`(as.matrix(piu_mat),  "double"); rownames(piu_mat)  <- years
  swds_mat <- `storage.mode<-`(as.matrix(swds_mat), "double"); rownames(swds_mat) <- years
  
  df_piu <- as.data.frame(piu_mat);  df_piu$Year <- years
  df_swd <- as.data.frame(swds_mat); df_swd$Year <- years
  
  pad_years <- setdiff(x_years_full, years)
  if (length(pad_years)) {
    add_piu <- as.data.frame(matrix(NA_real_, nrow = length(pad_years), ncol = ncol(piu_mat),
                                    dimnames = list(NULL, colnames(piu_mat))))
    add_piu$Year <- pad_years
    df_piu <- dplyr::bind_rows(df_piu, add_piu)
    
    add_swd <- as.data.frame(matrix(NA_real_, nrow = length(pad_years), ncol = ncol(swds_mat),
                                    dimnames = list(NULL, colnames(swds_mat))))
    add_swd$Year <- pad_years
    df_swd <- dplyr::bind_rows(df_swd, add_swd)
  }
  
  keep_cols <- c("Year", owners)
  df_piu <- df_piu |> dplyr::select(dplyr::any_of(keep_cols)) |>
    dplyr::mutate(Year = as.numeric(Year)) |> dplyr::arrange(Year)
  df_swd <- df_swd |> dplyr::select(dplyr::any_of(keep_cols)) |>
    dplyr::mutate(Year = as.numeric(Year)) |> dplyr::arrange(Year)
  df_piu[owners] <- lapply(df_piu[owners], as.numeric)
  df_swd[owners] <- lapply(df_swd[owners], as.numeric)
  
  df <- dplyr::left_join(
    df_piu |> tidyr::pivot_longer(cols = dplyr::all_of(owners), names_to = "Owner", values_to = "PIU"),
    df_swd |> tidyr::pivot_longer(cols = dplyr::all_of(owners), names_to = "Owner", values_to = "SWDS"),
    by = c("Year","Owner")
  ) |>
    dplyr::mutate(
      PIU  = dplyr::if_else(Year < ownership_start_year, NA_real_, PIU),
      SWDS = dplyr::if_else(Year < ownership_start_year, NA_real_, SWDS)
    )
  
  df$Value <- dplyr::case_when(
    pools == "piu"  ~ df$PIU,
    pools == "swds" ~ df$SWDS,
    TRUE            ~ df$PIU + df$SWDS
  )
  
  base_cols <- if (requireNamespace("viridisLite", quietly = TRUE)) {
    viridisLite::viridis(length(owners))
  } else {
    grDevices::rainbow(length(owners))
  }
  names(base_cols) <- owners
  
  if (pools == "both") {
    df_long <- dplyr::bind_rows(
      df |> dplyr::mutate(series = "Products in Use", Value = PIU),
      df |> dplyr::mutate(series = "SWDS",            Value = SWDS)
    ) |>
      dplyr::select(Year, Owner, series, Value) |>
      dplyr::arrange(Owner, series, Year) |>
      dplyr::filter(!is.na(Value))
    
    series_levels <- levels(interaction(df_long$Owner, df_long$series, sep = " — ", drop = TRUE))
    col_map <- setNames(rep("#999999", length(series_levels)), series_levels)
    for (o in owners) {
      col_map[paste(o, "Products in Use", sep = " — ")] <- base_cols[o]
      col_map[paste(o, "SWDS",            sep = " — ")] <- if (requireNamespace("scales", quietly = TRUE)) {
        scales::alpha(base_cols[o], 0.55)
      } else base_cols[o]
    }
    
    # --- hide Exports in legend but still draw it ---
    hide_keys  <- c("Exports — Products in Use", "Exports — SWDS")
    keep_breaks <- setdiff(series_levels, hide_keys)
    
    ax <- axis_pretty(
      df_long |> dplyr::group_by(Year) |>
        dplyr::summarise(Tot = sum(Value, na.rm = TRUE), .groups = "drop") |>
        dplyr::pull(Tot)
    )
    
    return(
      ggplot2::ggplot(df_long,
                      ggplot2::aes(Year, Value, fill = interaction(Owner, series, sep = " — "))) +
        ggplot2::geom_area(na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = col_map, name = NULL, breaks = keep_breaks) +  # <—
        ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                    limits = c(ax$min, ax$max), expand = c(0, 0)) +
        ggplot2::scale_x_continuous(breaks = pretty(x_years_full),
                                    limits = c(xmin, xmax), expand = c(0, 0)) +
        ggplot2::labs(x = "Harvest Year", y = y_lab,
                      title = "Cumulative carbon stored in PIU and SWDS by ownership") +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")
    )
  }
  
  # single-pool path
  df_plot <- df |> dplyr::arrange(Owner, Year) |> dplyr::filter(!is.na(Value))
  ax <- axis_pretty(
    df_plot |> dplyr::group_by(Year) |>
      dplyr::summarise(Tot = sum(Value, na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(Tot)
  )
  
  # --- hide "Exports" in legend (still plotted) ---
  legend_breaks <- setdiff(owners, "Exports")
  
  ggplot2::ggplot(df_plot, ggplot2::aes(Year, Value, fill = Owner)) +
    ggplot2::geom_area(na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = base_cols, name = NULL, breaks = legend_breaks) +  # <—
    ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                limits = c(ax$min, ax$max), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = pretty(x_years_full),
                                limits = c(xmin, xmax), expand = c(0, 0)) +
    ggplot2::labs(x = "Harvest Year", y = y_lab,
                  title = paste("Cumulative carbon stored in",
                                if (pools == "piu") "products in use" else "SWDS",
                                "by ownership")) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}




# Example call (keeps axis covering all years; no fill before 1952):
p5 <- plot_carbon_storage_by_ownership(
  hwp,
  pools = "both",
  ownership_start_year = 1952,
  x_years_full = harv.hwp$Year
)
print(p5)



# =========================================================
# Carbon storage — Domestic vs Imports (MMT C)
#   pools: "both" | "piu" | "swds"
#   ownership_start_year: mask (no color) before this year
#   x_years_full: full x-axis years to display (e.g., harv.hwp$Year)
#   Imports are stacked ON TOP of Domestic.
# =========================================================
plot_carbon_storage_domestic_vs_imports <- function(
    hwp,
    pools = c("both","piu","swds"),
    ownership_start_year = NULL,
    x_years_full = NULL
) {
  pools <- match.arg(pools)
  
  # -------- helpers --------
  axis_pretty <- function(x) {
    rng <- range(x, na.rm = TRUE)
    brk <- pretty(rng)
    by  <- if (length(brk) >= 2) brk[2] - brk[1] else max(rng, 1)
    list(min = 0, max = max(0, ceiling(max(rng, 0)/by) * by), by = by)
  }
  sum_by_owner_set <- function(arr, owner_names) {
    odim <- trimws(dimnames(arr)[[2]])
    oi   <- which(odim %in% owner_names)
    if (!length(oi)) return(rep(0, dim(arr)[3]))
    as.numeric(apply(arr[, oi, , drop = FALSE], 3, sum, na.rm = TRUE))
  }
  sum_total_or_all <- function(arr) {
    odim <- trimws(dimnames(arr)[[2]])
    jTot <- which(odim == "Total")
    if (length(jTot)) as.numeric(apply(arr[, jTot, , drop = FALSE], 3, sum, na.rm = TRUE))
    else              as.numeric(apply(arr, 3, sum, na.rm = TRUE))
  }
  y_lab <- "MMT C"
  
  # -------- inputs --------
  pu_arr   <- hwp$pu.final_array
  swds_arr <- hwp$swdsCtotal_array
  if (is.null(pu_arr) || is.null(swds_arr)) stop("pu.final_array and swdsCtotal_array must be present in `hwp`.")
  
  years <- hwp$years
  arr_years <- suppressWarnings(as.numeric(dimnames(pu_arr)[[3]]))
  if (!any(is.na(arr_years)) && length(arr_years) == dim(pu_arr)[3]) years <- arr_years
  
  owners_pu   <- trimws(if (!is.null(dimnames(pu_arr)[[2]])) dimnames(pu_arr)[[2]] else character())
  owners_swds <- trimws(if (!is.null(dimnames(swds_arr)[[2]])) dimnames(swds_arr)[[2]] else character())
  common_owners <- intersect(owners_pu, owners_swds)
  
  imports_tag  <- "Imports"
  domestic_set <- setdiff(common_owners, c("Total", imports_tag))
  
  # x-axis domain
  if (is.null(x_years_full)) {
    if (exists("harv.hwp", inherits = TRUE) && is.data.frame(harv.hwp) && "Year" %in% names(harv.hwp)) {
      x_years_full <- sort(unique(as.numeric(harv.hwp$Year)))
    } else {
      x_years_full <- sort(unique(as.numeric(years)))
    }
  }
  x_years_full <- sort(unique(as.numeric(x_years_full)))
  xmin <- min(x_years_full, na.rm = TRUE)
  xmax <- max(x_years_full, na.rm = TRUE)
  
  # anchor/mask default
  if (is.null(ownership_start_year)) {
    osy <- get0("OWNERSHIP_STARTYEAR", ifnotfound = NA_real_)
    ownership_start_year <- if (is.na(osy)) min(years, na.rm = TRUE) else osy
  }
  baseline_year <- ownership_start_year - 1L
  
  # -------- base series (MMT C) --------
  # Domestic by owner (non-imports), typically valid post-1952
  piu_dom_post <- sum_by_owner_set(pu_arr,   domestic_set) / 1e6
  swd_dom_post <- sum_by_owner_set(swds_arr, domestic_set) / 1e6
  # Imports
  piu_imp <- sum_by_owner_set(pu_arr,   imports_tag)  / 1e6
  swd_imp <- sum_by_owner_set(swds_arr, imports_tag)  / 1e6
  # Totals
  piu_tot <- sum_total_or_all(pu_arr)   / 1e6
  swd_tot <- sum_total_or_all(swds_arr) / 1e6
  
  # Pre-1952 Domestic = Total − Imports (per pool), used for both display and baseline
  piu_dom_pre <- (piu_tot - piu_imp)
  swd_dom_pre <- (swd_tot - swd_imp)
  
  df <- data.frame(
    Year            = years,
    Domestic_PIU    = piu_dom_post,   # will adjust shortly
    Imports_PIU     = piu_imp,
    Domestic_SWDS   = swd_dom_post,   # will adjust shortly
    Imports_SWDS    = swd_imp,
    check.names = FALSE
  )
  
  # ---- Anchor-and-increment for 1952+ (adds legacy once; no double count) ----
  if (ownership_start_year %in% df$Year && any(df$Year <= baseline_year)) {
    i_anchor <- which(df$Year == ownership_start_year)[1]
    i_base   <- tail(which(df$Year <= baseline_year), 1)
    
    base_piu  <- piu_dom_pre[i_base]   # full legacy at 1951
    base_swds <- swd_dom_pre[i_base]
    
    anch_piu  <- df$Domestic_PIU[i_anchor]   # owner-split at 1952
    anch_swds <- df$Domestic_SWDS[i_anchor]
    
    is_post <- df$Year >= ownership_start_year
    
    df$Domestic_PIU[is_post]  <- base_piu  + pmax(df$Domestic_PIU[is_post]  - anch_piu,  0)
    df$Domestic_SWDS[is_post] <- base_swds + pmax(df$Domestic_SWDS[is_post] - anch_swds, 0)
  }
  
  # ---- NEW: Keep Domestic visible pre-1952 (continuous fill) ----
  pre <- df$Year <= baseline_year
  df$Domestic_PIU[pre]  <- pmax(piu_dom_pre[pre],  0)
  df$Domestic_SWDS[pre] <- pmax(swd_dom_pre[pre],  0)
  
  # pad to x_years_full
  if (length(setdiff(x_years_full, df$Year))) {
    df <- merge(data.frame(Year = x_years_full), df, by = "Year", all.x = TRUE, sort = TRUE)
  }
  
  # mask ONLY imports before ownership_start_year (Domestic stays visible)
  mask_imp <- df$Year < ownership_start_year
  df$Imports_PIU[mask_imp]   <- NA_real_
  df$Imports_SWDS[mask_imp]  <- NA_real_
  
  # ---- long format ----
  df_long <- rbind(
    data.frame(Year = df$Year, Owner = "Domestic", series = "Products in Use", Value = df$Domestic_PIU),
    data.frame(Year = df$Year, Owner = "Imports",  series = "Products in Use", Value = df$Imports_PIU),
    data.frame(Year = df$Year, Owner = "Domestic", series = "SWDS",            Value = df$Domestic_SWDS),
    data.frame(Year = df$Year, Owner = "Imports",  series = "SWDS",            Value = df$Imports_SWDS)
  )
  df_long <- df_long[is.finite(df_long$Year) & !is.na(df_long$Value), , drop = FALSE]
  
  # axis
  ax <- axis_pretty(aggregate(Value ~ Year, df_long, sum, na.rm = TRUE)$Value)
  
  # colors
  col_dom <- "#159A74"
  col_imp <- "#D95F02"
  alpha_if <- function(col, a = 0.55) {
    if (requireNamespace("scales", quietly = TRUE)) scales::alpha(col, a) else col
  }
  
  if (pools == "both") {
    fill_levels <- c(
      "Domestic — Products in Use",
      "Domestic — SWDS",
      "Imports — Products in Use",
      "Imports — SWDS"
    )
    df_long$fill_key <- factor(interaction(df_long$Owner, df_long$series, sep = " — "),
                               levels = fill_levels)
    col_map <- setNames(
      c(col_dom, alpha_if(col_dom, 0.55), col_imp, alpha_if(col_imp, 0.55)),
      fill_levels
    )
    
    return(
      ggplot2::ggplot(df_long, ggplot2::aes(Year, Value, fill = fill_key)) +
        ggplot2::geom_area(position = ggplot2::position_stack(reverse = FALSE), na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = col_map, name = NULL, breaks = fill_levels) +
        ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                    limits = c(ax$min, ax$max), expand = c(0, 0)) +
        ggplot2::scale_x_continuous(breaks = pretty(x_years_full),
                                    limits = c(xmin, xmax), expand = c(0, 0)) +
        ggplot2::labs(
          x = "Harvest Year", y = y_lab,
          title = paste0(
            "Cumulative carbon stored in PIU and SWDS — Domestic vs Imports\n",
            "(Domestic shows pre-", ownership_start_year, " PIU & SWDS and includes legacy once in 1952+)"
          )
        ) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")
    )
  }
  
  # single-pool path
  df_plot <- if (pools == "piu") subset(df_long, series == "Products in Use") else subset(df_long, series == "SWDS")
  df_plot$Owner <- factor(df_plot$Owner, levels = c("Domestic","Imports"))
  col_two <- c(Domestic = col_dom, Imports = col_imp)
  ax2 <- axis_pretty(aggregate(Value ~ Year, df_plot, sum, na.rm = TRUE)$Value)
  
  ggplot2::ggplot(df_plot, ggplot2::aes(Year, Value, fill = Owner)) +
    ggplot2::geom_area(position = ggplot2::position_stack(reverse = FALSE), na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = col_two, name = NULL) +
    ggplot2::scale_y_continuous(breaks = seq(ax2$min, ax2$max, by = ax2$by),
                                limits = c(ax2$min, ax2$max), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = pretty(x_years_full),
                                limits = c(xmin, xmax), expand = c(0, 0)) +
    ggplot2::labs(
      x = "Harvest Year", y = y_lab,
      title = paste0(
        "Cumulative carbon stored in ",
        if (pools == "piu") "Products in Use" else "SWDS",
        " — Domestic vs Imports\n",
        "(Domestic shows pre-", ownership_start_year, " legacy and continues through earlier years)"
      )
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}



# ---------- Example call ----------
p_dom_imp <- plot_carbon_storage_domestic_vs_imports(
  hwp,
  pools = "both",               # "both" | "piu" | "swds"
  ownership_start_year = 1952,  # mask before this year
  x_years_full = harv.hwp$Year  # or NULL to use array years
)
print(p_dom_imp)




# =========================================================
# 6) Monte Carlo Estimates
#    plot.type: "1" (pools facet) | "2" (combined pools) | "3" (convergence)
#    metrictype: "TgC" | "CO2e"
#    You may pass mc_plot / mc_total / mc_iters explicitly, or keep NULL to pull from `hwp`.
# =========================================================

suppressWarnings(suppressMessages(require(ggplot2)))
suppressWarnings(suppressMessages(require(grid)))  # for unit() and margin()

# ---- Theme ----
common_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 24),
    axis.text  = element_text(size = 18),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(22, "pt"),
    legend.key.width = unit(25, "pt"),
    legend.box.margin = margin(8, 10, 0, 10),
    legend.spacing.x = unit(14, "pt"),
    plot.margin = margin(22, 18, 10, 14)
  )

# Safer `%||%`
`%||%` <- get0("%||%", inherits = TRUE, ifnotfound = NULL)
if (is.null(`%||%`)) `%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- small helpers for working with `hwp`
.hwp_get <- function(h, nm) {
  out <- tryCatch(h[[nm]], error = function(...) NULL)
  if (!is.null(out)) return(out)
  if (is.environment(h) && exists(nm, envir = h, inherits = FALSE)) {
    return(get(nm, envir = h, inherits = FALSE))
  }
  NULL
}

.hwp_set <- function(h, nm, val) {
  if (is.environment(h)) assign(nm, val, envir = h) else h[[nm]] <- val
  invisible(h)
}

# =========================================================
# Option B: ensure every pool facet has a readable label
# - Keeps existing pretty names in `hwp$C.names`
# - Fills any missing entries (e.g., "Total") with identity labels
# =========================================================
ensure_pool_labels <- function(hwp, verbose = TRUE) {
  mp <- .hwp_get(hwp, "mc_plot")
  if (is.null(mp) || !nrow(as.data.frame(mp))) {
    if (verbose) message("ensure_pool_labels: `mc_plot` is missing/empty; nothing to label yet.")
    return(hwp)
  }
  pools <- unique(as.character(as.data.frame(mp)$Type.M))
  pools <- pools[is.finite(match(pools, pools))]  # drop any weird NAs
  cn <- .hwp_get(hwp, "C.names")
  if (is.null(cn)) cn <- character()
  missing <- setdiff(pools, names(cn))
  if (length(missing)) {
    cn <- c(cn, stats::setNames(missing, missing))
    .hwp_set(hwp, "C.names", cn)
    if (verbose) message("ensure_pool_labels: filled labels for: ", paste(missing, collapse = ", "))
  } else if (verbose) {
    message("ensure_pool_labels: all pools already labeled.")
  }
  hwp
}

# =========================================================
# Robust labeller (prevents ggplot strip errors with incomplete C.names)
# =========================================================
.safe_pool_labeller <- function(hwp) {
  cn <- .hwp_get(hwp, "C.names")
  if (is.null(cn)) return(ggplot2::label_value)
  ggplot2::labeller(
    Type.M = function(v) {
      v <- as.character(v)
      m <- unname(cn[match(v, names(cn))])
      out <- v
      repl <- !is.na(m) & nzchar(m)
      out[repl] <- m[repl]
      out
    }
  )
}

# =========================================================
# Column standardizers used by the plotter
# =========================================================
.std_mc_plot <- function(df) {
  df <- as.data.frame(df); nm <- names(df)
  if (!"Year"  %in% nm) { cand <- intersect(c("Year","year","YEAR","yr"), nm);  if (length(cand)) names(df)[match(cand[1], nm)] <- "Year" }
  if (!"Means" %in% nm) { cand <- intersect(c("Means","Mean","mean","avg"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "Means" }
  if (!"lci"   %in% nm) { cand <- intersect(c("lci","LCI","lwr","lo","lower","ciLCI"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "lci" }
  if (!"uci"   %in% nm) { cand <- intersect(c("uci","UCI","upr","hi","upper","ciUCI"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "uci" }
  if (!"Type.M"%in% nm) { cand <- intersect(c("Type.M","Type","Pool","Series","series","pool","compartment"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "Type.M" }
  for (v in c("Year","Means","lci","uci")) if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df
}

.std_mc_total <- function(df) {
  df <- as.data.frame(df); nm <- names(df)
  if (!"Year" %in% nm) { cand <- intersect(c("Year","year","YEAR","yr"), nm);   if (length(cand)) names(df)[match(cand[1], nm)] <- "Year" }
  if (!"Mean" %in% nm) { cand <- intersect(c("Mean","Means","mean","avg"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "Mean" }
  if (!"lci"  %in% nm) { cand <- intersect(c("lci","LCI","lwr","lo","lower","ciLCI"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "lci" }
  if (!"uci"  %in% nm) { cand <- intersect(c("uci","UCI","upr","hi","upper","ciUCI"), nm); if (length(cand)) names[df][match(cand[1], nm)] <- "uci" }
  for (v in c("Year","Mean","lci","uci")) if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df
}

# =========================================================
# Hardened plotter that uses the safe labeller
# - Accepts explicit mc_* inputs or pulls from `hwp`
# - Uses .safe_pool_labeller(hwp) so missing labels never crash
# =========================================================
plot_mc_estimates <- function(
    hwp,
    plot.type  = c("1","2","3"),
    metrictype = c("TgC","CO2e"),
    mc_plot  = NULL,
    mc_total = NULL,
    mc_iters = NULL
) {
  plot.type  <- match.arg(as.character(plot.type), c("1","2","3"))
  metrictype <- match.arg(metrictype)
  
  # helpers
  pull_mc <- function(container, variants) {
    for (nm in variants) {
      val <- tryCatch(container[[nm]], error = function(...) NULL)
      if (!is.null(val)) return(val)
    }
    if (is.environment(container)) {
      for (nm in variants) {
        if (exists(nm, envir = container, inherits = FALSE)) {
          return(get(nm, envir = container, inherits = FALSE))
        }
      }
    }
    for (nm in variants) {
      val <- get0(nm, inherits = TRUE, ifnotfound = NULL)
      if (!is.null(val)) return(val)
    }
    NULL
  }
  
  # pull/standardize inputs
  if (plot.type == "1") {
    mc_plot <- mc_plot %||% pull_mc(hwp, c("mc_plot","MC_plot","mc.plot","mcPlot"))
    if (is.null(mc_plot) || !nrow(as.data.frame(mc_plot))) {
      # graceful fallback to totals
      mc_total <- mc_total %||% pull_mc(hwp, c("mc_PoolsTotalPlot","mc_total","mc_total_plot","mc.PoolsTotalPlot"))
      if (is.null(mc_total)) stop("`mc_plot` is required for plot.type = '1', and no total is available to fallback.")
      mt <- .std_mc_total(mc_total)
      mc_plot <- data.frame(Year = mt$Year, Means = mt$Mean, lci = mt$lci, uci = mt$uci, Type.M = "Total")
    } else {
      mc_plot <- .std_mc_plot(mc_plot)
    }
  } else if (plot.type == "2") {
    mc_total <- mc_total %||% pull_mc(hwp, c("mc_PoolsTotalPlot","mc_total","mc_total_plot","mc.PoolsTotalPlot"))
    if (is.null(mc_total)) stop("`mc_total` (aka `mc_PoolsTotalPlot`) is required for plot.type = '2'.")
    mc_total <- .std_mc_total(mc_total)
  } else { # "3"
    mc_iters <- mc_iters %||% pull_mc(hwp, c("mc_iter_results","mc.iters","mc_iters","mcIterResults"))
    if (is.null(mc_iters)) stop("`mc_iters` (`mc_iter_results`) is required for plot.type = '3'.")
    # standardizer for iters (kept minimal since we only need numeric columns)
    std_mc_iters <- function(df) {
      df <- as.data.frame(df); nm <- names(df)
      if (!"iter"%in% nm) { cand <- intersect(c("iter","iteration","Iteration","it"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "iter" }
      if (!"C"   %in% nm) { cand <- intersect(c("C","value","Value","sum","total","Total"), nm); if (length(cand)) names[df][match(cand[1], nm)] <- "C" }
      if (!"stat"%in% nm) { cand <- intersect(c("stat","Stat","metric","which"), nm); if (length(cand)) names[df][match(cand[1], nm)] <- "stat" }
      if ("C" %in% names(df))    df$C    <- suppressWarnings(as.numeric(df$C))
      if ("iter" %in% names(df)) df$iter <- suppressWarnings(as.numeric(df$iter))
      df
    }
    mc_iters <- std_mc_iters(mc_iters)
    mc_total <- mc_total %||% pull_mc(hwp, c("mc_PoolsTotalPlot","mc_total","mc_total_plot","mc.PoolsTotalPlot"))
    if (!is.null(mc_total)) mc_total <- .std_mc_total(mc_total)
  }
  
  # metric conversion & labels
  ylab <- if (metrictype == "CO2e") "Tg CO\u2082e" else "Tg C"
  if (metrictype == "CO2e") {
    if (plot.type == "1") mc_plot[,  c("Means","lci","uci")] <- lapply(mc_plot[,  c("Means","lci","uci")], `*`, 44/12)
    if (plot.type == "2") mc_total[, c("Mean","lci","uci")]  <- lapply(mc_total[, c("Mean","lci","uci")],  `*`, 44/12)
    if (plot.type == "3" && "C" %in% names(mc_iters)) mc_iters$C <- mc_iters$C * (44/12)
  }
  
  # ---- use the robust labeller
  labber <- .safe_pool_labeller(hwp)
  ci_pct <- tryCatch(100 * .hwp_get(hwp, "MC.CI.REPORT"), error = function(...) 95)
  
  # ---- plots
  if (plot.type == "1") {
    ggplot(mc_plot, aes(Year, Means/1e6)) +
      geom_ribbon(aes(ymin = lci/1e6, ymax = uci/1e6), fill = "grey25") +
      geom_line(color = "yellow") +
      facet_wrap(~ Type.M, labeller = labber) +
      labs(
        x = NULL,
        y = ylab,
        title = paste0("MC mean (yellow) and ", ci_pct, "% CI (band) — storage & emission pools")
      ) +
      common_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if (plot.type == "2") {
    ggplot(mc_total, aes(Year, Mean)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), fill = "grey25") +
      geom_line(color = "yellow") +
      labs(
        x = NULL,
        y = ylab,
        title = paste0("MC mean (yellow) and ", ci_pct, "% CI (band) — PIU + SWDS")
      ) +
      common_theme
    
  } else {  # plot.type == "3"
    end_yr <- if (!is.null(mc_total) && "Year" %in% names(mc_total)) tail(mc_total$Year, 1L) else NA
    lab_map <- c(
      mean   = "Mean",
      se     = "Standard Error",
      ciUCI  = paste0(ci_pct, "% CI, Upper"),
      ciLCI  = paste0(ci_pct, "% CI, Lower")
    )
    mc_iters$facet.labs <- unname(lab_map[as.character(mc_iters$stat)])
    mc_iters$facet.labs[is.na(mc_iters$facet.labs)] <- as.character(mc_iters$stat)
    mc_iters$C <- mc_iters$C / 1e6
    
    ggplot(mc_iters, aes(iter, C)) +
      geom_line() +
      facet_wrap(~ facet.labs, scales = "free_y") +
      labs(
        x = "Iterations",
        y = ylab,
        title = paste0(
          "Convergence — PIU + SWDS",
          if (!is.na(end_yr)) paste0(", ", end_yr) else "",
          " (N = ", tryCatch(.hwp_get(hwp, "N.ITER"), error = function(...) NA), ")"
        )
      ) +
      common_theme
  }
}

# =========================================================
# Usage:
# 1) Make sure `hwp$mc_plot` (or totals to fall back to) exist.
# 2) Run once to fill any missing labels (e.g., "Total"):
hwp <- ensure_pool_labels(hwp)

# 3) Plot normally:
p1 <- plot_mc_estimates(hwp, plot.type = "1", metrictype = "TgC"); print(p1)
p2 <- plot_mc_estimates(hwp, plot.type = "2", metrictype = "TgC"); print(p2)
p3 <- plot_mc_estimates(hwp, plot.type = "3", metrictype = "TgC"); print(p3)
# =========================================================




# ---------- simple file saver ----------
save_plot_png <- function(p, file, width = 8, height = 5, dpi = 300) {
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height, dpi = dpi)
}
