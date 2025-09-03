# HWP_Plots_Standalone.R

install.packages("ggpattern")

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridisLite)
library(abind)
library(reshape2) 
library(ggpattern)
library(scales)
library(tidyselect)


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
    # check core dims match (EndUse x Ownership x Year)
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
    # leave "Total" alone (BBF only used if you ask for BBF plots)
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
  # choose ~6 breaks
  by <- signif(span / 6, 1)
  list(min = if (positive_only) 0 else floor(rng[1]/by)*by,
       max = ceiling(rng[2]/by)*by,
       by = by)
}
`%||%` <- function(a,b) if (is.null(a)) b else a

.get_years <- function(hwp) {
  as.numeric(dimnames(hwp$eu_array)[[3]])
}
.get_ownerships <- function(hwp) {
  dimnames(hwp$eu_array)[[2]]
}
.idx_total <- function(hwp) {
  owns <- .get_ownerships(hwp)
  if ("Total" %in% owns) which(owns=="Total") else length(owns) # fallback to last
}
.as_co2e <- function(v, metrictype) if (metrictype=="CO2e") v * (44/12) else v
.lab_co2e <- function(metrictype) if (metrictype=="CO2e") expression("Tg C"*O[2]*e) else "Tg C"



# =========================================================
# Annual timber harvest by PRODUCT CATEGORY (EndUseID bins)
#   metric:  "MMTC" | "CO2e" | "BBF"   (arrays assumed in MMT C)
#   summary: "annual" | "cumulative"
#   mode:    "category_total" | "category" | "total"
# =========================================================

plot_ann_timber_by_enduse_bins <- function(
    hwp,
    metric  = c("MMTC","CO2e","BBF"),
    summary = c("annual","cumulative"),
    mode    = c("category_total","category","total"),
    keep_exports = TRUE
) {
  metric  <- match.arg(metric)
  summary <- match.arg(summary)
  mode    <- match.arg(mode)
  
  years <- .get_years(hwp)
  if (!exists(".axis_pretty", mode = "function")) {
    .axis_pretty <- function(x, positive_only = FALSE, n = 6) {
      x <- x[is.finite(x)]
      if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
      rng <- range(x, na.rm = TRUE); if (positive_only) rng[1] <- min(0, rng[1])
      br <- pretty(rng, n = n); list(min = min(br), max = max(br), by = diff(br)[1])
    }
  }
  
  # ---------- Collapse ownerships: drop "Total" after 1951; keep Exports as negative ----------
  owns_raw   <- .get_ownerships(hwp)
  owns_clean <- gsub("\\.", " ", owns_raw)
  
  idx_total   <- which(owns_clean == "Total")
  idx_exports <- which(owns_clean == "Exports")
  
  # base owners exclude "Total"; optionally exclude "Exports"
  idx_keep <- setdiff(seq_along(owns_clean), idx_total)
  if (!keep_exports) idx_keep <- setdiff(idx_keep, idx_exports)
  eu <- hwp$eu_array[, idx_keep, , drop = FALSE]  # [EndUseID, owner, year]
  
  # flip Exports negative if kept
  if (keep_exports && length(idx_exports)) {
    exp_in_keep <- which(owns_clean[idx_keep] == "Exports")
    if (length(exp_in_keep)) eu[, exp_in_keep, ] <- -eu[, exp_in_keep, , drop = FALSE]
  }
  
  # Sum across non-Total owners -> [year x EndUseID] in MMT C
  mat_enduse <- t(apply(eu, c(1, 3), sum)) / 1e6
  
  # For years <= 1951, use "Total" slice if non-Total owners are zero
  if (length(idx_total)) {
    mat_total <- t(apply(hwp$eu_array[, idx_total, , drop = FALSE], c(1, 3), sum)) / 1e6
    pre_mask <- years <= 1951
    zero_rows <- rowSums(mat_enduse, na.rm = TRUE) == 0
    rows_to_replace <- pre_mask & zero_rows
    if (any(rows_to_replace)) mat_enduse[rows_to_replace, ] <- mat_total[rows_to_replace, ]
  }
  # --------------------------------------------------------------------------------------------
  
  # ---- EndUseID -> category bins (first match wins) ----
  bin_defs <- list(
    "Fuel" = c(1, 48, 95, 142, 197),
    "Furniture" = c(5, 20, 28, 42, 52, 68, 81, 84, 99, 116, 121, 139, 144, 155, 165, 186),
    "Housing and Construction" = c(
      10,21,27,36,57,65,73,91,104,115,125,132,145,164,170,183,
      12,14,24,37,56,69,78,86,100,108,128,134,146,162,169,184,
      11,15,26,46,58,62,79,85,102,114,133,152,154,168,185,
      8,17,29,39,54,70,75,90,103,111,123,138,147,158,175,181,
      7,18,34,40,59,67,72,93,101,113,127,140,148,160,167,178
    ),
    # NEW: Residential Repair & Remodeling (R&R)
    "Residential Repair and Remodeling" = c(9, 16, 33, 38, 49, 66, 74, 83, 105, 110, 126, 130, 143, 161, 171, 187),
    "Packaging & Shipping" = c(4, 22, 31, 44, 50, 64, 77, 92, 97, 112, 119, 131, 150, 157, 173, 182),
    "Manufacturing Misc." = c(2, 13, 30, 43, 51, 60, 80, 87, 106, 107, 118, 136, 149, 159, 166, 180),
    "Other Industrial Products" = c(35, 82, 129, 176),
    # Rail UPDATED: removed R&R IDs listed above
    "Rail"  = c(3,19,25,41,53,63,71,89,96,117,122,137,153,163,172,177),
    "Paper" = c(47, 94, 141, 188),
    "Softwood Misc." = c(206,204,216,222,208,198,220,194,212,214,200,192,190,196,218,224,202,210),
    "Hardwood Misc." = c(205,203,215,221,207,197,219,193,211,213,199,191,189,195,217,223,201,209),
    "Other, N.A." = c(6,23,32,45,55,61,76,88,98,109,120,135,151,156,174,179)
  )
  
  n_ids <- ncol(mat_enduse); if (is.null(n_ids)) n_ids <- 0
  if (n_ids == 0) stop("eu_array appears empty or has unexpected dimensions.")
  
  # map IDs; drop anything unmapped (no 'Miscellaneous')
  id_to_cat <- rep(NA_character_, n_ids)
  for (cat in names(bin_defs)) {
    ids  <- intersect(bin_defs[[cat]], seq_len(n_ids))
    free <- ids[is.na(id_to_cat[ids])]
    id_to_cat[free] <- if (cat == "Other, N.A.") "Other" else cat
  }
  keep_cols <- which(!is.na(id_to_cat))  # remove "Miscellaneous" entirely
  id_to_cat <- id_to_cat[keep_cols]
  mat_enduse <- mat_enduse[, keep_cols, drop = FALSE]
  
  # aggregate by category
  present <- unique(id_to_cat)
  cat_mat <- sapply(present, function(cat) {
    cols <- which(id_to_cat == cat)
    rowSums(mat_enduse[, cols, drop = FALSE], na.rm = TRUE)
  })
  cat_mat <- as.matrix(cat_mat)
  rownames(cat_mat) <- years
  
  # ---- Long df ----
  df <- as.data.frame(cat_mat)
  df$Year <- years
  long <- tidyr::pivot_longer(df, -Year, names_to = "Category", values_to = "Value")
  
  if (summary == "cumulative") {
    long <- long |>
      dplyr::group_by(Category) |>
      dplyr::mutate(Value = cumsum(Value)) |>
      dplyr::ungroup()
  }
  
  # Metric label/convert
  ylab <- "MMT C"
  if (metric == "CO2e") { long$Value <- long$Value * (44/12); ylab <- expression("MMT C"*O[2]*e) }
  if (metric == "BBF")  { ylab <- "BBF" }
  
  # Totals + axis
  df_total <- long |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop")
  
  yr_env <- long |>
    dplyr::group_by(Year) |>
    dplyr::summarise(pos = sum(pmax(Value, 0), na.rm = TRUE),
                     neg = sum(pmin(Value, 0), na.rm = TRUE), .groups = "drop")
  ax <- .axis_pretty(c(yr_env$pos, yr_env$neg), positive_only = FALSE)
  
  # ---------- UPDATED palette & order ----------
  plot_levels <- c(
    "Fuel","Furniture","Housing and Construction","Residential Repair and Remodeling",
    "Packaging & Shipping","Manufacturing Misc.","Other Industrial Products","Rail","Paper",
    "Softwood Misc.","Hardwood Misc.","Other"
  )
  pal_cat <- c(
    "Fuel"                               = "#EE7733",
    "Furniture"                          = "#0077BB",
    "Housing and Construction"           = "#009988",
    "Residential Repair and Remodeling"  = "#6A3D9A",  # NEW color
    "Packaging & Shipping"               = "#33BBEE",
    "Manufacturing Misc."                = "#EE3377",
    "Other Industrial Products"          = "#CC3311",
    "Rail"                               = "#228833",
    "Paper"                              = "#CCBB44",
    "Softwood Misc."                     = "#332288",
    "Hardwood Misc."                     = "#AA4499",
    "Other"                              = "#999933"
  )
  present_levels <- intersect(plot_levels, unique(long$Category))
  long$Category  <- factor(long$Category, levels = present_levels)
  
  if (mode %in% c("category","category_total")) {
    p <- ggplot(long, aes(Year, Value, fill = Category)) +
      geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
      scale_fill_manual(values = pal_cat[present_levels],
                        breaks = present_levels,
                        name   = "Product category") +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                         limits = c(ax$min, ax$max), expand = c(0, 0)) +
      labs(x = "Harvest Year", y = ylab,
           title = paste0(if (summary == "cumulative") "Cumulative" else "Annual",
                          " timber harvest by product category")) +
      theme_bw(base_size = 14) +
      theme(legend.position = "right")
    
    if (mode == "category_total") {
      p <- p + geom_line(data = df_total, aes(Year, Total, color = "Total"),
                         linewidth = 0.9, inherit.aes = FALSE) +
        scale_color_manual(values = c(Total = "black"), name = NULL)
    }
    return(p)
  }
  
  ggplot(df_total, aes(Year, Total)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_line(linewidth = 1.0) +
    scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                       limits = c(ax$min, ax$max), expand = c(0, 0)) +
    labs(x = "Harvest Year", y = ylab,
         title = paste0(if (summary == "cumulative") "Cumulative" else "Annual",
                        " timber harvest (total)")) +
    theme_bw(base_size = 14)
}

# Example call
p_cat <- plot_ann_timber_by_enduse_bins(
  hwp,
  metric  = "MMTC",        # or "CO2e"
  summary = "annual",      # or "cumulative"
  mode    = "category_total"  # "category" or "total"
)

print(p_cat)




# =========================================================
# 1) Annual Harvest and Trade (AnnHarvestandTrade)
#    metric: "MMTC" | "CO2e" | "BBF"
#    summary: "annual" | "cumulative"
#    mode: "ownership_total" (stack owners + total line),
#          "ownership" (stack owners only),
#          "total" (total line only)
# Includes safe Exports derivation!
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
  
  years    <- .get_years(hwp)
  owns_raw <- .get_ownerships(hwp)
  
  # Labels & ordering
  owns_lab          <- gsub("\\.", " ", owns_raw)
  owns_lab_no_total <- setdiff(owns_lab, "Total")
  presplit_name <- "All ownerships (pre-1952)"
  base_order    <- c("Imports","BLM","USFS","State","Private and Tribal", presplit_name, "Exports")
  legend_order  <- intersect(base_order, c(owns_lab_no_total, presplit_name))
  
  # ---- Build ownership matrix as NUMERIC MATRIX (MMT C) ----
  mat <- t(apply(hwp$eu_array, c(2,3), sum)) / 1e6   # [year, owner]
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
    mat <- cbind(mat, presplit_col)           # stays a numeric matrix
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
  
  remainder     <- pmax(total_for_remainder - sum_known, 0)
  mask_presplit <- years < own_start
  mat[mask_presplit, presplit_name] <- remainder[mask_presplit]
  
  # ---- Long df (drop Total) ----
  own_stack <- as.data.frame(mat)
  own_stack$Year <- years
  own_stack <- tidyr::pivot_longer(own_stack, -Year, names_to = "Ownership", values_to = "Value") |>
    dplyr::filter(Ownership != "Total")
  
  if (summary == "cumulative") {
    own_stack <- own_stack |>
      dplyr::group_by(Ownership) |>
      dplyr::mutate(Value = cumsum(Value)) |>
      dplyr::ungroup()
  }
  
  # Exports below zero
  if ("Exports" %in% unique(own_stack$Ownership)) {
    own_stack$Value[own_stack$Ownership == "Exports"] <- -own_stack$Value[own_stack$Ownership == "Exports"]
  }
  
  # Metric label/convert if CO2e
  ylab <- "MMT C"
  if (metric == "CO2e") {
    own_stack$Value <- own_stack$Value * (44/12)
    ylab <- expression("MMT C"*O[2]*e)
  }
  
  # Split: invisible pre-1952 vs. colored ownerships
  avail_all   <- intersect(legend_order, unique(own_stack$Ownership))
  main_levels <- setdiff(avail_all, presplit_name)
  df_pre      <- dplyr::filter(own_stack, Ownership == presplit_name)
  df_main     <- dplyr::filter(own_stack, Ownership != presplit_name)
  if (nrow(df_main)) df_main$Ownership <- factor(df_main$Ownership, levels = main_levels)
  pal <- setNames(viridisLite::viridis(length(main_levels), option = "D", end = 0.95, begin = 0.05),
                  main_levels)
  
  # Reconciled total (sum of stack with negatives)
  df_total <- own_stack |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop")
  
  # Axis allowing negatives
  yr_env <- own_stack |>
    dplyr::group_by(Year) |>
    dplyr::summarise(pos = sum(pmax(Value, 0), na.rm = TRUE),
                     neg = sum(pmin(Value, 0), na.rm = TRUE), .groups = "drop")
  ax <- .axis_pretty(c(yr_env$pos, yr_env$neg), positive_only = FALSE)
  
  # Plot
  if (mode %in% c("ownership","ownership_total")) {
    p <- ggplot() + geom_hline(yintercept = 0, color = "black", linewidth = 1.0)
    
    # pre-1952 band: invisible; no legend
    if (nrow(df_pre)) {
      p <- p + geom_area(data = df_pre, aes(Year, Value),
                         inherit.aes = FALSE, fill = "transparent", color = NA, alpha = 0)
    }
    
    # regular ownerships (Imports first level → on top with reverse = FALSE)
    if (nrow(df_main)) {
      p <- p + geom_area(data = df_main,
                         aes(Year, Value, fill = Ownership),
                         alpha = 0.85, color = "white", linewidth = 0.2,
                         position = position_stack(reverse = FALSE)) +
        scale_fill_manual(values = pal, breaks = main_levels, name = "Ownership")
    }
    
    p <- p +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                         limits = c(ax$min, ax$max), expand = c(0, 0)) +
      labs(x = "Harvest Year", y = ylab,
           title = paste(summary, "timber harvest",
                         if (mode == "ownership_total") "by ownership + total" else "by ownership")) +
      theme_bw(base_size = 14) +
      theme(legend.position = "right")
    
    if (mode == "ownership_total") {
      p <- p +
        geom_line(data = df_total, aes(Year, Total, color = "Total"),
                  linewidth = 0.7, inherit.aes = FALSE) +
        scale_color_manual(values = c(Total = "black"), name = NULL)
    }
    return(p)
  } else {
    ggplot(df_total, aes(Year, Total)) +
      geom_hline(yintercept = 0, color = "black", linewidth = 1.0) +  # <-- add this
      geom_line(linewidth = 1.0) +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                         limits = c(ax$min, ax$max), expand = c(0, 0)) +
      labs(x = "Harvest Year", y = ylab, title = paste(summary, "timber harvest (total)")) +
      theme_bw(base_size = 14)
  }
}


# Plot call
p1 <- plot_ann_timber_harvest(hwp,
                              metric="MMTC", summary="annual", mode="ownership_total",
                              ownership_start_year=1952, trade_start_year=2001)
print(p1); save_plot_png(p1, "Plot_AnnHarvestandTrade.png")



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
print(p2); save_plot_png(p2, "Plot_AnNetChCStor_Production.png")

# ---- Example: Simple Decay approach with Net line ----
p2 <- plot_annual_net_change(
  hwp,
  approach       = "simple_decay",
  metrictype     = "MMTC",
  include_net_line = TRUE
)
print(p2); save_plot_png(p2, "Plot_AnNetChCStor_SimpleDecay.png")



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
#                          swdsCtotal_array [EndUse, Ownership, Year]
# =========================================================

plot_carbon_storage_by_product_category <- function(
    hwp,
    pools        = c("both","piu","swds"),
    metric       = c("MMTC","CO2e")
) {
  pools  <- match.arg(pools)
  metric <- match.arg(metric)
  
  # ---- inputs ----
  pu   <- hwp$pu.final_array
  swds <- hwp$swdsCtotal_array
  if (is.null(pu) || is.null(swds))
    stop("hwp$pu.final_array and hwp$swdsCtotal_array are required.")
  
  years <- hwp$years
  if (is.null(years)) {
    years <- as.numeric(dimnames(pu)[[3]])
    if (is.null(years)) stop("hwp$years is required (or set in array dimnames).")
  }
  
  # ---- helper: take TOTAL owner if present; else sum owners ----
  clean <- function(x) gsub("\\.", " ", trimws(x))
  sum_total_or_owners <- function(arr) {
    own <- clean(dimnames(arr)[[2]])
    idx_total <- which(own == "Total")
    # EU x Year in metric tonnes C
    mat <- if (length(idx_total)) {
      apply(arr[, idx_total, , drop = FALSE], c(1, 3), sum, na.rm = TRUE)
    } else {
      apply(arr, c(1, 3), sum, na.rm = TRUE)  # sum across all owners
    }
    mat / 1e6  # -> MMT C
  }
  
  eu_piu  <- sum_total_or_owners(pu)   # [EndUse x Year] MMT C
  eu_swds <- sum_total_or_owners(swds) # [EndUse x Year] MMT C
  
  # choose pool(s)
  eu_year <- switch(pools,
                    piu  = eu_piu,
                    swds = eu_swds,
                    both = eu_piu + eu_swds)
  
  n_eu <- nrow(eu_year)
  
  # ---- EndUseID -> product category bins (first match wins) ----
  clamp <- function(v) intersect(v, seq_len(n_eu))
  bin_defs <- list(
    "Fuel" = c(1, 48, 95, 142, 197),
    "Furniture" = c(5, 20, 28, 42, 52, 68, 81, 84, 99, 116, 121, 139, 144, 155, 165, 186),
    "Housing and Construction" = c(
      10,21,27,36,57,65,73,91,104,115,125,132,145,164,170,183,
      12,14,24,37,56,69,78,86,100,108,128,134,146,162,169,184,
      11,15,26,46,58,62,79,85,102,114,133,152,154,168,185,
      8,17,29,39,54,70,75,90,103,111,123,138,147,158,175,181,
      7,18,34,40,59,67,72,93,101,113,127,140,148,160,167,178
    ),
    "Residential Repair and Remodeling" = c(9,16,33,38,49,66,74,83,105,110,126,130,143,161,171,187),
    "Packaging & Shipping" = c(4,22,31,44,50,64,77,92,97,112,119,131,150,157,173,182),
    "Manufacturing Misc." = c(2,13,30,43,51,60,80,87,106,107,118,136,149,159,166,180),
    "Other Industrial Products" = c(35,82,129,176),
    "Rail" = c(3,19,25,41,53,63,71,89,96,117,122,137,153,163,172,177),
    "Paper" = c(47,94,141,188),
    "Softwood Misc." = c(206,204,216,222,208,198,220,194,212,214,200,192,190,196,218,224,202,210),
    "Hardwood Misc." = c(205,203,215,221,207,197,219,193,211,213,199,191,189,195,217,223,201,209),
    "Other" = c(6,23,32,45,55,61,76,88,98,109,120,135,151,156,174,179)
  )
  bin_defs <- lapply(bin_defs, clamp)
  bin_defs <- bin_defs[vapply(bin_defs, length, 1L) > 0]
  if (!length(bin_defs)) stop("No EndUse IDs matched the arrays' EndUse dimension.")
  
  # ---- aggregate to product categories (Year-wise) ----
  cat_mat <- sapply(names(bin_defs), function(cat) {
    ids <- bin_defs[[cat]]
    colSums(eu_year[ids, , drop = FALSE], na.rm = TRUE)  # vector per Year
  })
  cat_df <- as.data.frame(cat_mat)
  cat_df$Year <- years
  long <- tidyr::pivot_longer(cat_df, -Year, names_to = "Category", values_to = "Value")
  
  # ---- units ----
  ylab <- "MMT C"
  if (metric == "CO2e") { long$Value <- long$Value * (44/12); ylab <- expression("MMT C"*O[2]*e) }
  
  # ---- axis ----
  axis_pretty <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
    br <- pretty(range(x, na.rm = TRUE))
    list(min = min(br), max = max(br), by = diff(br)[1])
  }
  ax <- long |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Tot = sum(Value, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(Tot) |>
    axis_pretty()
  
  # ---- palette & order ----
  plot_levels <- c(
    "Fuel","Furniture","Housing and Construction","Residential Repair and Remodeling",
    "Packaging & Shipping","Manufacturing Misc.","Other Industrial Products","Rail","Paper",
    "Softwood Misc.","Hardwood Misc.","Other"
  )
  pal_cat <- c(
    "Fuel"                               = "#EE7733",
    "Furniture"                          = "#0077BB",
    "Housing and Construction"           = "#009988",
    "Residential Repair and Remodeling"  = "#6A3D9A",
    "Packaging & Shipping"               = "#33BBEE",
    "Manufacturing Misc."                = "#EE3377",
    "Other Industrial Products"          = "#CC3311",
    "Rail"                               = "#228833",
    "Paper"                              = "#CCBB44",
    "Softwood Misc."                     = "#332288",
    "Hardwood Misc."                     = "#AA4499",
    "Other"                              = "#999933"
  )
  present <- intersect(plot_levels, unique(long$Category))
  long$Category <- factor(long$Category, levels = present)
  
  ggplot2::ggplot(long, ggplot2::aes(Year, Value, fill = Category)) +
    ggplot2::geom_area(alpha = 0.9, color = "white", linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = pal_cat[present], breaks = present, name = "Product category") +
    ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                limits = c(ax$min, ax$max), expand = c(0, 0)) +
    ggplot2::labs(
      x = "Harvest Year",
      y = ylab,
      title = paste0("Cumulative carbon stored in ",
                     if (pools == "piu") "PIU"
                     else if (pools == "swds") "SWDS"
                     else "PIU and SWDS",
                     " by product category")
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}

# ---- Your call (unchanged) ----
p4 <- plot_carbon_storage_by_product_category(
  hwp,
  pools  = "both",  # or "piu", "swds"
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
# =========================================================
plot_cumulative_stocks_by_pool <- function(
    hwp,
    approach = c("production","simple_decay"),
    metric   = c("MMTC","CO2e"),
    include_total_line = TRUE
) {
  approach <- match.arg(approach)
  metric   <- match.arg(metric)
  
  # ---------- small helpers ----------
  axis_pretty <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(list(min = 0, max = 1, by = 0.2))
    br <- pretty(range(x, na.rm = TRUE))
    list(min = min(br), max = max(br), by = diff(br)[1])
  }
  clean <- function(x) gsub("\\.", " ", trimws(x))
  # prefer "Total" slice; otherwise sum owners (optionally exclude Exports)
  series_total_or_sum <- function(arr, exclude_exports = FALSE) {
    own <- clean(dimnames(arr)[[2]])
    idx_total   <- which(own == "Total")
    idx_exports <- which(own == "Exports")
    if (length(idx_total)) {
      apply(arr[, idx_total, , drop = FALSE], 3, sum, na.rm = TRUE)
    } else {
      keep <- seq_along(own)
      if (exclude_exports && length(idx_exports)) keep <- setdiff(keep, idx_exports)
      apply(arr[, keep, , drop = FALSE], 3, sum, na.rm = TRUE)
    }
  }
  
  # ---------- core inputs ----------
  years <- hwp$years
  if (is.null(years)) {
    years <- as.numeric(dimnames(hwp$pu.final_array)[[3]])
    if (is.null(years)) stop("hwp$years is required.")
  }
  
  # ---------- PRODUCTION approach ----------
  if (approach == "production") {
    if (is.null(hwp$pu.final_array) || is.null(hwp$swdsCtotal_array))
      stop("Production approach needs hwp$pu.final_array and hwp$swdsCtotal_array.")
    pu   <- series_total_or_sum(hwp$pu.final_array)   / 1e6  # MMT C
    swds <- series_total_or_sum(hwp$swdsCtotal_array) / 1e6
    
    df <- data.frame(
      Year  = years,
      PIU   = pu,
      SWDS  = swds,
      Total = pu + swds
    )
    
  } else {
    # ---------- SIMPLE-DECAY approach ----------
    if (is.null(hwp$eu_array) || is.null(hwp$eec_array) || is.null(hwp$ewoec_array))
      stop("Simple-decay approach needs hwp$eu_array, hwp$eec_array, and hwp$ewoec_array.")
    
    # Consumption inflow = Domestic + Imports − Exports (sum EndUse & owners, exclude "Total")
    owns <- clean(dimnames(hwp$eu_array)[[2]])
    idx_total   <- which(owns == "Total")
    idx_exports <- which(owns == "Exports")
    idx_keep    <- setdiff(seq_along(owns), idx_total)
    
    eu <- hwp$eu_array[, idx_keep, , drop = FALSE]
    if (length(idx_exports)) {
      exp_in_keep <- which(owns[idx_keep] == "Exports")
      if (length(exp_in_keep)) eu[, exp_in_keep, ] <- -eu[, exp_in_keep, , drop = FALSE]
    }
    inflow <- apply(eu, 3, sum, na.rm = TRUE) / 1e6  # MMT C
    
    # Emissions (prefer "Total"; otherwise sum owners but exclude Exports)
    eec   <- series_total_or_sum(hwp$eec_array,   exclude_exports = TRUE) / 1e6
    ewoec <- series_total_or_sum(hwp$ewoec_array, exclude_exports = TRUE) / 1e6
    
    # Total simple-decay stock (cumulative)
    total_sd <- cumsum(inflow - eec - ewoec)
    
    # Try to find pool-level simple-decay stocks; otherwise split by Production shares
    get_first <- function(x, nm) if (!is.null(x[[nm]])) x[[nm]] else NULL
    pu_sd   <- get_first(hwp, "pu.simple_array")
    sw_sd   <- get_first(hwp, "swds.simple_array")
    if (!is.null(pu_sd) && !is.null(sw_sd)) {
      # Sum EndUse; prefer "Total"
      pu_sd <- series_total_or_sum(pu_sd) / 1e6
      sw_sd <- series_total_or_sum(sw_sd) / 1e6
      df <- data.frame(Year = years, PIU = pu_sd, SWDS = sw_sd, Total = pu_sd + sw_sd)
    } else {
      # Fallback: use Production shares for the split
      if (is.null(hwp$pu.final_array) || is.null(hwp$swdsCtotal_array))
        stop("To split simple-decay stocks by pool, production stocks are needed (pu.final_array & swdsCtotal_array).")
      pu_prod   <- series_total_or_sum(hwp$pu.final_array)   / 1e6
      sw_prod   <- series_total_or_sum(hwp$swdsCtotal_array) / 1e6
      tot_prod  <- pu_prod + sw_prod
      pu_share  <- ifelse(tot_prod > 0, pu_prod / tot_prod, 0)
      sw_share  <- 1 - pu_share
      df <- data.frame(
        Year  = years,
        PIU   = total_sd * pu_share,
        SWDS  = total_sd * sw_share,
        Total = total_sd
      )
    }
  }
  
  # ---------- unit conversion ----------
  ylab <- "MMT C"
  if (metric == "CO2e") {
    df[c("PIU","SWDS","Total")] <- df[c("PIU","SWDS","Total")] * (44/12)
    ylab <- expression("MMT C"*O[2]*e)
  }
  
  # ---------- plot ----------
  ax <- axis_pretty(df$Total)
  long <- tidyr::pivot_longer(df, c(PIU, SWDS), names_to = "Pool", values_to = "Value")
  long$Pool <- factor(long$Pool, levels = c("PIU","SWDS"))
  
  p <- ggplot2::ggplot(long, ggplot2::aes(Year, Value, fill = Pool)) +
    ggplot2::geom_area(alpha = 0.9, color = "white", linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::scale_fill_manual(
      values = c(PIU = "#4B2E83", SWDS = "#B42E8D"),
      labels = c("Products in Use", "Solid Waste Disposal Sites"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by),
                                limits = c(ax$min, ax$max), expand = c(0, 0)) +
    ggplot2::labs(
      x = "Harvest Year",
      y = ylab,
      title = paste0(
        "Cumulative carbon stocks by pool — ",
        if (approach == "production") "Production" else "Simple-decay",
        " approach"
      )
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
  
  if (isTRUE(include_total_line)) {
    p <- p +
      ggplot2::geom_line(data = df, ggplot2::aes(Year, Total, color = "Total"),
                         linewidth = 1.05, inherit.aes = FALSE) +
      ggplot2::scale_color_manual(values = c(Total = "black"), name = NULL)
  }
  p
}

# ---------------------- Examples ----------------------
# Production approach (MMT C)
p_prod <- plot_cumulative_stocks_by_pool(
  hwp,
  approach = "production",
  metric   = "MMTC",
  include_total_line = TRUE
)
print(p_prod)

# Simple-decay approach (MMT C). If pool-level simple-decay stocks
# aren't present in `hwp`, the split uses Production pool shares.
p_sd <- plot_cumulative_stocks_by_pool(
  hwp,
  approach = "simple_decay",
  metric   = "MMTC",
  include_total_line = TRUE
)
print(p_sd)


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
  if (is.null(pu_arr) || is.null(swds_arr)) {
    stop("pu.final_array and swdsCtotal_array must be present in `hwp`.")
  }
  
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
  if (!length(owners)) {
    stop("None of the requested owners are present in both arrays.")
  }
  
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
      dplyr::filter(!is.na(Value))      # <-- drop masked rows to avoid warning
    
    series_levels <- levels(interaction(df_long$Owner, df_long$series, sep = " — ", drop = TRUE))
    col_map <- setNames(rep("#999999", length(series_levels)), series_levels)
    for (o in owners) {
      col_map[paste(o, "Products in Use", sep = " — ")] <- base_cols[o]
      col_map[paste(o, "SWDS",            sep = " — ")] <- scales::alpha(base_cols[o], 0.55)
    }
    
    ax <- axis_pretty(
      df_long |> dplyr::group_by(Year) |>
        dplyr::summarise(Tot = sum(Value, na.rm = TRUE), .groups = "drop") |>
        dplyr::pull(Tot)
    )
    
    return(
      ggplot2::ggplot(df_long,
                      ggplot2::aes(Year, Value, fill = interaction(Owner, series, sep = " — "))) +
        ggplot2::geom_area(na.rm = TRUE) +                      # <-- silence warning
        ggplot2::scale_fill_manual(values = col_map, name = NULL) +
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
  df_plot <- df |> dplyr::arrange(Owner, Year) |> dplyr::filter(!is.na(Value))  # <-- drop NAs
  ax <- axis_pretty(
    df_plot |> dplyr::group_by(Year) |>
      dplyr::summarise(Tot = sum(Value, na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(Tot)
  )
  
  ggplot2::ggplot(df_plot, ggplot2::aes(Year, Value, fill = Owner)) +
    ggplot2::geom_area(na.rm = TRUE) +                          # <-- silence warning
    ggplot2::scale_fill_manual(values = base_cols, name = NULL) +
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

dimnames(hwp$pu.final_array)[[2]]
dimnames(hwp$swdsCtotal_array)[[2]]
hwp$ownership.names





# =========================================================
# 6) Monte Carlo Estimates
#    plot.type: 1 (pools facet) | 2 (combined pools) | 3 (convergence)
# =========================================================
plot_mc_estimates <- function(hwp,
                              plot.type = c(1,2,3),
                              metrictype = c("TgC","CO2e")) {
  plot.type <- match.arg(as.character(plot.type))
  metrictype <- match.arg(metrictype)
  
  if (is.null(hwp$mc_iter_results) || is.null(hwp$mc_plot) || is.null(hwp$mc_PoolsTotalPlot)) {
    stop("Monte Carlo outputs not found: need mc_plot, mc_PoolsTotalPlot, mc_iter_results in `hwp`.")
  }
  
  # clone inputs; apply metric conversion if needed
  mc_plot <- hwp$mc_plot
  mc_total <- hwp$mc_PoolsTotalPlot
  mc_iters <- hwp$mc_iter_results
  ylab <- .lab_co2e(if (metrictype=="CO2e") "CO2e" else "TgC")
  
  if (metrictype == "CO2e") {
    mc_plot[, 3:5] <- mc_plot[, 3:5] * (44/12)
    mc_total[, 2:4] <- mc_total[, 2:4] * (44/12)
    mc_iters$C <- mc_iters$C * (44/12)
  }
  
  if (plot.type == "1") {
    ggplot(mc_plot, aes(Year, Means/1e6)) +
      geom_ribbon(aes(ymin = lci/1e6, ymax = uci/1e6)) +
      geom_line(color = "yellow") +
      facet_wrap(~ Type.M, labeller = hwp$C.names) +
      labs(x=NULL, y=ylab,
           title=paste0("MC mean (yellow) and ", 100*hwp$MC.CI.REPORT, "% CI (black) — storage & emission pools")) +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  } else if (plot.type == "2") {
    ggplot(mc_total, aes(Year, Mean)) +
      geom_ribbon(aes(ymin=lci, ymax=uci)) +
      geom_line(color="yellow") +
      labs(x=NULL, y=ylab,
           title=paste0("MC mean (yellow) and ", 100*hwp$MC.CI.REPORT, "% CI (black) — PIU + SWDS")) +
      theme_bw(base_size = 14)
  } else {
    end_yr <- mc_total$Year[nrow(mc_total)]
    mc_iters$facet.labs <- as.character(sapply(mc_iters$stat, switch,
                                               mean = "Mean",
                                               se   = "Standard Error",
                                               ciUCI= paste0(100*hwp$MC.CI.REPORT, "% CI, Upper"),
                                               ciLCI= paste0(100*hwp$MC.CI.REPORT, "% CI, Lower")
    ))
    mc_iters$C <- mc_iters$C/1e6
    ggplot(mc_iters, aes(iter, C)) +
      geom_line() +
      facet_wrap(~ facet.labs, scales = "free_y") +
      labs(x="Iterations", y=ylab,
           title=paste0("Convergence — PIU + SWDS, ", end_yr, " (N = ", hwp$N.ITER, ")")) +
      theme_bw(base_size = 14)
  }
}

# 5) Monte Carlo Estimates — facet by pool (type 1)
p6 <- plot_mc_estimates(hwp, plot.type=1, metrictype="TgC")
print(p6); save_plot_png(p5, "Plot_MC_facet.png")


# ---------- simple file saver ----------
save_plot_png <- function(p, file, width = 8, height = 5, dpi = 300) {
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height, dpi = dpi)
}
