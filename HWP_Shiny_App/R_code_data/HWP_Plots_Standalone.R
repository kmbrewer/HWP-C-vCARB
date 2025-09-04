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
    y_min = NA,    # optional fixed y-limits; leave NA to auto-compute
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
    df[c("PIU","SWDS","Total")] <- lapply(df[c("PIU","SWDS","Total")], function(v) v * (44/12))
  }
  ylab <- if (metric == "CO2e") expression("MMT CO"[2]*"e") else "MMT C"
  
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
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  if (isTRUE(include_total_line)) {
    p <- p +
      ggplot2::geom_line(
        data = df, ggplot2::aes(Year, Total, color = "Total"),
        linewidth = 0.8, inherit.aes = FALSE
      ) +
      ggplot2::scale_color_manual(values = c(Total = "black"), name = NULL)
  }
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
  y_lo <- y_min                      # <- force this value
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
    ggplot2::coord_cartesian(ylim = c(y_lo, y_hi), expand = FALSE, clip = "on") +  # <- belt & suspenders
    ggplot2::labs(x = "Year", y = ylab,
                  title = "Cumulative carbon accounting — Simple-decay approach") +
    ggplot2::theme_bw(base_size = 14)
  
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
# Production approach
p_prod <- plot_cumulative_stocks_by_pool(hwp, metric = "MMTC", include_total_line = TRUE)
print(p_prod)

# Simple-decay cumulative (force axis if desired)
p_sd <- plot_cumulative_simple_decay(
  hwp,
  metric = "MMTC",
  include_net_line = TRUE,
  y_min = -500,  # negative bound
  y_max =  900   # positive bound
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



# =========================================================
# 6) Monte Carlo Estimates
#    plot.type: "1" (pools facet) | "2" (combined pools) | "3" (convergence)
#    metrictype: "TgC" | "CO2e"
#    You may pass mc_plot / mc_total / mc_iters explicitly, or keep NULL to pull from `hwp`.
# =========================================================
plot_mc_estimates <- function(
    hwp,
    plot.type  = c("1","2","3"),
    metrictype = c("TgC","CO2e"),
    mc_plot  = NULL,
    mc_total = NULL,
    mc_iters = NULL
) {
  # ---------- resolve args ----------
  plot.type  <- match.arg(as.character(plot.type), c("1","2","3"))
  metrictype <- match.arg(metrictype)
  
  # ---------- helpers ----------
  as_num <- function(x) suppressWarnings(as.numeric(x))
  
  pull_mc <- function(container, variants) {
    # try explicit element access
    for (nm in variants) {
      val <- tryCatch(container[[nm]], error = function(...) NULL)
      if (!is.null(val)) return(val)
      # if `hwp` is an environment
      if (is.environment(container) && exists(nm, envir = container, inherits = FALSE)) {
        return(get(nm, envir = container, inherits = FALSE))
      }
    }
    # fallback: try from the global env
    for (nm in variants) {
      val <- get0(nm, inherits = TRUE, ifnotfound = NULL)
      if (!is.null(val)) return(val)
    }
    NULL
  }
  
  std_mc_plot <- function(df) {
    df <- as.data.frame(df); nm <- names(df)
    if (!"Means" %in% nm) { cand <- intersect(c("Means","Mean","mean","avg"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "Means" }
    if (!"lci"   %in% nm) { cand <- intersect(c("lci","LCI","lwr","lo","lower","ciLCI"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "lci" }
    if (!"uci"   %in% nm) { cand <- intersect(c("uci","UCI","upr","hi","upper","ciUCI"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "uci" }
    if (!"Type.M"%in% nm) { cand <- intersect(c("Type.M","Type","Pool","Series","series","pool"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "Type.M" }
    for (v in c("Year","Means","lci","uci")) if (v %in% names(df)) df[[v]] <- as_num(df[[v]])
    df
  }
  
  std_mc_total <- function(df) {
    df <- as.data.frame(df); nm <- names(df)
    if (!"Mean" %in% nm) { cand <- intersect(c("Mean","Means","mean","avg"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "Mean" }
    for (v in c("Year","Mean","lci","uci")) if (v %in% names(df)) df[[v]] <- as_num(df[[v]])
    df
  }
  
  std_mc_iters <- function(df) {
    df <- as.data.frame(df); nm <- names(df)
    if (!"iter"%in% nm) { cand <- intersect(c("iter","iteration","Iteration","it"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "iter" }
    if (!"C"   %in% nm) { cand <- intersect(c("C","value","Value","sum","total","Total"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "C" }
    if (!"stat"%in% nm) { cand <- intersect(c("stat","Stat","metric","which"), nm); if (length(cand)) names(df)[match(cand[1], nm)] <- "stat" }
    if ("C" %in% names(df))    df$C    <- as_num(df$C)
    if ("iter" %in% names(df)) df$iter <- as_num(df$iter)
    df
  }
  
  # ---------- pull only what we need ----------
  if (plot.type == "1") {
    mc_plot <- mc_plot %||% pull_mc(hwp, c("mc_plot","MC_plot","mc.plot","mcPlot"))
    if (is.null(mc_plot)) stop("`mc_plot` is required for plot.type = '1'.")
    mc_plot <- std_mc_plot(mc_plot)
  } else if (plot.type == "2") {
    mc_total <- mc_total %||% pull_mc(hwp, c("mc_PoolsTotalPlot","mc_total","mc_total_plot","mc.PoolsTotalPlot"))
    if (is.null(mc_total)) stop("`mc_total` (aka `mc_PoolsTotalPlot`) is required for plot.type = '2'.")
    mc_total <- std_mc_total(mc_total)
  } else { # "3"
    mc_iters <- mc_iters %||% pull_mc(hwp, c("mc_iter_results","mc.iters","mc_iters","mcIterResults"))
    if (is.null(mc_iters)) stop("`mc_iters` (`mc_iter_results`) is required for plot.type = '3'.")
    mc_iters <- std_mc_iters(mc_iters)
    # optional, for title year
    mc_total <- mc_total %||% pull_mc(hwp, c("mc_PoolsTotalPlot","mc_total","mc_total_plot","mc.PoolsTotalPlot"))
    if (!is.null(mc_total)) mc_total <- std_mc_total(mc_total)
  }
  
  # ---------- metric conversion ----------
  ylab <- if (metrictype == "CO2e") "Tg CO\u2082e" else "Tg C"
  if (metrictype == "CO2e") {
    if (plot.type == "1") mc_plot[,  c("Means","lci","uci")] <- lapply(mc_plot[,  c("Means","lci","uci")], `*`, 44/12)
    if (plot.type == "2") mc_total[, c("Mean","lci","uci")]  <- lapply(mc_total[, c("Mean","lci","uci")],  `*`, 44/12)
    if (plot.type == "3") mc_iters$C <- mc_iters$C * (44/12)
  }
  
  # ---------- labels ----------
  labber <- if (!is.null(hwp$C.names)) ggplot2::as_labeller(hwp$C.names) else ggplot2::label_value
  ci_pct <- tryCatch(100 * hwp$MC.CI.REPORT, error = function(...) 95)
  
  # ---------- plot ----------
  if (plot.type == "1") {
    ggplot2::ggplot(mc_plot, ggplot2::aes(Year, Means/1e6)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lci/1e6, ymax = uci/1e6), fill = "grey85") +
      ggplot2::geom_line(color = "yellow") +
      ggplot2::facet_wrap(~ Type.M, labeller = labber) +
      ggplot2::labs(
        x = NULL, y = ylab,
        title = paste0("MC mean (yellow) and ", ci_pct, "% CI (band) — storage & emission pools")
      ) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
  } else if (plot.type == "2") {
    ggplot2::ggplot(mc_total, ggplot2::aes(Year, Mean)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lci, ymax = uci), fill = "grey85") +
      ggplot2::geom_line(color = "yellow") +
      ggplot2::labs(
        x = NULL, y = ylab,
        title = paste0("MC mean (yellow) and ", ci_pct, "% CI (band) — PIU + SWDS")
      ) +
      ggplot2::theme_bw(base_size = 14)
    
  } else { # "3" — convergence
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
    
    ggplot2::ggplot(mc_iters, ggplot2::aes(iter, C)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ facet.labs, scales = "free_y") +
      ggplot2::labs(
        x = "Iterations", y = ylab,
        title = paste0("Convergence — PIU + SWDS", if (!is.na(end_yr)) paste0(", ", end_yr) else "",
                       " (N = ", tryCatch(hwp$N.ITER, error = function(...) NA), ")")
      ) +
      ggplot2::theme_bw(base_size = 14)
  }
}




# Use objects inside `hwp`
p1 <- plot_mc_estimates(hwp, plot.type = "1", metrictype = "TgC")


# Or pass explicitly (works even if the names differ in your object)
p1 <- plot_mc_estimates(
  hwp, plot.type = 1, metrictype = "TgC",
  mc_plot  = hwp$mc_plot
)

p2 <- plot_mc_estimates(
  hwp, plot.type = 2, metrictype = "CO2e",
  mc_total = hwp$mc_PoolsTotalPlot
)

p3 <- plot_mc_estimates(
  hwp, plot.type = 3, metrictype = "TgC",
  mc_iters = hwp$mc_iter_results,
  mc_total = hwp$mc_PoolsTotalPlot  # optional, just for the end-year in title
)


# ---------- simple file saver ----------
save_plot_png <- function(p, file, width = 8, height = 5, dpi = 300) {
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height, dpi = dpi)
}
