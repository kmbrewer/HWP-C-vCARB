# HWP_Plots_Standalone.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridisLite)
library(abind)


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
# 1) Annual Timber Harvest  (AnnTimHarv)
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

# Example call
p1 <- plot_ann_timber_harvest(hwp,
                              metric="MMTC", summary="annual", mode="ownership_total",
                              ownership_start_year=1952, trade_start_year=2001)
print(p1); save_plot_png(p1, "Plot_AnnTimHarv.png")



# =========================================================
# 2) Annual Net Change in Carbon Storage (Production/Simple Decay)
#    approach: "production" or "simple_decay"
#    metrictype: "TgC" | "CO2e"
# =========================================================
plot_annual_net_change <- function(hwp,
                                   approach = c("production","simple_decay"),
                                   metrictype = c("TgC","CO2e"),
                                   include_net_line = TRUE) {
  approach <- match.arg(approach)
  metrictype <- match.arg(metrictype)
  
  years <- .get_years(hwp)
  id_total <- .idx_total(hwp)
  
  # totals (Tg C)
  pu <- apply(hwp$pu.final_array[, id_total, ], 2, sum)/1e6
  sw <- apply(hwp$swdsCtotal_array[, id_total, ], 2, sum)/1e6
  eec <- apply(hwp$eec_array[, id_total, ], 2, sum)/1e6    # annual
  ewo <- apply(hwp$ewoec_array[, id_total, ], 2, sum)/1e6  # annual
  
  # annual changes (lag diff of cumulative pools)
  pu_ch   <- diff(pu)
  sw_ch   <- diff(sw)
  eec_ch  <- eec[-1]             # align to year t (your original code negates for stacked display)
  ewoec_ch<- ewo[-1]
  
  df <- data.frame(
    Year = years[-1],
    SWDSchange = sw_ch,
    PUchange   = pu_ch,
    EECchange  = -eec_ch,      # negative for plotting (emission downward)
    EWOECchange= -ewoec_ch     # negative for plotting
  )
  df$Net <- df$SWDSchange + df$PUchange
  
  # Harvest = sum of all annual flows (as in your Table 5 logic)
  df$Harvest <- df$PUchange + df$SWDSchange - df$EECchange - df$EWOECchange
  
  # metric conversion
  if (metrictype == "CO2e") {
    df[,-1] <- df[,-1] * (44/12)
  }
  ylab <- .lab_co2e(if (metrictype=="CO2e") "CO2e" else "TgC")
  
  if (approach == "production") {
    # show SWDSchange + PUchange stacked; optional Net line
    long <- df |>
      select(Year, SWDSchange, PUchange, Net) |>
      pivot_longer(-Year, names_to = "series", values_to = "val")
    bar <- long |> filter(series %in% c("SWDSchange","PUchange"))
    ax <- .axis_pretty(c(bar$val, if (include_net_line) long$val[long$series=="Net"]), positive_only = FALSE)
    
    p <- ggplot(bar, aes(Year, val, fill = series)) +
      geom_col() + geom_hline(yintercept = 0) +
      scale_fill_manual(values = c(SWDSchange="#B42E8D", PUchange="#7801A8"), name = NULL,
                        labels = c("Solid Waste Disposal Sites","Products in Use")) +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by), limits = c(ax$min, ax$max), expand = c(0,0)) +
      labs(x=NULL, y=ylab, title="IPCC production approach — stock change") +
      theme_bw(base_size = 14)
    if (include_net_line) {
      p <- p + geom_line(data = df, aes(Year, Net, color="Net"), linewidth=1.2) +
        scale_color_manual(values=c(Net="#3CB371"), name=NULL)
    }
    return(p)
  } else {
    # simple decay: stack Harvest (positive), EWOECchange (neg), EECchange (neg); optional Net line
    bar <- df |>
      select(Year, Harvest, EWOECchange, EECchange, Net) |>
      pivot_longer(-Year, names_to="series", values_to="val") |>
      filter(series %in% c("Harvest","EWOECchange","EECchange"))
    ax <- .axis_pretty(c(bar$val, if (include_net_line) df$Net), positive_only = FALSE)
    
    p <- ggplot(bar, aes(Year, val, fill=series)) +
      geom_col() + geom_hline(yintercept=0) +
      scale_fill_manual(values=c(EWOECchange="#F99A3E", EECchange="#F8DF25", Harvest="#00CED1"),
                        name=NULL,
                        labels=c("Emitted without Energy Capture",
                                 "Emitted with Energy Capture",
                                 "Annual Harvest")) +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by), limits = c(ax$min, ax$max), expand = c(0,0)) +
      labs(x=NULL, y=ylab, title="IPCC simple decay approach — net change") +
      theme_bw(base_size = 14)
    if (include_net_line) {
      p <- p + geom_line(data = df, aes(Year, Net, color="Net"), linewidth=1.2) +
        scale_color_manual(values=c(Net="#3CB371"), name=NULL)
    }
    return(p)
  }
}

# 2) Annual Net Change (Production approach) with Net line
p2 <- plot_annual_net_change(hwp, approach="production", metrictype="TgC", include_net_line=TRUE)
print(p2); save_plot_png(p2, "Plot_AnNetChCStor_Production.png")


# =========================================================
# 3) Harvest by Functional Lifespan
#    type: "absolute" (TgC/CO2e) | "proportion"
# =========================================================
plot_harvest_by_functional_lifespan <- function(hwp,
                                                type = c("absolute","proportion"),
                                                metrictype = c("TgC","CO2e")) {
  type <- match.arg(type)
  metrictype <- match.arg(metrictype)
  
  if (is.null(hwp$eu_half.lives.hwp)) {
    stop("hwp$eu_half.lives.hwp is required for lifespan plotting.")
  }
  years <- .get_years(hwp)
  id_total <- .idx_total(hwp)
  
  sml <- hwp$eu_half.lives.hwp %>%
    mutate(ShortMedLong = ifelse(EU_HalfLife > 0 & EU_HalfLife <= 6, "Short",
                                 ifelse(EU_HalfLife > 6 & EU_HalfLife <= 30, "Medium",
                                        ifelse(EU_HalfLife > 30, "Long", "Fuel"))))
  
  # attach EU flows for Total ownership
  eu_tot <- as.data.frame(hwp$eu_array[, id_total, ])
  colnames(eu_tot) <- years
  dat <- cbind(sml, eu_tot) |>
    pivot_longer(cols = all_of(as.character(years)), names_to="Year", values_to="MMTC") |>
    mutate(Year = as.numeric(Year)) |>
    group_by(ShortMedLong, Year) |>
    summarise(MMTC = sum(MMTC), .groups="drop")
  
  totals <- dat |>
    group_by(Year) |>
    summarise(Total = sum(MMTC), .groups="drop")
  
  out <- dat |>
    left_join(totals, by="Year") |>
    mutate(value_abs = .as_co2e(MMTC/1e6, if (metrictype=="CO2e") "CO2e" else "TgC"),
           value_prop = ifelse(Total>0, MMTC/Total, 0),
           ShortMedLong = factor(ShortMedLong, levels=c("Fuel","Short","Medium","Long")))
  
  if (type == "absolute") {
    ax <- .axis_pretty(out %>% group_by(Year) %>% summarise(Tot=sum(value_abs)) %>% pull(Tot))
    ggplot(out, aes(Year, value_abs, fill = ShortMedLong)) +
      geom_area(alpha=0.6, color="white") +
      scale_fill_viridis_d(end=0,
                           labels=c("Fuel (instant)","Short (1–6 yr)","Medium (7–30 yr)","Long (31+ yr)")) +
      scale_y_continuous(breaks=seq(ax$min, ax$max, by=ax$by), limits=c(ax$min, ax$max), expand=c(0,0)) +
      labs(x="Harvest Year", y=.lab_co2e(if (metrictype=="CO2e") "CO2e" else "TgC"),
           title="Annual allocation of harvested carbon by functional lifespan") +
      theme_bw(base_size = 14)
  } else {
    ggplot(out, aes(Year, value_prop, fill = ShortMedLong)) +
      geom_area(alpha=0.6, color="white") +
      scale_fill_viridis_d(end=0,
                           labels=c("Fuel (instant)","Short (1–6 yr)","Medium (7–30 yr)","Long (31+ yr)")) +
      scale_y_continuous(labels=scales::percent, limits=c(0,1), expand=c(0,0)) +
      labs(x="Harvest Year", y="Share of annual harvest",
           title="Proportional allocation of harvested carbon by functional lifespan") +
      theme_bw(base_size = 14)
  }
}

# 3) Harvest by Functional Lifespan — absolute, CO2e
p3 <- plot_harvest_by_functional_lifespan(hwp, type="absolute", metrictype="CO2e")
print(p3); save_plot_png(p3, "Plot_HarvFuncLS_abs_CO2e.png")


# =========================================================
# 4) Carbon Storage by Ownership
#    pools: "both" | "piu" | "swds"
#    owners: character vector of ownership names (defaults to all non-Total)
# =========================================================
plot_carbon_storage_by_ownership <- function(hwp,
                                             pools = c("both","piu","swds"),
                                             metrictype = c("TgC","CO2e"),
                                             owners = NULL) {
  pools <- match.arg(pools)
  metrictype <- match.arg(metrictype)
  
  years <- .get_years(hwp)
  owns  <- .get_ownerships(hwp)
  owns_no_total <- setdiff(owns, "Total")
  if (is.null(owners)) owners <- owns_no_total
  
  # Build cumulative storage series per ownership (Tg C)
  piu <- sapply(owners, function(o) {
    oi <- which(owns == o)
    apply(hwp$pu.final_array[, oi, ], 2, sum)/1e6
  })
  swd <- sapply(owners, function(o) {
    oi <- which(owns == o)
    apply(hwp$swdsCtotal_array[, oi, ], 2, sum)/1e6
  })
  if (is.null(dim(piu))) piu <- matrix(piu, ncol=1, dimnames=list(NULL, owners))
  if (is.null(dim(swd))) swd <- matrix(swd, ncol=1, dimnames=list(NULL, owners))
  
  df_piu <- as.data.frame(piu); df_piu$Year <- years
  df_swd <- as.data.frame(swd); df_swd$Year <- years
  
  df <- left_join(
    df_piu |> pivot_longer(-Year, names_to="Owner", values_to="PIU"),
    df_swd |> pivot_longer(-Year, names_to="Owner", values_to="SWDS"),
    by = c("Year","Owner")
  ) |>
    mutate(Value = dplyr::case_when(
      pools == "piu"  ~ PIU,
      pools == "swds" ~ SWDS,
      TRUE            ~ PIU + SWDS
    ))
  
  df$Value <- .as_co2e(df$Value, if (metrictype=="CO2e") "CO2e" else "TgC")
  ylab <- .lab_co2e(if (metrictype=="CO2e") "CO2e" else "TgC")
  
  # stacked area by (Owner, pool) or just (Owner)
  if (pools == "both") {
    df_long <- bind_rows(
      df |> mutate(series="Products in Use",   Value = .as_co2e(PIU,  if (metrictype=="CO2e") "CO2e" else "TgC")),
      df |> mutate(series="SWDS",              Value = .as_co2e(SWDS, if (metrictype=="CO2e") "CO2e" else "TgC"))
    ) |>
      select(Year, Owner, series, Value)
    
    ax <- .axis_pretty(df_long %>% group_by(Year) %>% summarise(Tot=sum(Value)) %>% pull(Tot))
    
    ggplot(df_long, aes(Year, Value, fill = interaction(Owner, series, sep=" — "))) +
      geom_area() +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by), limits = c(ax$min, ax$max), expand = c(0,0)) +
      scale_fill_viridis_d(end=0, name = NULL) +
      labs(x="Harvest Year", y=ylab, title="Cumulative carbon stored in products in use and SWDS by ownership") +
      theme_bw(base_size = 14) + theme(legend.position = "bottom")
  } else {
    ax <- .axis_pretty(df %>% group_by(Year) %>% summarise(Tot=sum(Value)) %>% pull(Tot))
    ggplot(df, aes(Year, Value, fill = Owner)) +
      geom_area() +
      scale_y_continuous(breaks = seq(ax$min, ax$max, by = ax$by), limits = c(ax$min, ax$max), expand = c(0,0)) +
      scale_fill_viridis_d(end=0, name = NULL) +
      labs(x="Harvest Year", y=ylab,
           title = paste("Cumulative carbon stored in", if (pools=="piu") "products in use" else "SWDS", "by ownership")) +
      theme_bw(base_size = 14) + theme(legend.position = "bottom")
  }
}

# 4) Carbon Storage by Ownership — both pools, CO2e
p4 <- plot_carbon_storage_by_ownership(hwp, pools="both", metrictype="CO2e")
print(p4); save_plot_png(p4, "Plot_CStorOwn_both_CO2e.png")


# =========================================================
# 5) Monte Carlo Estimates
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
p5 <- plot_mc_estimates(hwp, plot.type=1, metrictype="TgC")
print(p5); save_plot_png(p5, "Plot_MC_facet.png")


# ---------- simple file saver ----------
save_plot_png <- function(p, file, width = 8, height = 5, dpi = 300) {
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height, dpi = dpi)
}
