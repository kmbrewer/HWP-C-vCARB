# Sankey_Code.r

# --- Build reduced inputs for the Sankey horizon ---
# --- Domestic harvest only (exclude Imports/Exports) for the Sankey mini-run ---
yr_row <- which(harv.hwp$Year == hwp.yr)

owner_cols <- setdiff(names(harv.hwp), c("Year", "Total", "Imports", "Exports"))
if (length(owner_cols)) {
  domestic_mbf <- sum(as.numeric(harv.hwp[yr_row, owner_cols, drop = FALSE]), na.rm = TRUE)
} else {
  total_mbf   <- as.numeric(harv.hwp[yr_row, "Total"])
  imports_mbf <- if ("Imports" %in% names(harv.hwp)) as.numeric(harv.hwp[yr_row, "Imports"]) else 0
  exports_mbf <- if ("Exports" %in% names(harv.hwp)) as.numeric(harv.hwp[yr_row, "Exports"]) else 0
  domestic_mbf <- total_mbf - imports_mbf - exports_mbf
  domestic_mbf <- max(domestic_mbf, 0)
}

# Keep the column name as your selection (usually "Total") so downstream joins don't break
colname <- ownr.sel
harv.red.hwp <- data.frame(Year = (hwp.yr + (0:(d.yrs - 1))), tmp = 0)
names(harv.red.hwp)[2] <- colname
harv.red.hwp[1, colname] <- domestic_mbf

tpr.red.hwp <- tpr.hwp[, c(1, yr.index + 1)]
ppr.red.hwp <- ppr.hwp[, c(1, yr.index + 1)]
eur.red.hwp <- if ("data.table" %in% class(eur.hwp)) eur.hwp[, c(1, yr.index + 1), with = FALSE] else eur.hwp[, c(1, yr.index + 1)]

discard.fates.red.hwp <- if (length(hwp.yr:years[length(years)]) < d.yrs) {
  xtra.yrs <- d.yrs - length(hwp.yr:years[length(years)])
  extend.disc.fates <- data.frame(matrix(unlist(rep(discard.fates.hwp[, length(years) + 2], xtra.yrs)), ncol = xtra.yrs))
  names(extend.disc.fates) <- (hwp.yr + (d.yrs - xtra.yrs)):(hwp.yr + (d.yrs - 1))
  cbind(discard.fates.hwp[, c(1:2, (yr.index + 2):(length(years) + 2))], extend.disc.fates)
} else {
  discard.fates.hwp[, c(1:2, (yr.index + 2):(yr.index + 2 + d.yrs - 1))]
}

# --- Run minimized HWP for Sankey horizon ---
hwp.sankey.output <- HwpModel.Sankey.fcn(
  harv = harv.red.hwp,
  bfcf = bfcf.hwp,
  tpr = tpr.red.hwp,
  ppr = ppr.red.hwp,
  ratio_cat = ratio_cat.hwp,
  ccf_conversion = ccf_conversion.hwp,
  eur = eur.red.hwp,
  eu_half.lives = eu_half.lives.hwp,
  discard.fates = discard.fates.red.hwp,
  discard.hl = discard.hl.hwp,
  hwp.yr = hwp.yr,
  ownership.names = ownership.names,
  N.EUR = N.EUR,
  PIU.WOOD.LOSS = PIU.WOOD.LOSS,
  PIU.PAPER.LOSS = PIU.PAPER.LOSS,
  years = years,
  yr.index = yr.index,
  ownr.index = ownr.index,
  d.yrs = d.yrs
)

# --- Year index for imports/exports from the FULL model ---
yr_col <- match(hwp.yr, years)

# Imports (prefer vector, fallback from eu_array)
imports_mmtc <- 0
if (!is.null(model.outputs$import_carbon_mt) && !is.na(yr_col)) {
  imports_mmtc <- as.numeric(model.outputs$import_carbon_mt[yr_col])
}
if ((is.na(imports_mmtc) || imports_mmtc == 0) && "Imports" %in% dimnames(model.outputs$eu_array)[[2]]) {
  imports_mmtc <- sum(model.outputs$eu_array[, "Imports", yr_col], na.rm = TRUE)
}
imports_mmtc[is.na(imports_mmtc)] <- 0

# Exports
exports_mmtc <- 0
if (!is.null(model.outputs$export_carbon_mt) && !is.na(yr_col)) {
  exports_mmtc <- as.numeric(model.outputs$export_carbon_mt[yr_col])
}
exports_mmtc[is.na(exports_mmtc)] <- 0

# --- Core flows from the minimized run (harvest-only this year) ---
eur_mmtc        <- sum(hwp.sankey.output$eu_matrix[, 1])                  # primary (harvest) this year
eec_mmtc        <- sum(hwp.sankey.output$fuel_matrix[, 1])                # fuel only; DEC added below
eu.reduced_mmtc <- sum(hwp.sankey.output$eu.reduced_matrix[, 1])          # in use after PIU loss
dp.wood_mmtc    <- sum(hwp.sankey.output$dp_matrix[-hwp.sankey.output$eur.pulp, 1])  # wood placement loss
dp.paper_mmtc   <- sum(hwp.sankey.output$dp_matrix[ hwp.sankey.output$eur.pulp,  1]) # pulp placement loss

pu.discard_mmtc <- if (d.yrs >= 2) sum(hwp.sankey.output$pu.discard_matrix[, 2:d.yrs]) else 0

# Remaining PIU stock at end of horizon (correct: take the modeled stock at t = d.yrs)
pu_remaining_mmtc <- sum(hwp.sankey.output$pu_matrix[, d.yrs])

# Immediate discard fates (already include PIU discards)
landfill.input_mmtc <- sum(hwp.sankey.output$landfill.input_matrix[, 1:d.yrs])
dumps.input_mmtc    <- sum(hwp.sankey.output$dumps.input_matrix[,    1:d.yrs])
compost.input_mmtc  <- sum(hwp.sankey.output$compost.input_matrix[,  1:d.yrs])
bwoec.input_mmtc    <- sum(hwp.sankey.output$bwoec.input_matrix[,    1:d.yrs])  # burned w/o EC
recov.input_mmtc    <- sum(hwp.sankey.output$recov.input_matrix[,    1:d.yrs])
dec.input_mmtc      <- sum(hwp.sankey.output$dec.input_matrix[,      1:d.yrs])  # with EC

# SWDS emissions over horizon + non-decaying LF stock
dumps.discard_mmtc     <- sum(hwp.sankey.output$dumps.discard_matrix[,     1:d.yrs])
landfill.discard_mmtc  <- sum(hwp.sankey.output$landfill.discard_matrix[,  1:d.yrs])

lf.fixed_mmtc       <- sum(hwp.sankey.output$lf.fixed_matrix[, 1:d.yrs])   # permanent LF carbon
lf.available_mmtc   <- sum(hwp.sankey.output$landfill_matrix[, d.yrs])     # LF stock still decaying at end
dumps_mmtc          <- sum(hwp.sankey.output$dumps_matrix[,     d.yrs])    # dumps stock at end
swds_mmtc           <- lf.fixed_mmtc + lf.available_mmtc + dumps_mmtc

# Recovered pool (stock at end + its future discards counted above)
recov_mmtc          <- sum(hwp.sankey.output$recov_matrix[, d.yrs])
recov.discard_mmtc  <- sum(hwp.sankey.output$recov.discard_matrix[, 1:d.yrs])

# --- Mass-balance scaling at the Primary node ---
# k = (Harvest + Imports − Exports) / Harvest
scale_factor <- if (eur_mmtc > 0) (eur_mmtc + imports_mmtc - exports_mmtc) / eur_mmtc else 1
scale_factor <- max(scale_factor, 0)  # avoid negative

primary_target     <- eur_mmtc + imports_mmtc
primary_out_scaled <- eec_mmtc + eu.reduced_mmtc + dp.wood_mmtc + dp.paper_mmtc + exports_mmtc
message(sprintf("[Sankey %d] Primary net (should be ~0): %.6f", hwp.yr, primary_target - primary_out_scaled))

message(sprintf("[Sankey %d] InUse stock (PU) @end=%.3f, Recovered stock @end=%.3f, Discard->DEC=%.3f",
                hwp.yr, pu_remaining_mmtc, recov_mmtc, dec.input_mmtc))

eec_mmtc        <- eec_mmtc        * scale_factor
eu.reduced_mmtc <- eu.reduced_mmtc * scale_factor
dp.wood_mmtc    <- dp.wood_mmtc    * scale_factor
dp.paper_mmtc   <- dp.paper_mmtc   * scale_factor

pu.discard_mmtc     <- pu.discard_mmtc     * scale_factor
pu_remaining_mmtc   <- pu_remaining_mmtc   * scale_factor

landfill.input_mmtc <- landfill.input_mmtc * scale_factor
dumps.input_mmtc    <- dumps.input_mmtc    * scale_factor
compost.input_mmtc  <- compost.input_mmtc  * scale_factor
bwoec.input_mmtc    <- bwoec.input_mmtc    * scale_factor
recov.input_mmtc    <- recov.input_mmtc    * scale_factor
dec.input_mmtc      <- dec.input_mmtc      * scale_factor

dumps.discard_mmtc    <- dumps.discard_mmtc    * scale_factor
landfill.discard_mmtc <- landfill.discard_mmtc * scale_factor
recov.discard_mmtc    <- recov.discard_mmtc    * scale_factor

lf.fixed_mmtc       <- lf.fixed_mmtc       * scale_factor
lf.available_mmtc   <- lf.available_mmtc   * scale_factor
dumps_mmtc          <- dumps_mmtc          * scale_factor
swds_mmtc           <- swds_mmtc           * scale_factor
recov_mmtc          <- recov_mmtc          * scale_factor

# Sanity (≈ 0 with FP noise)
primary_target     <- eur_mmtc + imports_mmtc
primary_out_scaled <- eec_mmtc + eu.reduced_mmtc + dp.wood_mmtc + dp.paper_mmtc + exports_mmtc
message(sprintf("Primary net (should be ~0): %.6f", primary_target - primary_out_scaled))

# --- Define Sankey nodes (includes an explicit stock node) ---
nodes <- data.frame(name = c(
  "Harvest",
  "Imports",
  "Primary Products",
  "Emitted with Energy Capture",
  "Products in Use",
  "In-Use Stock (End of Horizon)",      # <-- added explicit stock node
  "Loss When Wood Placed Into End Uses",
  "Loss When Pulp Placed Into End Uses",
  "Discard",
  "Dumps",
  "Landfill, Permanent",
  "Landfill, Decomposing",
  "Compost",
  "Burned",
  "Recovered",
  "Emitted without Energy Capture",
  "Discard Energy Capture",
  "Exports"
), stringsAsFactors = FALSE)

idx <- setNames(seq_len(nrow(nodes)), nodes$name)
n_nodes <- nrow(nodes)
mat.mmtc <- matrix(0, nrow = n_nodes, ncol = n_nodes)
rownames(mat.mmtc) <- colnames(mat.mmtc) <- nodes$name

# Inflows to Primary
mat.mmtc[idx["Harvest"],  idx["Primary Products"]] <- eur_mmtc
mat.mmtc[idx["Imports"],  idx["Primary Products"]] <- imports_mmtc

# Primary → downstream
mat.mmtc[idx["Primary Products"], idx["Emitted with Energy Capture"]]           <- eec_mmtc
mat.mmtc[idx["Primary Products"], idx["Products in Use"]]                       <- eu.reduced_mmtc
mat.mmtc[idx["Primary Products"], idx["Loss When Wood Placed Into End Uses"]]   <- dp.wood_mmtc
mat.mmtc[idx["Primary Products"], idx["Loss When Pulp Placed Into End Uses"]]   <- dp.paper_mmtc
mat.mmtc[idx["Primary Products"], idx["Exports"]]                               <- exports_mmtc

# PIU stock and discards
mat.mmtc[idx["Products in Use"], idx["In-Use Stock (End of Horizon)"]] <- pu_remaining_mmtc
mat.mmtc[idx["Products in Use"], idx["Discard"]]                       <- pu.discard_mmtc

# Placement losses → Discard
mat.mmtc[idx["Loss When Wood Placed Into End Uses"],  idx["Discard"]] <- dp.wood_mmtc
mat.mmtc[idx["Loss When Pulp Placed Into End Uses"],  idx["Discard"]] <- dp.paper_mmtc

# Discard → immediate fates
mat.mmtc[idx["Discard"], idx["Dumps"]]                 <- dumps.input_mmtc
mat.mmtc[idx["Discard"], idx["Landfill, Permanent"]]   <- lf.fixed_mmtc
# guard against tiny negatives from rounding
lf_decomp_flow <- max(0, landfill.input_mmtc - lf.fixed_mmtc)
mat.mmtc[idx["Discard"], idx["Landfill, Decomposing"]] <- lf_decomp_flow
mat.mmtc[idx["Discard"], idx["Compost"]]               <- compost.input_mmtc
mat.mmtc[idx["Discard"], idx["Burned"]]                <- bwoec.input_mmtc
mat.mmtc[idx["Discard"], idx["Recovered"]]             <- recov.input_mmtc
mat.mmtc[idx["Discard"], idx["Discard Energy Capture"]] <- dec.input_mmtc

# Recovered → stock (recovered material remaining at end)
mat.mmtc[idx["Recovered"], idx["In-Use Stock (End of Horizon)"]] <- recov_mmtc

# Disposal → emissions
mat.mmtc[idx["Dumps"],                 idx["Emitted without Energy Capture"]] <- dumps.discard_mmtc
mat.mmtc[idx["Landfill, Decomposing"], idx["Emitted without Energy Capture"]] <- landfill.discard_mmtc
mat.mmtc[idx["Compost"],               idx["Emitted without Energy Capture"]] <- compost.input_mmtc
mat.mmtc[idx["Burned"],                idx["Emitted without Energy Capture"]] <- bwoec.input_mmtc
mat.mmtc[idx["Recovered"],             idx["Emitted without Energy Capture"]] <- recov.discard_mmtc
mat.mmtc[idx["Discard Energy Capture"],idx["Emitted with Energy Capture"]]    <- dec.input_mmtc

# ---- Hide specific nodes (optional) ----
drop_names <- c("In-Use Stock (End of Horizon)")

# Drop nodes with zero flow AND any explicitly hidden ones
used <- ((rowSums(mat.mmtc) > 0) | (colSums(mat.mmtc) > 0)) &
  !(rownames(mat.mmtc) %in% drop_names)

nodes2  <- nodes[used, , drop = FALSE]
mat.mmtc <- mat.mmtc[nodes2$name, nodes2$name, drop = FALSE]

# Build links
links <- mat.mmtc |>
  as.data.frame() |>
  tibble::rownames_to_column("source") |>
  tidyr::pivot_longer(-source, names_to = "target", values_to = "value") |>
  dplyr::filter(value > 0) |>
  dplyr::mutate(
    IDsource = match(source, nodes2$name) - 1L,
    IDtarget = match(target, nodes2$name) - 1L
  )

# Quick checks you can print once

# message(sprintf("Primary inflow %d: Harvest=%.3f, Imports=%.3f, Import share=%.1f%%",hwp.yr, eur_mmtc, imports_mmtc, 100 * imports_mmtc / pmax(1e-12, eur_mmtc + imports_mmtc)))
# message(sprintf("Primary outflow raw vs target: %.3f vs %.3f (scale=%.4f)",primary_out_raw, primary_target, scale_factor))

# Optional quick checks (comment out if noisy):
# message(sprintf("Primary inflow: %0.3f  |  outflow: %0.3f",
#                 eur_mmtc + imports_mmtc,
#                 eec_mmtc + eu.reduced_mmtc + dp.wood_mmtc + dp.paper_mmtc + exports_mmtc))
# print(dim(mat.mmtc)); print(head(links))
#subset(links, source=="Imports" & target=="Primary Products")
#subset(links, source=="Primary Products" & target=="Exports")











