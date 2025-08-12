

harv.red.hwp <- data.frame(Year = (hwp.yr + (0:(d.yrs - 1))), name = 0)
colnames(harv.red.hwp)[2] <- eval(ownr.sel)
harv.red.hwp[1, 2] <- harv.hwp[harv.hwp$Year == hwp.yr, ownr.index + 1]

tpr.red.hwp <- tpr.hwp[,c(1, yr.index + 1)]
ppr.red.hwp <- ppr.hwp[,c(1, yr.index + 1)]
eur.red.hwp <- if ("data.table" %in% class(eur.hwp)) eur.hwp[, c(1, yr.index + 1), with = FALSE] else eur.hwp[, c(1, yr.index + 1)]

discard.fates.red.hwp <- if (length(hwp.yr:years[length(years)]) < d.yrs) {   # Takes all discard fate data starting at hwp.yr, adds repeats if runs out of info
  xtra.yrs <- d.yrs - length(hwp.yr:years[length(years)])
  extend.disc.fates <- data.frame(matrix(unlist(rep(discard.fates.hwp[,length(years) + 2], xtra.yrs)), ncol = xtra.yrs))
  names(extend.disc.fates) <- (hwp.yr + (d.yrs - xtra.yrs)):(hwp.yr + (d.yrs - 1))
  cbind(discard.fates.hwp[, c(1:2, (yr.index + 2):(length(years) + 2))], extend.disc.fates)
} else {
  discard.fates.hwp[, c(1:2, (yr.index + 2):(yr.index + 2 + d.yrs - 1))]
} 


# Run the HWP model that is minimized to the selected state/year/ownership for the Sankey display
#  This function can be found in PlotFunctions1.r 
hwp.sankey.output <- HwpModel.Sankey.fcn(harv = harv.red.hwp, 
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
                                         d.yrs = d.yrs)

# --- Year index for import/export vectors from the full model ---
yr_col <- match(hwp.yr, years)

# --- Imports/Exports (use the full-model vectors; DO NOT override from Sankey run) ---
imports_mmtc <- if (!is.null(model.outputs$import_carbon_mt)) {
  as.numeric(model.outputs$import_carbon_mt[yr_col])
} else 0

exports_mmtc <- if (!is.null(model.outputs$export_carbon_mt)) {
  as.numeric(model.outputs$export_carbon_mt[yr_col])
} else 0

# --- Core flows from the minimal Sankey model (harvest-only run) ---
eur_mmtc        <- sum(hwp.sankey.output$eu_matrix[, 1])                 # total primary products this year
eec_mmtc        <- sum(hwp.sankey.output$eec_matrix[, 1])                # fuel + DEC
eu.reduced_mmtc <- sum(hwp.sankey.output$eu.reduced_matrix[, 1])         # products in use after PIU loss
dp.wood_mmtc    <- sum(hwp.sankey.output$dp_matrix[-hwp.sankey.output$eur.pulp, 1])  # wood loss at placement
dp.paper_mmtc   <- sum(hwp.sankey.output$dp_matrix[ hwp.sankey.output$eur.pulp, 1])  # pulp loss at placement

# PIU discard over decay horizon
pu.discard_mmtc <- sum(hwp.sankey.output$pu.discard_matrix[, 2:d.yrs])

# Immediate discard fates from DP + PIU discard (already built inside Sankey run)
landfill.input_mmtc <- sum(hwp.sankey.output$landfill.input_matrix[, 1:d.yrs])
dumps.input_mmtc    <- sum(hwp.sankey.output$dumps.input_matrix[,    1:d.yrs])
compost.input_mmtc  <- sum(hwp.sankey.output$compost.input_matrix[,  1:d.yrs])
bwoec.input_mmtc    <- sum(hwp.sankey.output$bwoec.input_matrix[,    1:d.yrs])  # burned w/o EC
recov.input_mmtc    <- sum(hwp.sankey.output$recov.input_matrix[,    1:d.yrs])
dec.input_mmtc      <- sum(hwp.sankey.output$dec.input_matrix[,      1:d.yrs])  # DEC (with EC)

# SWDS emissions over horizon + non-decaying LF stock
dumps.discard_mmtc     <- sum(hwp.sankey.output$dumps.discard_matrix[,     1:d.yrs])
landfill.discard_mmtc  <- sum(hwp.sankey.output$landfill.discard_matrix[,  1:d.yrs])

lf.fixed_mmtc       <- sum(hwp.sankey.output$lf.fixed_matrix[, 1:d.yrs])   # permanent LF carbon
lf.available_mmtc   <- sum(hwp.sankey.output$landfill_matrix[, d.yrs])     # LF stock still decaying at end
dumps_mmtc          <- sum(hwp.sankey.output$dumps_matrix[,     d.yrs])    # dumps stock at end
swds_mmtc           <- lf.fixed_mmtc + lf.available_mmtc + dumps_mmtc

recov_mmtc          <- sum(hwp.sankey.output$recov_matrix[, d.yrs])        # stock of recovered in-use at end
recov.discard_mmtc  <- sum(hwp.sankey.output$recov.discard_matrix[, 1:d.yrs])

# --- Optional: scale primary outflows so that Primary out = Harvest + Imports - Exports ---
# This keeps mass balance tidy if your minimal run did not already remove exports internally.
primary_out_raw <- eec_mmtc + eu.reduced_mmtc + dp.wood_mmtc + dp.paper_mmtc + exports_mmtc
primary_target  <- eur_mmtc + imports_mmtc    # what we want Primary node to represent

scale_factor <- if (primary_out_raw > 0) (primary_target / primary_out_raw) else 1
# Apply scaling only to the non-export outflows
eec_mmtc        <- eec_mmtc        * scale_factor
eu.reduced_mmtc <- eu.reduced_mmtc * scale_factor
dp.wood_mmtc    <- dp.wood_mmtc    * scale_factor
dp.paper_mmtc   <- dp.paper_mmtc   * scale_factor
# Exports stays as-is

# PIU
# PIU to Discard

pu.discard_mmtc <- sum(hwp.sankey.output$pu.discard_matrix[, 2:d.yrs])
# PIU to PIU
pu_mmtc <- eu.reduced_mmtc - pu.discard_mmtc   # Don't think I need this metric


# Discard
# Discard to SWDS
landfill.input_mmtc <- sum(hwp.sankey.output$landfill.input_matrix[, 1:d.yrs])
dumps.input_mmtc <- sum(hwp.sankey.output$dumps.input_matrix[, 1:d.yrs])
swds.input_mmtc <- landfill.input_mmtc + dumps.input_mmtc
# Discard to Compost
compost.input_mmtc  <- sum(hwp.sankey.output$compost.input_matrix[, 1:d.yrs])
# Discard to BWOEC
bwoec.input_mmtc  <- sum(hwp.sankey.output$bwoec.input_matrix[, 1:d.yrs])
# Discard to Recovered
recov.input_mmtc <- sum(hwp.sankey.output$recov.input_matrix[, 1:d.yrs])
# Discard to DEC (Discard Energy Capture, previously BWEC or Burned with Energy Capture)
dec.input_mmtc <- sum(hwp.sankey.output$dec.input_matrix[, 1:d.yrs])

# SWDS
#swds_mmtc  <- sum(swdsCtotal_matrix[, 7, yr.index])
# SWDS to EWOEC
dumps.discard_mmtc  <- sum(hwp.sankey.output$dumps.discard_matrix[, 1:d.yrs])
landfill.discard_mmtc  <- sum(hwp.sankey.output$landfill.discard_matrix[, 1:d.yrs])
swds.discard_mmtc <- dumps.discard_mmtc + landfill.discard_mmtc
# SWDS to SWDS
lf.fixed_mmtc <- sum(hwp.sankey.output$lf.fixed_matrix[, 1:d.yrs])
lf.available_mmtc <- sum(hwp.sankey.output$landfill_matrix[, d.yrs])
#dumps_matrix <- hwp.sankey.output$dumps_matrix
#dumps_matrix[, 2:3] <- hwp.sankey.output$dumps_matrix[, 2:3] - hwp.sankey.output$dumps_matrix[, 1:2]
#dumps_mmtc <- sum(dumps_matrix[, 1:3])
dumps_mmtc <- sum(hwp.sankey.output$dumps_matrix[, d.yrs])
swds_mmtc <- sum(lf.fixed_mmtc, lf.available_mmtc, dumps_mmtc)


# 1952 discards check: 
#dumps_mmtc + dumps.discard_mmtc + landfill.discard_mmtc + lf.fixed_mmtc + lf.available_mmtc + 
#  bwoec.input_mmtc + compost.input_mmtc + recov_mmtc + recov.discard_mmtc


# Compost
# Compost to EWOEC
#compost.input_mmtc  # See above. Already created

# BWOEC
# BWOEC to EWOEC
#bwoec.input_mmtc  # See above. Already created


# Recovered
recov_mmtc <- sum(hwp.sankey.output$recov_matrix[, d.yrs])
# Recovered to EWOEC
recov.discard_mmtc <- sum(hwp.sankey.output$recov.discard_matrix[, 1:d.yrs])

# EWOEC
ewoec_mmtc <- sum(swds.discard_mmtc, bwoec.input_mmtc, compost.input_mmtc, recov.discard_mmtc)
# Correct check = landfill/dump/recov yrs 2 & 3, bwoec/compost yrs 1 & 2

# 1) Define the 16 Sankey nodes in order
nodes <- data.frame(name = c(
  "Imports",
  "Primary Products = Total Harvest",
  "Emitted with Energy Capture",
  "Products in Use",
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
))

# 2) Create a name→index lookup
idx <- setNames(seq_len(nrow(nodes)), nodes$name)

# 3) Initialize an empty 16×16 matrix
mat.mmtc <- matrix(0, nrow = 16, ncol = 16)

# 4) Fill in the flows by name, one link at a time

# Imports → Primary
mat.mmtc[idx["Imports"], idx["Primary Products = Total Harvest"]] <- imports_mmtc

# Primary → downstream
mat.mmtc[idx["Primary Products = Total Harvest"], idx["Emitted with Energy Capture"]]            <- eec_mmtc
mat.mmtc[idx["Primary Products = Total Harvest"], idx["Products in Use"]]                        <- eu.reduced_mmtc
mat.mmtc[idx["Primary Products = Total Harvest"], idx["Loss When Wood Placed Into End Uses"]]    <- dp.wood_mmtc
mat.mmtc[idx["Primary Products = Total Harvest"], idx["Loss When Pulp Placed Into End Uses"]]    <- dp.paper_mmtc
mat.mmtc[idx["Primary Products = Total Harvest"], idx["Exports"]]                                <- exports_mmtc

# PIU / placement losses → Discard
mat.mmtc[idx["Products in Use"],                      idx["Discard"]] <- pu.discard_mmtc
mat.mmtc[idx["Loss When Wood Placed Into End Uses"],  idx["Discard"]] <- dp.wood_mmtc
mat.mmtc[idx["Loss When Pulp Placed Into End Uses"],  idx["Discard"]] <- dp.paper_mmtc

# Discard → immediate fates
mat.mmtc[idx["Discard"], idx["Dumps"]]                 <- dumps.input_mmtc
mat.mmtc[idx["Discard"], idx["Landfill, Permanent"]]   <- lf.fixed_mmtc
mat.mmtc[idx["Discard"], idx["Landfill, Decomposing"]] <- (landfill.input_mmtc - lf.fixed_mmtc)
mat.mmtc[idx["Discard"], idx["Compost"]]               <- compost.input_mmtc
mat.mmtc[idx["Discard"], idx["Burned"]]                <- bwoec.input_mmtc            # no EC
mat.mmtc[idx["Discard"], idx["Recovered"]]             <- recov.input_mmtc
mat.mmtc[idx["Discard"], idx["Discard Energy Capture"]]<- dec.input_mmtc              # with EC

# Disposal flows → emissions
mat.mmtc[idx["Dumps"],                 idx["Emitted without Energy Capture"]] <- dumps.discard_mmtc
mat.mmtc[idx["Landfill, Decomposing"], idx["Emitted without Energy Capture"]] <- landfill.discard_mmtc
mat.mmtc[idx["Compost"],               idx["Emitted without Energy Capture"]] <- compost.input_mmtc
mat.mmtc[idx["Burned"],                idx["Emitted without Energy Capture"]] <- bwoec.input_mmtc
mat.mmtc[idx["Recovered"],             idx["Emitted without Energy Capture"]] <- recov.discard_mmtc
mat.mmtc[idx["Discard Energy Capture"],idx["Emitted with Energy Capture"]]    <- dec.input_mmtc              <- dec.input_mmtc

# 5) Don’t prune any real node here – keep all 16 in nodes2
nodes2 <- nodes

# Now you can continue with your link‐building:
colnames(mat.mmtc) <- rownames(mat.mmtc) <- nodes2$name
links <- mat.mmtc %>%
  as.data.frame() %>%
  rownames_to_column("source") %>%
  pivot_longer(-source, names_to="target", values_to="value") %>%
  filter(value>0) %>%
  mutate(
    IDsource = match(source, nodes2$name)-1,
    IDtarget = match(target, nodes2$name)-1
  )

#if (input$metrictype == "2") {    # Changing MMT C / Tg C values to CO2e if Tg CO2e selected
#  mat.mmtc <- mat.mmtc * 44/12   
#}


links <- as.data.frame(links)


subset(links, source=="Imports" & target=="Primary Products = Total Harvest")
subset(links, source=="Primary Products = Total Harvest" & target=="Exports")













