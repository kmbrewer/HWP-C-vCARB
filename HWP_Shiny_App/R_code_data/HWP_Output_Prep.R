# HWP_Output_Prep.R

###### This script operates a bit like a function.  The purpose is to provide 
#####   model output values that may be used for generating Shiny plots, 
###     downloading tables, or providing to the Monte Carlo.  

eu_ratios <- model.outputs$eu_ratios
eu.reduced_array <- model.outputs$eu.reduced_array
dp.total_array <- model.outputs$dp.total_array
empty.eu_array <- model.outputs$empty.eu_array
first.eu_array <- model.outputs$first.eu_array
harv_cf1 <- model.outputs$harv_cf1
landfill.fx.ratio <- model.outputs$landfill.fx.ratio
landfill.hl <- model.outputs$landfill.hl
dump.hl <- model.outputs$dump.hl
recov.hl <- model.outputs$recov.hl
eur.pulp <- model.outputs$eur.pulp

eu_array <- model.outputs$eu_array
export_idx <- model.outputs$export_idx  # Index of 'Exports' ownership
eec_array <- model.outputs$eec_array
fuel_array <- model.outputs$fuel_array
dec.input_array <- model.outputs$dec.input_array
ewoec_array <- model.outputs$ewoec_array
dumps.discard_array <- model.outputs$dumps.discard_array 
landfill.discard_array <- model.outputs$landfill.discard_array
recov.discard_array <- model.outputs$recov.discard_array
compost.input_array <- model.outputs$compost.input_array
bwoec.input_array <- model.outputs$bwoec.input_array
swdsCtotal_array <- model.outputs$swdsCtotal_array
lf.fixed.cumsum_array <- model.outputs$lf.fixed.cumsum_array
landfill_array <- model.outputs$landfill_array
dumps_array <- model.outputs$dumps_array
pu.final_array <- model.outputs$pu.final_array
pu_array <- model.outputs$pu_array
recov_array <- model.outputs$recov_array

# Get ownership names and years from the arrays we definitely have
ownership.names <- dimnames(eu_array)[[2]]
years_dim <- as.numeric(dimnames(eu_array)[[3]])
n_years <- length(years_dim)

# --- Shape guardrails --------------------------------------------------------
if(is.null(ownership.names) || is.null(years_dim)) {
  stop("eu_array is missing dimnames for ownerships/years.", call. = FALSE)
}

# eu_total should match n_years
eu_total <- apply(eu_array, 3, sum)
assert_len <- function(x, n, nm){ if(length(x)!=n) stop(sprintf("%s length %d != %d years", nm, length(x), n), call. = FALSE) }
assert_len(eu_total, n_years, "eu_total")

# Safe getter: if the model didn’t return the vectors, use zeros of the right length
get_or_zero <- function(x, n) {
  if (!is.null(x) && length(x) == n) as.numeric(x) else rep(0, n)
}

# Prefer the raw vectors computed inside HwpModel.fcn (based on eu_array_raw)
export_carbon_mt <- get_or_zero(model.outputs$export_carbon_mt, n_years)
import_carbon_mt <- get_or_zero(model.outputs$import_carbon_mt, n_years)

# If those were missing AND we have an Exports/Imports ownership in eu_array, try to compute from eu_array.
# NOTE: If your HwpModel.fcn zeros the Exports column in eu_array (recommended), this will remain zeros—and that’s OK,
# because export_carbon_mt above (from model.outputs) is the authoritative source.
if (all(export_carbon_mt == 0) && "Exports" %in% ownership.names) {
  exports_in_eu <- apply(eu_array[, ownership.names == "Exports", , drop = FALSE], 3, sum)
  if (sum(exports_in_eu) > 0) export_carbon_mt <- as.numeric(exports_in_eu)
}
if (all(import_carbon_mt == 0) && "Imports" %in% ownership.names) {
  imports_in_eu <- apply(eu_array[, ownership.names == "Imports", , drop = FALSE], 3, sum)
  if (sum(imports_in_eu) > 0) import_carbon_mt <- as.numeric(imports_in_eu)
}

# Build a denominator that avoids double-counting exports:
# total EUR = (everything in eu_array) - (exports already in eu_array, if any) + (authoritative exports vector)
eu_total <- apply(eu_array, 3, sum)

exports_in_eu <- if ("Exports" %in% ownership.names)
  apply(eu_array[, ownership.names == "Exports", , drop = FALSE], 3, sum) else rep(0, n_years)

# total EUR = everything currently in eu_array (which may still include an Exports slice from legacy data),
# minus any Exports already in eu_array, plus the authoritative export_carbon_mt vector.
total_carbon_by_year <- eu_total - exports_in_eu + export_carbon_mt

# after computing export_carbon_mt/import_carbon_mt and total_carbon_by_year
stopifnot(length(export_carbon_mt) == n_years, length(import_carbon_mt) == n_years)
attr(export_carbon_mt, "locked") <- TRUE
attr(import_carbon_mt, "locked") <- TRUE

# -------- Tables --------
# T6.0 – Annual Exports (MTC)
t6 <- data.frame(
  Year = years_dim,
  Export_C_MTC = export_carbon_mt
)

# T6.1 – Cumulative Exports
t6.1 <- data.frame(
  Year = years_dim,
  Export_Cumulative_MTC = cumsum(export_carbon_mt)
)

# T6.2 – Exports as % of total EUR
t6.2 <- data.frame(
  Year = years_dim,
  Export_Percent_of_Total_EUR = ifelse(total_carbon_by_year > 0,
                                       round(100 * export_carbon_mt / total_carbon_by_year, 2),
                                       0)
)

# T7.0 – Annual Imports (MTC)
t7 <- data.frame(
  Year = years_dim,
  Import_C_MTC = import_carbon_mt
)

# T7.1 – Cumulative Imports
t7.1 <- data.frame(
  Year = years_dim,
  Import_Cumulative_MTC = cumsum(import_carbon_mt)
)

# T7.2 – Imports as % of total EUR
t7.2 <- data.frame(
  Year = years_dim,
  Import_Percent_of_Total_EUR = ifelse(total_carbon_by_year > 0,
                                       round(100 * import_carbon_mt / total_carbon_by_year, 2),
                                       0)
)