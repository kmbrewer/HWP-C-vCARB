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


# ============================
# T6.0 - Annual Carbon Exported (pre-subtraction)
# ============================

if ("Exports" %in% ownership.names) {
  export_idx <- which(ownership.names == "Exports")
  
  # Sum all EndUseID carbon values per year for Exports
  export_carbon_mt <- apply(eu_array[, export_idx, ], 2, sum)
  
  t6 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Export_C_MTC = as.numeric(export_carbon_mt)
  )
} else {
  t6 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Export_C_MTC = rep(0, length(dimnames(eu_array)[[3]]))
  )
}

# ============================
# T6.1 - Cumulative Carbon Exported (MTC)
# ============================

if ("Exports" %in% ownership.names) {
  export_idx <- which(ownership.names == "Exports")
  export_carbon_mt <- apply(eu_array[, export_idx, ], 2, sum)
  export_cumulative <- cumsum(export_carbon_mt)
  
  t6.1 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Export_Cumulative_MTC = as.numeric(export_cumulative)
  )
} else {
  t6.1 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Export_Cumulative_MTC = rep(0, length(dimnames(eu_array)[[3]]))
  )
}


# ============================
# T6.2 - Percent of Total EUR Assigned to Exports
# ============================

total_carbon_by_year <- apply(eu_array, 3, sum)  # Total across all ownerships
export_percent <- if ("Exports" %in% ownership.names) {
  export_carbon_mt <- apply(eu_array[, export_idx, ], 2, sum)
  percent <- 100 * (export_carbon_mt / total_carbon_by_year)
  replace(percent, is.nan(percent), 0)  # Handle 0/0
} else {
  rep(0, length(total_carbon_by_year))
}

t6.2 <- data.frame(
  Year = dimnames(eu_array)[[3]],
  Export_Percent_of_Total_EUR = round(export_percent, 2)
)


# ============================
# T7.0 - Annual Carbon Imported (MTC)
# ============================

if ("Imports" %in% ownership.names) {
  import_idx <- which(ownership.names == "Imports")
  import_carbon_mt <- apply(eu_array[, import_idx, ], 2, sum)
  
  t7 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Import_C_MTC = as.numeric(import_carbon_mt)
  )
} else {
  t7 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Import_C_MTC = rep(0, length(dimnames(eu_array)[[3]]))
  )
}


# ============================
# T7.1 - Cumulative Carbon Imported (MTC)
# ============================

if ("Imports" %in% ownership.names) {
  import_cumulative <- cumsum(import_carbon_mt)
  
  t7.1 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Import_Cumulative_MTC = as.numeric(import_cumulative)
  )
} else {
  t7.1 <- data.frame(
    Year = dimnames(eu_array)[[3]],
    Import_Cumulative_MTC = rep(0, length(dimnames(eu_array)[[3]]))
  )
}


# ============================
# T7.2 - Percent of Total EUR Assigned to Imports
# ============================

import_percent <- if ("Imports" %in% ownership.names) {
  percent <- 100 * (import_carbon_mt / total_carbon_by_year)
  replace(percent, is.nan(percent), 0)  # Handle 0/0
} else {
  rep(0, length(total_carbon_by_year))
}

t7.2 <- data.frame(
  Year = dimnames(eu_array)[[3]],
  Import_Percent_of_Total_EUR = round(import_percent, 2)
)
