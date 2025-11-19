# ---- Packages ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(scales)
library(grid)  # for unit()

# ---- Theme ----
common_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 24),
    axis.text  = element_text(size = 28),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "horizontal",         # <-- one-row legends
    legend.title = element_blank(),
    legend.text  = element_text(size = 22),
    legend.key.size = unit(22, "pt"),
    legend.key.width = unit(25, "pt"),
    legend.box.margin = margin(8, 10, 0, 10),
    legend.spacing.x = unit(14, "pt"),
    plot.margin = margin(22, 18, 10, 14)
  )

# ---- File path ----
# Use your local path if running locally; /mnt/data is for this chat workspace.
path_xlsx <- "C:/Users/kbrewer/OneDrive - California Air Resources Board/Biomass_MAIN/Biomass Modelling/HWP-C-vR/HWP Data/ExistingData/CA_Inputs_HWP_Model_Graph.xlsx"
# path_xlsx <- "/mnt/data/CA_Inputs_HWP_Model_Graph.xlsx"

# ---- Lightweight name cleaner (no janitor dependency) ----
clean_names_simple <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

# ---- Flexible matchers ----
is_year_col     <- function(nm) nm %in% "year"
is_inflow_col   <- function(nm) str_detect(nm, "^domestic_.*inflow$") | nm %in% "domestic_inflow"
is_c_emitted_col <- function(nm) {
  str_detect(nm, "^domestic_.*c.*(emitt|emiss|efflux)$") |
    nm %in% c("domestic_c_emitted","domestic_c_emissions","domestic_emitted_c","domestic_c_efflux")
}

# ---- Find a sheet that has Year, Domestic_Inflow, Domestic_C_Emitted (or variants) ----
sheets <- readxl::excel_sheets(path_xlsx)
probe <- map(sheets, function(sh) {
  df_head <- suppressMessages(readxl::read_excel(path_xlsx, sheet = sh, n_max = 5))
  nms <- clean_names_simple(names(df_head))
  list(sheet = sh, nms = nms,
       has_year = any(is_year_col(nms)),
       has_inflow = any(is_inflow_col(nms)),
       has_emit = any(is_c_emitted_col(nms)))
})
candidates <- keep(probe, ~ .x$has_year && .x$has_inflow && .x$has_emit)
if (length(candidates) == 0) {
  msg <- paste0(
    "Could not find a sheet with Year + Domestic_Inflow + Domestic_C_Emitted (or variants).\n\n",
    "Sheets scanned and their cleaned columns:\n",
    paste(map_chr(probe, ~ paste0("- ", .x$sheet, ": ", paste(.x$nms, collapse = ", "))), collapse = "\n")
  )
  stop(msg, call. = FALSE)
}
pick <- candidates[[1]]$sheet

# ---- Read chosen sheet & clean names ----
raw <- suppressMessages(readxl::read_excel(path_xlsx, sheet = pick))
names(raw) <- clean_names_simple(names(raw))

# Identify actual column names
year_col   <- names(raw)[is_year_col(names(raw))][1]
inflow_col <- names(raw)[is_inflow_col(names(raw))][1]
emit_col   <- names(raw)[is_c_emitted_col(names(raw))][1]
stopifnot(!is.na(year_col), !is.na(inflow_col), !is.na(emit_col))

# ---- Prepare data (filter to 2001–2022) ----
dat <- raw %>%
  transmute(
    Year          = as.integer(.data[[year_col]]),
    Inflow_Total  = as.numeric(.data[[inflow_col]]),
    C_Emitted     = as.numeric(.data[[emit_col]])
  ) %>%
  tidyr::drop_na(Year) %>%
  filter(dplyr::between(Year, 2001, 2022)) %>%   # <-- restrict reporting period
  mutate(Net = Inflow_Total - C_Emitted)

# ---- Long form for bars (C Input vs C Loss) ----
bars <- dat %>%
  pivot_longer(c(Inflow_Total, C_Emitted),
               names_to = "Component", values_to = "value") %>%
  mutate(
    value_plot = ifelse(Component == "Inflow_Total", value, -value),
    Component = factor(Component,
                       levels = c("Inflow_Total","C_Emitted"),
                       labels = c("Carbon Input","Carbon Loss"))
  )

# ---- Colors ----
col_tan      <- "#D2B48C"  # for C Input
col_bronze   <- "#B67C45"  # for C Loss
col_mahogany <- "#6F2E0F"  # Net C line

# ---- Axes scaling ----
y_all    <- range(c(bars$value_plot, dat$Net), na.rm = TRUE)
pad_y    <- 0.15 * max(abs(y_all))
ylim_y   <- c(y_all[1] - pad_y, y_all[2] + pad_y)
xr       <- range(dat$Year, na.rm = TRUE)
x_limits <- xr + c(-0.6, 0.6)

# ---- Plot ----
p <- ggplot() +
  # Bars
  geom_col(data = bars,
           aes(Year, value_plot, fill = Component),
           width = 0.8, alpha = 0.95) +
  # Net C line (solid)
  geom_line(data = dat,
            aes(Year, Net, color = "Net Carbon Stock Change"),
            linewidth = 1.6, lineend = "round") +
  # Fills (bars)
  scale_fill_manual(values = c(
    "Carbon Input" = col_tan,
    "Carbon Loss" = col_bronze
  )) +
  # Single line color
  scale_color_manual(values = c("Net Carbon Stock Change" = col_mahogany)) +
  # Legend guides: keep everything on one row
  guides(
    color = guide_legend(order = 1, nrow = 1, byrow = TRUE,
                         override.aes = list(linetype = "solid", linewidth = 1.8)),
    fill  = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  ) +
  # ---- Updated X-axis: show all years and rotate labels ----
scale_x_continuous(
  breaks = seq(min(dat$Year), max(dat$Year), by = 1),   # every year
  limits = x_limits,
  expand = expansion(mult = 0.01)
) +
  scale_y_continuous(
    limits = ylim_y, expand = expansion(mult = 0.01),
    name = "HWP Carbon Stock (Million Metric Tons)"
  ) +
  labs(
    x = "Year",
    title = "Annual Net Change in Harvested Wood Products Carbon Storage"
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)     # diagonal year labels
  )

print(p)
