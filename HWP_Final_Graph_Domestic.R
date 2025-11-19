# ---- Packages ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)  # for unit()

# ---- Theme ----
common_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 38),
    axis.text  = element_text(size = 30),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text  = element_text(size = 30),
    legend.key.size = unit(22, "pt"),
    legend.key.width = unit(25, "pt"),
    legend.box.margin = margin(8, 10, 0, 10),
    legend.spacing.x = unit(14, "pt"),
    plot.margin = margin(22, 18, 10, 14)
  )


# ---- File path & data ----
path_xlsx <- "C:/Users/kbrewer/OneDrive - California Air Resources Board/Biomass_MAIN/Biomass Modelling/HWP-C-vR/HWP Data/ExistingData/CA_Inputs_HWP_Model_Graph.xlsx"

# Use Domestic_ columns
required_cols <- c(
  "Year",
  "Domestic_Inflow",
  "Domestic_C_Emitted_CO2",
  "Domestic_C_Emitted_CH4",
  "Domestic_Inflow_Total_CO2",
  "Domestic_CO2e_Emitted"
)

pick <- NULL
for (sh in readxl::excel_sheets(path_xlsx)) {
  df_try <- suppressMessages(readxl::read_excel(path_xlsx, sheet = sh))
  if (all(required_cols %in% names(df_try))) { pick <- sh; break }
}
stopifnot(!is.null(pick))
raw <- readxl::read_excel(path_xlsx, sheet = pick)

# Select Domestic_* columns, then rename to generic names used downstream
dat <- raw %>%
  select(all_of(required_cols)) %>%
  transmute(
    Year              = as.integer(Year),
    Inflow_Total      = as.numeric(Domestic_Inflow),
    C_Emitted_CO2     = as.numeric(Domestic_C_Emitted_CO2),
    C_Emitted_CH4     = as.numeric(Domestic_C_Emitted_CH4),
    Inflow_Total_CO2  = as.numeric(Domestic_Inflow_Total_CO2),
    CO2e_Emitted      = as.numeric(Domestic_CO2e_Emitted)
  ) %>%
  mutate(
    Net      = Inflow_Total - C_Emitted_CO2 - C_Emitted_CH4,
    Net_CO2e = Inflow_Total_CO2 - CO2e_Emitted
  )

bars <- dat %>%
  pivot_longer(c(Inflow_Total, C_Emitted_CO2, C_Emitted_CH4),
               names_to = "Component", values_to = "value") %>%
  mutate(
    value_plot = ifelse(Component == "Inflow_Total", value, -value),
    Component = factor(
      Component,
      levels = c("Inflow_Total","C_Emitted_CO2","C_Emitted_CH4"),
      labels = c("Carbon Input","Carbon Loss (as CO2)","Carbon Loss (as CH4)")
    )
  )

# ---- Beige→Brown palette ----
col_tan       <- "#D2B48C"
col_bronze    <- "#B67C45"
col_saddle    <- "#8B4513"
col_mahogany  <- "#6F2E0F"
col_espresso  <- "#4B1D08"

# Requested line colors
col_yellow <- "#FF7518"
col_red    <- "#B22222"

# ---- Axes scaling ----
sf <- 44/12
y_all <- range(c(
  bars$value_plot, dat$Net,
  dat$Inflow_Total_CO2 / sf, -dat$CO2e_Emitted / sf, dat$Net_CO2e / sf
), na.rm = TRUE)
pad_y <- 0.15 * max(abs(y_all))
ylim_left <- c(y_all[1] - pad_y, y_all[2] + pad_y)
xr <- range(dat$Year, na.rm = TRUE)
x_limits <- xr + c(-0.6, 0.6)

# ---- INSERTED CODE: X-AXIS DECADE BREAKS ----
x_breaks_all <- seq(1910, 2020, by = 10)
x_breaks <- x_breaks_all[x_breaks_all >= x_limits[1] & x_breaks_all <= x_limits[2]]

# ---- Plot ----
p <- ggplot() +
  geom_col(data = bars,
           aes(Year, value_plot, fill = Component),
           width = 0.8, alpha = 0.95) +
  geom_line(data = dat,
            aes(Year, Net, color = "Net Carbon Stock Change",
                linetype = "Net Carbon Stock Change"),
            linewidth = 1.5) +
  geom_line(data = dat,
            aes(Year, Inflow_Total_CO2 / sf,
                color = "CO2-e Sequestration", linetype = "CO2-e Sequestration"),
            linewidth = 2.5) +
  geom_line(data = dat,
            aes(Year, -CO2e_Emitted / sf,
                color = "CO2-e Emission", linetype = "CO2-e Emission"),
            linewidth = 2.5) +
  geom_line(data = dat,
            aes(Year, Net_CO2e / sf,
                color = "Net CO2-e Mitigation",
                linetype = "Net CO2-e Mitigation"),
            linewidth = 2.5) +
  
  scale_fill_manual(values = c(
    "Carbon Input"    = col_tan,
    "Carbon Loss (as CO2)" = col_bronze,
    "Carbon Loss (as CH4)" = col_saddle
  )) +
  
  scale_color_manual(
    breaks = c("Net Carbon Stock Change", "CO2-e Emission", "CO2-e Sequestration", "Net CO2-e Mitigation"),
    values = c(
      "Net Carbon Stock Change"     = col_mahogany,
      "CO2-e Emission"              = col_red,
      "CO2-e Sequestration"         = col_yellow,
      "Net CO2-e Mitigation"        = col_espresso
    )
  ) +
  
  scale_linetype_manual(
    breaks = c("Net Carbon Stock Change", "CO2-e Emission", "CO2-e Sequestration", "Net CO2-e Mitigation"),
    values = c("solid","dotted","dotted","dotted")
  ) +
  
  guides(
    color = guide_legend(
      order = 1, nrow = 1, byrow = TRUE,
      override.aes = list(
        linetype = c("solid","dotted","dotted","dotted"),
        linewidth = 2.2
      )
    ),
    fill  = guide_legend(order = 2, nrow = 1),
    linetype = "none"
  ) +
  
  # ---- USE THE NEW BREAKS ----
scale_x_continuous(
  limits = x_limits,
  breaks = x_breaks,
  expand = expansion(mult = 0.01)
) +
  
  scale_y_continuous(
    limits = ylim_left,
    expand = expansion(mult = 0.01),
    name = "HWP Carbon Stock\n(MMT C)",
    sec.axis = sec_axis(~ . * sf,
                        name = "HWP GHG Mitigation\n(MMT CO2-e)")
  ) +
  
  labs(x = "Year", title = "Annual Net Change in Carbon Storage") +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)











# Just NET CARBON STOCK CHANGE and NET CO2-e MITIGATION

# ---- Theme ----
common_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 38),
    axis.text  = element_text(size = 30),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text  = element_text(size = 30),
    legend.key.size = unit(25, "pt"),
    legend.key.width = unit(25, "pt"),
    legend.box.margin = margin(8, 10, 0, 10),
    legend.spacing.x = unit(20, "pt"),
    plot.margin = margin(22, 18, 10, 14)
  )

# ---- File path & data ----
path_xlsx <- "C:/Users/kbrewer/OneDrive - California Air Resources Board/Biomass_MAIN/Biomass Modelling/HWP-C-vR/HWP Data/ExistingData/CA_Inputs_HWP_Model_Graph.xlsx"
# Use Domestic_ columns
required_cols <- c(
  "Year",
  "Domestic_Inflow",
  "Domestic_C_Emitted_CO2",
  "Domestic_C_Emitted_CH4",
  "Domestic_Inflow_Total_CO2",
  "Domestic_CO2e_Emitted"
)
pick <- NULL
for (sh in readxl::excel_sheets(path_xlsx)) {
  df_try <- suppressMessages(readxl::read_excel(path_xlsx, sheet = sh))
  if (all(required_cols %in% names(df_try))) { pick <- sh; break }
}
stopifnot(!is.null(pick))
raw <- readxl::read_excel(path_xlsx, sheet = pick)

dat <- raw %>%
  select(all_of(required_cols)) %>%
  mutate(across(-Year, as.numeric), Year = as.integer(Year)) %>%
  mutate(Net = Domestic_Inflow - Domestic_C_Emitted_CO2 - Domestic_C_Emitted_CH4,
         Net_CO2e = Domestic_Inflow_Total_CO2 - Domestic_CO2e_Emitted)

# ---- Colors ----
col_mahogany <- "#D2B48C"  # Net Carbon Stock Change (solid)
col_espresso <- "#4B1D08"  # Net CO2-e Mitigation (dashed)

# ---- Axes scaling ----
sf <- 44/12  # scaling factor between C and CO2e axis

y_all <- range(c(dat$Net, dat$Net_CO2e / sf), na.rm = TRUE)
pad_y <- 0.15 * max(abs(y_all))
ylim_left <- c(y_all[1] - pad_y, y_all[2] + pad_y)

xr <- range(dat$Year, na.rm = TRUE)
x_limits <- xr + c(-0.6, 0.6)

# left y-axis breaks: multiples of 3
left_breaks <- seq(
  from = floor(min(ylim_left) / 3) * 3,
  to   = ceiling(max(ylim_left) / 3) * 3,
  by   = 3
)

# x-axis decade breaks: 1910, 1920, ..., 2010 (clipped to data range)
x_breaks_all <- seq(1910, 2020, by = 10)
x_breaks <- x_breaks_all[x_breaks_all >= x_limits[1] & x_breaks_all <= x_limits[2]]

# ---- Plot ----
p <- ggplot(dat, aes(x = Year)) +
  # baseline at zero
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  
  geom_line(
    aes(y = Net,
        color = "Net Carbon Stock Change",
        linetype = "Net Carbon Stock Change"),
    linewidth = 3.5
  ) +
  geom_line(
    aes(y = Net_CO2e / sf,
        color = "Net CO2-e Mitigation",
        linetype = "Net CO2-e Mitigation"),
    linewidth = 3.5
  ) +
  
  scale_color_manual(
    breaks = c("Net Carbon Stock Change", "Net CO2-e Mitigation"),
    values = c(
      "Net Carbon Stock Change" = col_mahogany,
      "Net CO2-e Mitigation"    = col_espresso
    )
  ) +
  scale_linetype_manual(
    breaks = c("Net Carbon Stock Change", "Net CO2-e Mitigation"),
    values = c("solid", "dotted")
  ) +
  
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      byrow = TRUE,
      keywidth = unit(40, "pt"),
      override.aes = list(
        linetype = c("solid", "dotted"),
        linewidth = 3
      )
    ),
    linetype = "none"
  ) +
  
  
  # x-axis: decade ticks
  scale_x_continuous(
    limits = x_limits,
    breaks = x_breaks,
    expand = expansion(mult = 0.01)
  ) +
  
  # y-axes: left in multiples of 3; right aligned with same grid lines
  scale_y_continuous(
    limits = ylim_left,
    breaks = left_breaks,
    expand = expansion(mult = 0.01),
    name = "HWP Carbon Stock\n(MMT C)",
    sec.axis = sec_axis(
      ~ . * sf,
      breaks = left_breaks * sf,
      name = "HWP GHG Mitigation\n(MMT CO2-e)"
    )
  ) +
  
  labs(
    x = "Year",
    title = "Annual Net Change in Carbon Storage"
  ) +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)




# PIU vs SWDS CUMULATIVE STOCK

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

# ---- Read data ----
# Adjust `sheet =` if needed to match your workbook
hwp_df <- readxl::read_excel(
  "C:/Users/kbrewer/OneDrive - California Air Resources Board/Biomass_MAIN/Biomass Modelling/HWP-C-vR/HWP Data/ExistingData/CA_Inputs_HWP_Model_Graph.xlsx",
  sheet = 1
)

# Expecting columns: Year, Domestic_Stock_PIU, Domestic_Stock_SWDS
# Rename for nicer legend labels and pivot to long format
plot_df <- hwp_df %>%
  transmute(
    Year = as.numeric(Year),
    `Products in Use`            = Domestic_Stock_PIU,
    `Solid Waste Disposal Sites` = Domestic_Stock_SWDS
  ) %>%
  pivot_longer(
    cols     = c(`Products in Use`, `Solid Waste Disposal Sites`),
    names_to = "Pool",
    values_to = "Value"
  )

# Put SWDS on the bottom, PIU on top
plot_df$Pool <- factor(
  plot_df$Pool,
  levels = c("Products in Use", "Solid Waste Disposal Sites")
)

# ---- Y-axis padding so top doesn’t touch panel ----
max_y <- max(plot_df$Value, na.rm = TRUE)

# ---- Plot ----
p_hwp_stock <- ggplot(plot_df, aes(x = Year, y = Value, fill = Pool)) +
  geom_area(
    color = "black",
    linewidth = 0.2,
    alpha = 0.95,
    position = "stack"
  ) +
  scale_fill_manual(
    values = c(
      "Products in Use"            = "#D8C097",
      "Solid Waste Disposal Sites" = "#5F280D"
    )
  ) +
  scale_y_continuous(
    limits = c(0, max_y * 1.65),  # 10% headroom
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "HWP Carbon Stock (MMT C)"
  ) +
  common_theme

print(p_hwp_stock)


