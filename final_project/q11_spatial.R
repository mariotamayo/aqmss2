# =============================================================================
# Q11 Spatial Analysis: Geographic Distribution of Q11 Responses Across EU
# Flash Eurobarometer 3592 (2025) — GESIS ZA9078
# =============================================================================
# Author: Mario Tamayo
# Date: 2026-04-19
# =============================================================================

# --- Load libraries ----------------------------------------------------------
library(haven)       # Reading Stata .dta files
library(tidyverse)   # Data wrangling and visualization (dplyr, ggplot2, etc.)
library(sf)          # Simple features for spatial data
library(giscoR)      # Official Eurostat/GISCO country boundaries
library(patchwork)   # Combining multiple ggplot objects
library(ggrepel)     # Non-overlapping text labels on maps
library(viridis)     # Perceptually uniform color scales

# =============================================================================
# 1. FILE PATHS
# =============================================================================

# Base directory for all input/output files
base_dir <- "/Users/mariotamayo/Documents/Doctorado/Primero/Asignaturas/AQMSS2/aqmss2_assignments/final_project"

# Path to the Eurobarometer dataset
data_path <- file.path(base_dir, "ZA9078_v1-0-0.dta")

# Output file paths
out_facet_map <- file.path(base_dir, "q11_facet_map.png")
out_q11_9_map <- file.path(base_dir, "q11_9_map.png")
out_barplot   <- file.path(base_dir, "q11_barplot.png")

# =============================================================================
# 2. Q11 ITEM METADATA
# =============================================================================

# Variable names for Q11 items
q11_vars <- paste0("q11_", 1:10)

# Human-readable labels for each Q11 item
q11_labels <- c(
  q11_1  = "More visible accounts",
  q11_2  = "More captivating formats",
  q11_3  = "More relevant topics",
  q11_4  = "More frequent updates",
  q11_5  = "Clearer language",
  q11_6  = "Trusted recommendations",
  q11_7  = "Native language content",
  q11_8  = "Other",
  q11_9  = "Nothing would make me follow",
  q11_10 = "Don't know"
)

# Variables to include in the main faceted map and bar chart
# (excluding q11_8 "Other" and q11_10 "Don't know")
q11_plot_vars <- paste0("q11_", c(1:7, 9))

# EU-27 ISO 3166-1 alpha-2 country codes present in the dataset
eu27_iso <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI",
  "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
  "NL", "PL", "PT", "RO", "SE", "SI", "SK"
)

# =============================================================================
# 3. DATA LOADING & WRANGLING
# =============================================================================

message("Loading dataset...")
raw <- read_dta(data_path)

# Filter to the valid Q11 subsample:
# Q11 was only asked to respondents who do NOT currently follow EU institutions
# (i.e., those filtered in at Q8). A non-NA value on q11_1 identifies them.
df_q11 <- raw %>%
  filter(!is.na(q11_1)) %>%
  # Keep only relevant variables for efficiency
  select(isocntry, w1, all_of(q11_vars))

message(paste("Q11 subsample size:", nrow(df_q11), "respondents"))
message(paste("Countries represented:", n_distinct(df_q11$isocntry)))

# Compute weighted country-level proportions for each Q11 item.
# For binary items (0/1), the weighted mean equals the weighted proportion.
country_props <- df_q11 %>%
  group_by(isocntry) %>%
  summarise(
    across(
      all_of(q11_vars),
      # Weighted proportion: sum(weight * value) / sum(weight), * 100 for %
      ~ weighted.mean(.x, w = w1, na.rm = TRUE) * 100,
      .names = "{.col}_pct"
    ),
    n_weighted = sum(w1, na.rm = TRUE),  # Effective weighted N per country
    .groups = "drop"
  )

message("Country-level weighted proportions computed.")
glimpse(country_props)

# =============================================================================
# 4. SPATIAL DATA: Download and prepare EU country polygons
# =============================================================================

message("Downloading EU country polygons from GISCO...")

# Download country polygons at 1:20M resolution (faster download, adequate detail)
eu_sf_raw <- gisco_get_countries(
  region     = "Europe",
  resolution = "20",
  epsg       = "4326"   # WGS84 geographic coordinates
)

# Filter to EU-27 member states only
eu_sf <- eu_sf_raw %>%
  filter(CNTR_ID %in% eu27_iso)

message(paste("EU countries in spatial data:", nrow(eu_sf)))

# Join Q11 proportions to spatial data
# giscoR uses CNTR_ID as the ISO code column; dataset uses isocntry
eu_sf_data <- eu_sf %>%
  left_join(country_props, by = c("CNTR_ID" = "isocntry"))

# Check for any countries missing Q11 data
missing_data <- eu_sf_data %>%
  filter(is.na(q11_1_pct)) %>%
  pull(CNTR_ID)

if (length(missing_data) > 0) {
  warning(paste("Countries missing Q11 data:", paste(missing_data, collapse = ", ")))
} else {
  message("All EU-27 countries have Q11 data.")
}

# Compute centroids for country label placement (used in Map 2)
eu_centroids <- eu_sf %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(CNTR_ID, lon, lat)

# =============================================================================
# 5. RESHAPE DATA FOR FACETED MAPPING
# =============================================================================

# Pivot to long format for faceting: one row per country x Q11 item
eu_sf_long <- eu_sf_data %>%
  # Select geometry + all pct columns for plot variables
  select(CNTR_ID, geometry,
         all_of(paste0(q11_plot_vars, "_pct"))) %>%
  pivot_longer(
    cols      = ends_with("_pct"),
    names_to  = "variable",
    values_to = "pct"
  ) %>%
  # Strip the "_pct" suffix to recover the original variable name
  mutate(
    var_name  = str_remove(variable, "_pct"),
    # Map variable name to human-readable label
    var_label = q11_labels[var_name],
    # Order factor by original variable order (q11_1, q11_2, ..., q11_9)
    var_label = factor(var_label, levels = q11_labels[q11_plot_vars])
  )

# =============================================================================
# 6. MAP PLOT 1 — FACETED CHOROPLETH MAP
# =============================================================================

message("Creating faceted choropleth map (Map 1)...")

# Define map extent for EU (excludes overseas territories)
xlim_eu <- c(-25, 45)
ylim_eu <- c(34, 72)

map_faceted <- ggplot(eu_sf_long) +
  # Draw country polygons, filled by weighted %
  geom_sf(aes(fill = pct), color = "white", linewidth = 0.3) +
  # Sequential viridis color scale (option "D" = classic viridis)
  scale_fill_viridis_c(
    option   = "D",
    name     = "Weighted %",
    labels   = function(x) paste0(round(x), "%"),
    na.value = "grey85",
    guide    = guide_colorbar(
      title.position = "top",
      barwidth       = unit(8, "lines"),
      barheight      = unit(0.5, "lines")
    )
  ) +
  # Facet by Q11 item label, 3 columns
  facet_wrap(~ var_label, ncol = 3) +
  # Crop to EU extent
  coord_sf(xlim = xlim_eu, ylim = ylim_eu, expand = FALSE) +
  # Labels
  labs(
    title    = "What Would Make Citizens Follow EU Institutions on Social Media?",
    subtitle = "Weighted % of non-followers selecting each option, by country (Flash Eurobarometer 3592, 2025)",
    caption  = paste(
      "Source: GESIS ZA9078.",
      "Note: Sample restricted to respondents not currently following EU institutions (Q8 \u2260 6).",
      "Weighted by w1."
    )
  ) +
  # Clean void theme (no axes, no grid) suitable for maps
  theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0.5,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, hjust = 0.5, color = "grey30",
                                    margin = margin(b = 8)),
    plot.caption     = element_text(size = 7, color = "grey50", hjust = 0,
                                    margin = margin(t = 8)),
    strip.text       = element_text(face = "bold", size = 8.5,
                                    margin = margin(b = 3)),
    legend.position  = "bottom",
    legend.title     = element_text(size = 8, face = "bold"),
    legend.text      = element_text(size = 7),
    plot.margin      = margin(10, 10, 10, 10)
  )

# =============================================================================
# 7. MAP PLOT 2 — FOCUSED MAP: q11_9 "Nothing would make me follow"
# =============================================================================

message("Creating focused map for q11_9 (Map 2)...")

# Extract q11_9 data for focused map
eu_sf_q9 <- eu_sf_data %>%
  select(CNTR_ID, geometry, q11_9_pct) %>%
  left_join(eu_centroids, by = "CNTR_ID")

map_q11_9 <- ggplot(eu_sf_q9) +
  # Country polygons filled by q11_9 weighted %
  geom_sf(aes(fill = q11_9_pct), color = "white", linewidth = 0.4) +
  # Sequential red color scale: higher = more impermeable to outreach
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,            # Higher values = darker red
    name     = "Weighted %",
    labels   = function(x) paste0(round(x), "%"),
    na.value = "grey85",
    guide    = guide_colorbar(
      title.position = "top",
      barwidth       = unit(8, "lines"),
      barheight      = unit(0.6, "lines")
    )
  ) +
  # Add ISO country code labels, repelling overlaps
  geom_label_repel(
    data        = eu_sf_q9 %>% st_drop_geometry(),
    aes(x = lon, y = lat, label = CNTR_ID),
    size        = 2.5,
    fontface    = "bold",
    color       = "grey20",
    fill        = alpha("white", 0.6),
    label.size  = 0,
    box.padding = 0.2,
    max.overlaps = 30,
    seed        = 42
  ) +
  # Crop to EU extent
  coord_sf(xlim = xlim_eu, ylim = ylim_eu, expand = FALSE) +
  # Labels
  labs(
    title    = "\u2018Nothing Would Make Me Follow EU Institutions on Social Media\u2019",
    subtitle = "Weighted % by country \u2014 Flash Eurobarometer 3592 (2025)",
    caption  = "Source: GESIS ZA9078. Weighted by w1."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30",
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 0,
                                 margin = margin(t = 8)),
    legend.position  = "bottom",
    legend.title     = element_text(size = 8, face = "bold"),
    legend.text      = element_text(size = 7),
    plot.margin      = margin(10, 10, 10, 10)
  )

# =============================================================================
# 8. BAR CHART — Overall weighted distribution of Q11 responses
# =============================================================================

message("Creating overall bar chart...")

# Compute overall weighted proportions across all countries combined
overall_props <- df_q11 %>%
  summarise(
    across(
      all_of(q11_plot_vars),
      ~ weighted.mean(.x, w = w1, na.rm = TRUE) * 100
    )
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "var_name",
    values_to = "pct"
  ) %>%
  mutate(
    # Full human-readable label
    label = q11_labels[var_name],
    label = factor(label, levels = q11_labels[q11_plot_vars]),
    # Flag q11_9 for highlighting (impermeable to outreach)
    is_nothing = (var_name == "q11_9")
  ) %>%
  # Sort bars by descending frequency
  arrange(desc(pct)) %>%
  mutate(label = fct_reorder(label, pct))

# Define bar colors: red-orange for q11_9, steel blue for actionable options
bar_colors <- c("TRUE" = "#D73027", "FALSE" = "#4575B4")

barplot_q11 <- ggplot(overall_props, aes(x = pct, y = label, fill = is_nothing)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  # Add percentage labels at end of bars
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    hjust  = -0.15,
    size   = 3.2,
    color  = "grey30"
  ) +
  scale_fill_manual(values = bar_colors) +
  # Extend x-axis slightly to accommodate labels
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "What Would Make Citizens Follow EU Institutions on Social Media?",
    subtitle = "Overall weighted % of non-followers selecting each option (EU-27 combined)",
    x        = "Weighted % of respondents",
    y        = NULL,
    caption  = paste(
      "Source: GESIS ZA9078.",
      "Note: 'Other' and 'Don't know' excluded.",
      "\nHighlighted in red: 'Nothing would make me follow'.",
      "Weighted by w1."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 9, color = "grey30", hjust = 0,
                                   margin = margin(b = 8)),
    plot.caption    = element_text(size = 7, color = "grey50", hjust = 0,
                                   margin = margin(t = 8)),
    axis.text.y     = element_text(size = 9),
    axis.text.x     = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin     = margin(10, 20, 10, 10)
  )

# =============================================================================
# 9. SAVE OUTPUTS
# =============================================================================

message("Saving outputs...")

# Map 1: Faceted choropleth map (all Q11 items)
ggsave(
  filename = out_facet_map,
  plot     = map_faceted,
  width    = 14,
  height   = 10,
  dpi      = 300,
  bg       = "white"
)
message(paste("Saved:", out_facet_map))

# Map 2: Focused map for q11_9 "Nothing would make me follow"
ggsave(
  filename = out_q11_9_map,
  plot     = map_q11_9,
  width    = 8,
  height   = 7,
  dpi      = 300,
  bg       = "white"
)
message(paste("Saved:", out_q11_9_map))

# Bar chart: Overall weighted distribution
ggsave(
  filename = out_barplot,
  plot     = barplot_q11,
  width    = 8,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)
message(paste("Saved:", out_barplot))

message("All outputs saved successfully.")

# =============================================================================
# END OF SCRIPT
# =============================================================================
