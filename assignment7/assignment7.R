##### Assignment 7: Spatial Data I

library(sf)
library(spData)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)

#### 1 Part 1: In-Class (Exploring Spatial Data with sf) ---------------------

data(world)

### 1.1 Inspecting an sf object ----------------------------------------------

## a)

class(world)
names(world)
nrow(world)

# A regular R data frame is purely tabular, containing only rows and columns 
# of characters, factors, or numbers, while an 'sf' object is a data frame with 
# spatial characteristics. The 'geometry' column is what distinguishes an sf 
# object. Unlike a standard column, it is stored as a 'list-column' 
# (specifically a 'sfc' object). Each entry in this list contains the actual 
# coordinates (points, lines, or polygons) that define the shape of the feature 
# (e.g., a country's borders). It also bundles essential metadata, 
# such as the Coordinate Reference System (CRS), which tells R how 
# to project those coordinates onto a map of the Earth.

## b)

st_crs(world)

# WGS84 stands for World Geodetic System 1984. It is a geographic 
# coordinate system that uses a three-dimensional terrestrial 
# reference frame to define locations on Earth. Unlike a "projected" 
# CRS (which flattens the world into a 2D map like a piece of paper), 
# WGS84 treats the Earth as an ellipsoid.

# WGS84 is the global standard because it is the reference system 
# used by the Global Positioning System (GPS). Since it provides 
# a single, consistent model for the entire planet, it is the 
# default for almost all global geographic data, web mapping and 
# international aviation/navigation. It allows us to define any point 
# on Earth using a simple pair of coordinates: Latitude and Longitude.

## c)

st_geometry_type(world)
unique(st_geometry_type(world))

# A POLYGON is a single enclosed shape (a single ring of coordinates). 
# A MULTIPOLYGON is a collection of multiple polygons that are treated 
# as a single logical entity (one row in the data frame). This is 
# essential for representing geographic features that are not 
# contiguous—meaning they are made of several "islands" or 
# separated landmasses that all belong to the same country.

# Regarding the examples, we could point at countries like:
# 1. France: due to its somewhat recent colonial empire, France still holds 
#    multiple spots of land scattered around the world. Although many
#    projections would simply not include them, the existence of 
#    big territories outside the 'hexagon' like French Guiana makes it
#    often necessary to use a MULTIPOLYGON to represent the country.
# 2. The United States: also requires a MULTIPOLYGON to represent every single
#    state, including the 48 of the continental US,the detached state of 
#    Alaska, and the islands of Hawaii. A faithful representation would also
#    include other overseas territories, like PR, Guam or America Samoa.

## d)

pdf("plots/world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

# 1. Wealthiest Regions (Purple/Pink/Yellow shades):
# North America (USA and Canada) and Oceania (Australia and NZ) stand 
# out with significantly higher GDP per capita, appearing in the 
# purple/pink range of the scale. Parts of Western Europe and 
# high-income Middle Eastern nations (like Saudi Arabia or the UAE) 
# also show relatively higher wealth.

# 2. Poorest Regions (Dark Blue shades):
# The vast majority of the African continent, as well as large 
# portions of South and Southeast Asia and Latin America, appear 
# in deep blue, indicating that they fall in the lowest bracket 
# of the GDP per capita scale.

#### 1.2 Attribute operations ------------------------------------------------

## a)

africa = world %>% 
  filter(continent == "Africa")

nrow(africa)

plot(africa["gdpPercap"], main = "GDP per Capita in Africa")

# The filtered dataset contains 51 countries for the African continent.  
# This count might feel slightly "off" depending on the source, as, for example, 
# the African Union recognizes 55 member states. The discrepancy usually arises
# because global mapping datasets often omit very small island nations
# (like Seychelles or Mauritius) at lower resolutions, or they may not have 
# administrative/GDP data for disputed territories like Western Sahara,
# which are represented as geometries but excluded from attribute-based filters.

## b)

world = world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

print(gdp_by_continent)

## c)

top_5_africa = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap) %>%
  slice(1:5)

print(top_5_africa)

# According to the dataset, the five African countries with the highest GDP 
# per capita are:
# 1. Equatorial Guinea
# 2. Botswana
# 3. Gabon
# 4. South Africa
# 5. Namibia

#### 1.3 Simple visualization with ggplot2 -----------------------------------

## a)

ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")

ggsave("plots/world_gdp.pdf", width = 10, height = 5)

# Wealthiest Regions (Pink/Red shades):
# The wealthiest regions are primarily located in the "Global North" 
# and Oceania. Specifically, North America (USA and Canada), 
# Western Europe and Oceania (Australia and NZ) show the highest income levels. 
# A notable outlier in the Middle East is the Arabian Peninsula (like Saudi 
# Arabia and the UAE), which appears in a distinct reddish-pink 
# indicating high resource-driven wealth.

# Poorest Regions (Deep Blue/Dark Purple shades):
# Sub-Saharan Africa is the most consistently poor region on the 
# map, appearing in the darkest blue tones. Large parts of South 
# and Southeast Asia, as well as portions of Central and South 
# America, also fall into the lower-income brackets compared 
# to the industrialized nations.

## b)

africa_gdp_map <- ggplot(data = africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", name = "GDP per capita") +
  labs(title = "GDP per Capita across Africa",
       subtitle = "Data source: spData 'world' dataset") +
  theme_minimal()

africa_gdp_map

ggsave("plots/africa_gdp.pdf", plot = africa_gdp_map, width = 8, height = 6)

# The map reveals a highly unequal economic landscape characterized 
# by extreme outliers. The most prominent feature is a small "bright" 
# spot in Central-West Africa—Equatorial Guinea—which appears in 
# yellow, indicating a GDP per capita significantly higher than 
# its neighbors (exceeding $30,000). Generally, wealth is concentrated 
# at the continental "periphery": the Northern African nations (like Libya and 
# Algeria) and the Southern African nations (like South Africa and Botswana) 
# appear in brighter pink/purple shades.

## c)

africa_gdp_borders <- ggplot(data = africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", name = "GDP per capita") +
  labs(title = "GDP per Capita across Africa (with Borders)",
       subtitle = "Data source: spData 'world' dataset") +
  theme_minimal()

africa_gdp_borders

ggsave("plots/africa_gdp_borders.pdf", plot = africa_gdp_borders, width = 8, height = 6)

# The border layer improves readability by providing visual separation 
# between adjacent countries. In the "magma" scale, many neighboring 
# low-income countries in Central Africa are shaded nearly identical 
# dark tones, so the white borders prevent these from blending into a 
# single black mass.

#### 2 Part 2: Take-Home (Point Data and Spatial Joins) ----------------------

df <- read.csv("data/conflict_events.csv")


### 2.1 Converting tabular data to sf ---------------------------------------

## a)

events_sf = st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

class(events_sf)
st_crs(events_sf)

# 'st_as_sf()' transforms a standard data frame into a spatial 'sf' object 
# by creating a new 'geometry' column. The 'coords' argument tells R which 
# existing columns in the data frame represent the horizontal (x) and vertical 
# (y) positions on a map (in this case, 'longitude' and 'latitude').
# For its parte, 'crs = 4326' defines the Coordinate Reference System as WGS84. 
# This is the standard global system used by GPS and Google Maps, where 
# coordinates are measured in decimal degrees on a spherical model of the Earth.

## b)

nrow(df)
table(events_sf$event_type)

# Based on the output, the most common event type is "state-based" 
# conflict, with 33,487 recorded events.

## c)

conflict_map <- ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white", linewidth = 0.1) +
  geom_sf(data = events_sf, aes(color = event_type), alpha = 0.5, size = 0.5) +
  scale_color_viridis_d(name = "Event Type") +
  labs(title = "Global Distribution of Conflict Events",
       subtitle = "Point data overlaid on world polygons") +
  theme_minimal()

conflict_map

ggsave("plots/conflict_events_map.png", plot = conflict_map, width = 10, height = 6)

# Conflict events are most densely concentrated in the Greater Horn of Africa
# (Ethiopia, Somalia, and South Sudan), the Great Lakes Region (eastern DRC), 
# and West Africa & the Sahel belt (Nigeria, Mali, and Burkina Faso).
# In contrast, the Southern cone (Botswana, Namibia) and the Sahara 
# desert regions show much lower event density, likely due to a 
# combination of higher political stability in the south and extremely 
# low population density in the desert interior.


### 2.2 Spatial join: events to countries -----------------------------------

## a)

st_crs(world) == st_crs(events_sf)

events_joined = st_join(events_sf, world)

nrow(events_joined)
nrow(events_sf)

# The function 'st_join()' performs a "spatial left join". It looks at the 
# coordinates of each point in 'events_sf' and checks which polygon in 'world' 
# contains those coordinates. It then appends the attributes (like name_long and 
# gdpPercap) of that specific country to the event's row. Checking the CRS is 
# crucial because if the systems differ (for example, if one uses meters and 
# the other uses degrees) the software will essentially be looking for a point 
# in the wrong place, leading to failed joins or incorrect data assignment.

## b)

na_count <- sum(is.na(events_joined$name_long))
total_events <- nrow(events_joined)
na_fraction <- na_count / total_events

print(paste("Number of unmatched events:", na_count))
print(paste("Fraction of unmatched events:", round(na_fraction, 4)))

# Only 1576 events, or around 2.3%, do not match any country polygon.
# Possibly some of the unmatched events occurred on coastal or maritime
# locations, and, thus, their coordinates would not intersect with any 
# terrestrial polygons in the dataset. Another possible explanation would be
# polygon simplification. The 'world' object often uses simplified geometries 
# to save memory. Hence, points located on jagged coastlines or small peninsulas  
# might technically fall into the "ocean" according to a low-resolution map, 
# even if they were on land in reality.

## c)

country_stats <- events_joined %>%
  filter(!is.na(name_long)) %>%
  st_drop_geometry() %>%
  group_by(name_long) %>%
  summarise(
    event_count = n(),
    total_fatalities = sum(fatalities, na.rm = TRUE)
  ) %>%
  arrange(desc(event_count))

print(head(country_stats, 10))

# The results are highly consistent with contemporary conflict data, and
# highlights a clear distinction between conflict frequency and intensity. 
# Countries like the DRC, Nigeria, and Somalia show the highest 
# event counts, reflecting long-term, fragmented insurgencies. However, 
# the fatality data tells a different story: Rwanda and Ethiopia show 
# disproportionately high death tolls relative to their event counts, 
# capturing high-intensity periods like the 1994 Genocide and major 
# civil wars. Conversely, South Africa’s presence in the top 10 for 
# events—but with very low fatalities—suggests that its conflict data 
# is likely dominated by civil unrest and protests rather than 
# large-scale military engagements.

### 2.3 Choropleth of conflict intensity ------------------------------------

## a)

country_counts_df <- country_stats %>%
  st_drop_geometry()

world_with_events <- world %>%
  left_join(country_counts_df, by = "name_long")

world_with_events <- world_with_events %>%
  mutate(
    event_count = replace_na(event_count, 0),
    total_fatalities = replace_na(total_fatalities, 0)
  )

nrow(world_with_events) == nrow(world)

## b)

conflict_choropleth <- ggplot(data = world_with_events) +
  geom_sf(aes(fill = event_count), color = "white", linewidth = 0.1) +
  scale_fill_distiller(palette = "Reds", direction = 1, name = "Event Count") +
  # Focus the map on Africa
  coord_sf(xlim = c(-20, 55), ylim = c(-35, 40)) +
  labs(title = "Total Conflict Events by Country",
       subtitle = "Aggregated event counts from point data") +
  theme_minimal()

conflict_choropleth

ggsave("plots/conflict_choropleth_africa.png", plot = conflict_choropleth)

# The geographic patterns in the choropleth map are highly consistent 
# with the event-level point map from 2.1c. The dense clusters of 
# points seen in Nigeria, the DRC, Ethiopia, and Somalia translate directly 
# into the darkest red polygons on the choropleth map.

# However, the choropleth provides a different perspective on 
# regional intensity. While the point map highlighted specific 
# internal hotspots (like Eastern DRC or Northern Nigeria), the 
# choropleth aggregates this data to show the total national burden. 
# Both maps confirm that conflict is concentrated in the Sahel, 
# the Great Lakes, and the Horn of Africa, while Southern Africa 
# and the deep Sahara remain significantly less affected.

## c)

conflict_log_map <- ggplot(data = world_with_events) +
  geom_sf(aes(fill = log1p(event_count)), color = "white", linewidth = 0.1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Log(events+1)") +
  labs(title = "Global Distribution of Conflict (Log-Transformed)",
       subtitle = "Using log1p to better visualize variation across all countries") +
  theme_minimal()

conflict_log_map

ggsave("plots/conflict_log_map.pdf", plot = conflict_log_map, width = 10, height = 6)

# The log transformation (log1p) is very useful here because 
# conflict data is highly "right-skewed". In the raw map, the extreme 
# outliers like the DRC (9,000+ events) dominated the color scale, 
# forcing almost every other country into the lightest shade. 

# By using a log scale, we "compress" the top end and "stretch" the 
# bottom end of the data. This reveals variation that was previously 
# invisible: we can now clearly distinguish between countries with 
# low conflict (like Namibia) and moderate conflict (like Morocco or 
# Tanzania). 

### 2.4 Bonus (optional): Are events far from the capital city more  --------

## a) & b)

nigeria_events <- events_joined %>%
  filter(name_long == "Nigeria")

## c)

capitals_df <- data.frame(
  city = "Abuja",
  lat = 9,
  lon = 7.5
)

## d)

abuja_sf <- st_as_sf(capitals_df, coords = c("lon", "lat"), crs = 4326)

print(abuja_sf)

nigeria_events_utm <- st_transform(nigeria_events, crs = 32632)
abuja_utm <- st_transform(abuja_sf, crs = 32632)


st_crs(nigeria_events_utm)$units

## e)

nigeria_events_utm$dist_to_abuja_km <- as.numeric(st_distance(nigeria_events_utm, abuja_utm)) / 1000

head(nigeria_events_utm[, c("dist_to_abuja_km", "fatalities")])

## f)

model1 <- lm(log1p(fatalities) ~ log(dist_to_abuja_km), 
             data = nigeria_events_utm)

model2 <- lm(log1p(fatalities) ~ log(dist_to_abuja_km) + event_type, 
             data = nigeria_events_utm)

model3 <- lm(log1p(fatalities) ~ log(dist_to_abuja_km) * event_type, 
             data = nigeria_events_utm)

stargazer(model1, model2, model3, 
          type = "text", 
          title = "Modelos de Regresión: Fatalidades vs. Distancia a Abuja",
          column.labels = c("Básico", "Controles", "Interacción"),
          covariate.labels = c("Log(Distancia)", "Tipo Violencia", "Interacción"),
          dep.var.labels = "Log(Fatalidades + 1)")
         

## g)

# The regression results confirm that conflict events in Nigeria tend to be 
# more deadly as they occur further from the national capital, Abuja. While 
# distance alone (Model 1) is not a significant predictor, the inclusion of 
# "type_of_violence" (Model 2) reveals a positive and highly significant 
# relationship (0.098***). This suggests that distance was previously masked 
# by the different baseline lethalities of conflict types. For its part, 
# Model 3 provides the strongest evidence for the hypothesis. The interaction 
# term for State-based conflict is large and positive (0.440***). This 
# indicates that the "distance penalty" is most severe when the government is
# a party to the conflict.These findings support the idea that the state’s 
# ability to project power and minimize fatalities diminishes toward the 
# country's periphery. 

### 2.5 Discussion ----------------------------------------------------------

## a)

# One major limitation of the 'st_join' approach is its sensitivity to 
# coordinate precision and "point-in-polygon" edge cases. Events that 
# fall exactly on a border may be assigned to multiple countries or 
# none at all, while events occurring just offshore or near a coastline 
# might be excluded entirely if they do not overlap with the terrestrial 
# polygon. To handle these imprecisions, a more robust method would be 
# to use a "nearest neighbor" join (st_nearest_feature) or to apply 
# a small buffer around the country borders to capture near-miss events.

## b)

# The primary difference is the matching criteria. While 'left_join' uses 
# shared attributes (like a "country_id" column) to link rows,'st_join' 
# uses spatial intersection (overlapping coordinates). 
# We would use 'left_join' when we have a clean, common ID 
# across datasets, as it is faster and avoids geometric errors. 
# Conversely, 'st_join' is essential when you have raw GPS points 
# and need to determine which geographic boundary they belong to based 
# strictly on their location.

