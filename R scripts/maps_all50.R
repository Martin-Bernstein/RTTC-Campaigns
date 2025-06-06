library(ggplot2)
library(maps)
library(plotly)
library(data.table)
library(htmltools)
library(dplyr)
library(usmap)

####State-level####
# ——————————————————————————————————————————————
# (A) Read in your CSV / build ‘highlight’ + ‘campaigns’ exactly as before
# ——————————————————————————————————————————————

setwd(
  file.path(
    '~','Library','CloudStorage',
    'GoogleDrive-mbernstein@g.harvard.edu/My Drive/Summer 2025',
    'rttc','Pam Maps'
  )
)

d <- fread(file.path('data','state-level campaign.csv'))

# Which states to highlight?  (make lowercase)
highlight_states <- tolower(unique(d$`State(s)`))

# Build a little lookup table (“camp”) where each state has a pasted‐together Campaign string
camp <- d[, .(campaign = paste(Campaign, collapse = ",\n")), by = .(`State(s)`)]
camp[, region_lower := tolower(`State(s)`)]


# ——————————————————————————————————————————————
# (B) Load usmap *as an sf* object, then join your “camp” lookup onto it
# ——————————————————————————————————————————————

# NOTE: us_map(regions="states") returned an sf‐style data.table with a column named “geom”
state_map <- us_map(regions = "states")   # returns 51 rows (50 states + DC) as an sf‐tibble

# Convert to data.table so we can join
setDT(state_map)

# Create a lowercase version of the full state name (the “full” column is e.g. “Alabama”, “Alaska”, etc.)
state_map[, region_lower := tolower(full)]

# Flag which states are in your highlight list
state_map[, highlight := region_lower %in% highlight_states]

# Join the pasted‐together Campaign string by lowercase state name
# (if a state has no campaigns, “campaigns” will be NA)
state_map <- merge(
  state_map,
  camp[, .(region_lower, campaigns = campaign)],
  by = "region_lower",
  all.x = TRUE
)

# ——————————————————————————————————————————————
# (B) BEFORE touching state_map again, make a copy for the state‐campaign plot
# ——————————————————————————————————————————————

state_map_for_campaigns <- copy(state_map)


# ——————————————————————————————————————————————
# (C) Build state_plot from state_map_for_campaigns (so it never breaks later)
# ——————————————————————————————————————————————

p <- ggplot(state_map_for_campaigns) +
  geom_sf(
    aes(
      geometry = geom,
      fill     = highlight,
      text     = campaigns
    ),
    color = "black"
  ) +
  scale_fill_manual(values = c("white", "steelblue")) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "States with state‐level powerbuilding by a hub or member organization"
  )

g <- ggplotly(p, tooltip = "text") %>% layout(width = 800, height = 500)
# Remove polygon hover
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

g
htmlwidgets::saveWidget(g, "state_campaigns.html")
state_plot <- g

####Hubs####
# ——————————————————————————————————————————————
# 1. Read hubs.csv and split “States” into one‐row‐per‐State
# ——————————————————————————————————————————————

h <- fread(file.path('data','hubs.csv'))
h <- h[, .(Hub, States)]
# Keep only non‐empty “States” rows
h_clean <- h[nzchar(States)]

# Split “States” (e.g. "NY,PA,NJ") into separate rows; one (Hub, State) per line
eh <- unique(
  h_clean[, .(State = unlist(strsplit(States, ","))), by = Hub]
)
# Lowercase for joining
eh[, region_lower := tolower(State)]


# ——————————————————————————————————————————————
# 2. Join the “hub” column onto state_map by region_lower
# ——————————————————————————————————————————————

# (a) Make sure any old ‘hub’ column is removed:
state_map[, hub := NULL]

# (b) Perform a left‐join: for each row in state_map, pick x.Hub from eh by region_lower
state_map[, hub := eh[.SD, on = .(region_lower), x.Hub]]


# ——————————————————————————————————————————————
# 3. Split into “with_hub” vs “without_hub”
# ——————————————————————————————————————————————

states_with_hub    <- state_map[!is.na(hub)]
states_without_hub <- state_map[ is.na(hub)]


# ——————————————————————————————————————————————
# 4. Plot two layers of geom_sf(): no‐hub (white) beneath, hub‐filled on top
# ——————————————————————————————————————————————

p_hubs <- ggplot() +
  # Layer 1: all states that have no hub → fill white, no tooltip
  geom_sf(
    data = states_without_hub,
    aes(geometry = geom),
    fill = "white",
    color = "black"
  ) +
  # Layer 2: all states that DO have a hub → fill by hub, tooltip = hub
  geom_sf(
    data = states_with_hub,
    aes(
      geometry = geom, 
      fill     = hub,
      text     = hub
    ),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Hubs")

#p_hubs

# Make interactive
g <- ggplotly(p_hubs, tooltip = "text") %>%
  layout(width = 800, height = 500)

# Remove any remaining default tooltip on the map background
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

htmlwidgets::saveWidget(g, "hubs.html")
hub_plot <- g

####City-level####
# ——————————————————————————————————————————————
# 1. Read city‐level CSV and build 'tooltip'; make a 'lon' column
# ——————————————————————————————————————————————

dt <- fread(file.path("data","city-level powerbuilding.csv"))
setnames(dt, c("campaign","org","scale","lat","long"))

# Fix the one row with missing coords (row 32 → New York City)
dt[32, c("lat","long") := .(40.7127753, -74.0059728)]

# Ensure lat/long are numeric
dt[, c("lat","long") := .(as.numeric(lat), as.numeric(long))]

# Build a custom tooltip string
dt[, tooltip := paste0(
  campaign, "\n",
  "Organization(s): ", org
)]

# Copy 'long' → 'lon' so usmap_transform() will find it
dt[, lon := long]


# ——————————————————————————————————————————————
# 2. Call usmap_transform() (returns a data.table with a geometry column)
# ——————————————————————————————————————————————

dt_trans <- usmap_transform(
  dt,
  input_names = c("lon", "lat")
)
# dt_trans is now a data.table with a column named "geometry" (an sfc_POINT)

# ——————————————————————————————————————————————
# 3. Extract numeric (x,y) from the geometry list‐column
# ——————————————————————————————————————————————

# st_coordinates applied to the geometry column itself:
coords <- st_coordinates(dt_trans$geometry)
setDT(dt_trans)
dt_trans[, `:=`(x = coords[,1], y = coords[,2])]


# ——————————————————————————————————————————————
# 4. Plot: draw all 50 states with geom_sf(), then overlay dt_trans points
# ——————————————————————————————————————————————

# (Assumes `state_map` already exists: a data.table with columns:
#   region_lower, full, abbr, fips, geom (sfc_MULTIPOLYGON), highlight,
#   campaigns, hub, etc.)

p_city <- ggplot() +
  # (a) Base map: all 50 states (AK & HI inset) from state_map$geom
  geom_sf(
    data = state_map,
    aes(geometry = geom),
    fill = "white",
    color = "gray"
  ) +
  # (b) Overlay red points at (x,y) with hover‐text = tooltip
  geom_point(
    data = dt_trans,
    aes(x = x, y = y, text = tooltip),
    color = "red",
    size = 2
  ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "City‐level powerbuilding campaigns by member organizations"
  )

#p_city

# Convert to interactive plot
# g <- ggplotly(p, tooltip = "text")
g <- ggplotly(p_city, tooltip = "text") %>% layout(width = 800, height = 500)

# Remove polygon hover
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

# Save or display
htmlwidgets::saveWidget(g, "city_campaigns.html")
#g
city_plot <- g

####C4s####
# ——————————————————————————————————————————————
# 1. Read c4.csv and rename columns
# ——————————————————————————————————————————————

c4 <- fread(file.path("data","c4s.csv"))
setnames(c4, c("c4","c3","att","lat","long"))

# Copy long → lon so that usmap_transform() can find it
c4[, lon := long]


# ——————————————————————————————————————————————
# 2. Transform (lon, lat) → projected POINT geometry
# ——————————————————————————————————————————————

c4_trans <- usmap_transform(
  c4,
  input_names = c("lon", "lat")
)
# This returns an sf‐frame (with class “data.table” + “sf”) that has:
#   columns: c4, c3, att, lat, long, lon, plus “geometry” (an sfc_POINT)

setDT(c4_trans)  # for consistency


# ——————————————————————————————————————————————
# 3. Extract numeric (x,y) from c4_trans$geometry
# ——————————————————————————————————————————————

coords_c4 <- st_coordinates(c4_trans$geometry)
c4_trans[, `:=`(x = coords_c4[,1], y = coords_c4[,2])]

# Verify:
# str(c4_trans)
# should show columns x: num and y: num


# ——————————————————————————————————————————————
# 4. Plot: all 50 states via geom_sf(), then dark-green C4 points
# ——————————————————————————————————————————————

p_c4 <- ggplot() +
  # (a) Base map: draw every state (AK & HI inset)
  geom_sf(
    data = state_map,
    aes(geometry = geom),
    fill = "white",
    color = "gray"
  ) +
  # (b) Overlay dark-green points at (x,y), hover‐text = c4
  geom_point(
    data = c4_trans,
    aes(x = x, y = y, text = c4),
    color = "darkgreen",
    size = 2
  ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "C4 Organizations"
  )

#p_c4
# Convert to interactive plot
# g <- ggplotly(p, tooltip = "text")
g <- ggplotly(p_c4, tooltip = "text") %>% layout(width = 800, height = 500)

# Remove polygon hover
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

# Save or display
htmlwidgets::saveWidget(g, "c4s.html")
#g
c4_plot <- g

####dashboard####
# Build the full HTML page
# Assemble all parts in a tagList
dashboard <- tagList(
  tags$h2("Power-Building Campaign Dashboard"),
  
  # Toggle buttons
  tags$div(
    style = "margin-bottom: 1em;",
    tags$button("Hub locations", onclick = "showTab('hub')"),
    tags$button("State-level Campaigns", onclick = "showTab('state')"),
    tags$button("City-level Campaigns", onclick = "showTab('city')"),
    tags$button("C4 Organizations", onclick = "showTab('c4')")
  ),
  
  # Embed the three maps
  tags$div(id = "hub", style = "display: block;", hub_plot),
  tags$div(id = "state", style = "display: none;", state_plot),
  tags$div(id = "city", style = "display: none;", city_plot),
  tags$div(id = "c4", style = "display: none;", c4_plot),
  
  # JavaScript for toggling visibility
  tags$script(HTML("
    function showTab(id) {
      document.getElementById('hub').style.display = 'none';
      document.getElementById('state').style.display = 'none';
      document.getElementById('city').style.display = 'none';
      document.getElementById('c4').style.display = 'none';
      document.getElementById(id).style.display = 'block';
    }
  ")),
  
  tags$style(HTML("
  body { font-family: sans-serif; margin: 20px; }
  button { margin-right: 10px; padding: 6px 12px; font-size: 14px; }
  h2 { margin-bottom: 15px; }
"))
)

# Save the dashboard as HTML
htmltools::save_html(dashboard, file.path("dashboard","campaign_dashboard.html"))