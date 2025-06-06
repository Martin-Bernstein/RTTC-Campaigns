setwd(file.path('~','Library','CloudStorage','GoogleDrive-mbernstein@g.harvard.edu/My Drive/Summer 2025','rttc','Pam Maps'))
library(ggplot2)
library(maps)
library(plotly)
library(data.table)
library(htmltools)
library(dplyr)

####State-level####
d <- fread(file.path('data','state-level campaign.csv'))

highlight_states <- tolower(unique(d$`State(s)`))

d[,.(`State(s)`,Campaign)]
camp <- d[,.(campaign = paste(Campaign,collapse=',\n')),by=.(`State(s)`)]
camp[,region := tolower(`State(s)`)]

# Load US state map data
us_states <- map_data("state")

# Add a column to flag highlighted states
setDT(us_states)
us_states[,highlight := region %in% highlight_states]
us_states[,campaigns := camp[.SD,on=.(region),x.campaign]]

# Plot the map
p <- ggplot(us_states, aes(long, lat, group = group,text=campaigns)) +
  geom_polygon(aes(fill = highlight), color = "black") +
  scale_fill_manual(values = c("white", "steelblue")) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none")+
  labs(title='States with state-level power-building by a hub or member organization')
# g <- ggplotly(p, tooltip = "text")
g <- ggplotly(p, tooltip = "text") %>% layout(width = 800, height = 500)
# Remove polygon hover
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

g
htmlwidgets::saveWidget(g, "state_campaigns.html")
state_map <- g

####Hubs####
h <- fread(file.path('data','hubs.csv'))
h<-h[,.(Hub,States)]
#One state per row to join to us_states data
h_clean <- h[nzchar(States)]
eh <- unique(h_clean[, .(State = unlist(strsplit(States, ","))), by = Hub])
eh[,region := tolower(State)]
#Which hub is each state a member of?
us_states[,hub:=NULL]
us_states[,hub := eh[.SD,on=.(region),x.Hub]]
us_states[,hover_label:=NULL]
us_states[, hover_label := hub]
# Plot
# Separate polygons into hub vs. no-hub
states_with_hub <- us_states[!is.na(hub)]
states_without_hub <- us_states[is.na(hub)]

# Plot: two separate polygon layers
p <- ggplot() +
  # Layer 1: no tooltips for non-hub states
  geom_polygon(
    data = states_without_hub,
    aes(x = long, y = lat, group = group),
    fill = "white", color = "black"
  ) +
  # Layer 2: tooltips for hub states
  geom_polygon(
    data = states_with_hub,
    aes(x = long, y = lat, group = group, fill = hub, text = hub),
    color = "black"
  ) +
  coord_fixed(1.3) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = 'Hubs')

# Make interactive
g <- ggplotly(p, tooltip = "text") %>%
  layout(width = 800, height = 500)

# Remove any remaining default tooltip on the map background
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

htmlwidgets::saveWidget(g, "hubs.html")
hub_map <- g
us_states[,c('hub','hover_label'):=NULL]

####City-level####

#City-level powerbuilding
dt <- fread(file.path('data','city-level powerbuilding.csv'))
names(dt) <- c('campaign','org','scale','lat','long')
dt[32,c('lat','long') := .(40.7127753,-74.0059728)]
dt[,c('lat','long'):=.(as.numeric(lat),as.numeric(long))]

# Construct a custom tooltip field
dt[, tooltip := paste0(
  campaign, "\n",
  "Organization(s): ", org
)]

# Base ggplot with custom tooltip
p <- ggplot(us_states, aes(long, lat, group = group)) +
  geom_polygon(color = "gray", fill = "white") +
  geom_point(
    data = dt,
    aes(x = long, y = lat, text = tooltip),  # custom tooltip
    inherit.aes = FALSE,
    color = "red",
    size = 2
  ) +
  coord_fixed(1.3) +
  theme_void()+
  theme(legend.position = "none")+
  labs(title='City-level powerbuilding campaigns by member organizations')

# Convert to interactive plot
# g <- ggplotly(p, tooltip = "text")
g <- ggplotly(p, tooltip = "text") %>% layout(width = 800, height = 500)

# Remove polygon hover
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

# Save or display
htmlwidgets::saveWidget(g, "city_campaigns.html")
g
city_map <- g


####C4s####
c4 <- fread(file.path('data','c4s.csv'))
names(c4)<-c('c4','c3','att','lat','long')
# Base ggplot with custom tooltip
p <- ggplot(us_states, aes(long, lat, group = group)) +
  geom_polygon(color = "gray", fill = "white") +
  geom_point(
    data = c4,
    aes(x = long, y = lat, text = c4),  # custom tooltip
    inherit.aes = FALSE,
    color = "darkgreen",
    size = 2
  ) +
  coord_fixed(1.3) +
  theme_void()+
  theme(legend.position = "none")+
  labs(title='C4 Organizations')

# Convert to interactive plot
# g <- ggplotly(p, tooltip = "text")
g <- ggplotly(p, tooltip = "text") %>% layout(width = 800, height = 500)

# Remove polygon hover
g$x$data[[1]]$text <- NULL
g$x$data[[1]]$hoverinfo <- "none"

# Save or display
htmlwidgets::saveWidget(g, "c4s.html")
g
c4_map <- g


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
  tags$div(id = "hub", style = "display: block;", hub_map),
  tags$div(id = "state", style = "display: none;", state_map),
  tags$div(id = "city", style = "display: none;", city_map),
  tags$div(id = "c4", style = "display: none;", c4_map),
  
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