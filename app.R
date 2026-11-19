# DineSight - Shiny dashboard
# Five pages:
#   Start              - landing page: "I'm exploring ideas" or "I have a concept in mind"
#   Discover           - for clients still deciding: filters, a ranked station shortlist,
#                        a concept x area heatmap and a gap finder
#   Location overview  - one station at a glance: summary, four indicators, map with a
#                        pin, and nearby restaurants
#   Deep dive          - the evidence: footfall, rent, competition and price charts
#   Break-even         - customers needed per day to cover costs (planning estimate)
# Location overview and Deep dive share the sidebar (concept, station, cuisines, radius).
#
# Run from the project root:  shiny::runApp("app")
# Needs app/app_data.rds, made by R/04_prepare_app_data.R

library(shiny); library(bslib); library(bsicons); library(shinyWidgets); library(leaflet); library(DT); library(plotly)
suppressPackageStartupMessages({ library(dplyr); library(sf); library(glue) })

d <- readRDS("app_data.rds")
`%||%` <- function(a, b) if (is.null(a)) b else a   # "a, or b if a is empty"

# Base map: OneMap Grey (Singapore Land Authority) - free, no API key needed.
# If tiles stay blank, change "/Grey/" to "/Grey_HD/" below.
add_basemap <- function(map) {
  map |> addTiles(
    urlTemplate = "https://www.onemap.gov.sg/maps/tiles/Grey/{z}/{x}/{y}.png",
    attribution = 'OneMap &copy; contributors | <a href="https://www.sla.gov.sg" target="_blank">Singapore Land Authority</a>',
    options = tileOptions(minZoom = 11, maxZoom = 19))
}

# Colours ---------------------------------------------------------------------
ink <- "#1d2a22"; muted <- "#5f675f"; green <- "#2f5d45"
rust <- "#c0603f"; amber <- "#c98a1b"; sand <- "#b8a77a"
level_col <- c(High = "#2f7d55", Medium = "#c98a1b", Low = "#b0503a")
housing_cols <- c("1-3 room HDB" = "#e7b75f", "4-5 room / executive HDB" = "#4c9a7a",
                  "Condo / apartment" = "#6c7fc4", "Landed" = "#a4547c")
housing_pal  <- colorFactor(unname(housing_cols), levels = names(housing_cols), na.color = "#e4e2dc")
heat <- c("#fbf1df", "#f6d9a8", "#eeb27a", "#e0835c", "#c45a43")
density_pal  <- colorBin(c("#fbf1df", heat), domain = d$subzones$density, bins = c(0, 100, 5000, 10000, 20000, 30000, Inf),
                         na.color = "#ece9e2")
income_pal   <- colorBin(heat, domain = c(0, 1), bins = c(0, 0.1, 0.25, 0.5, 0.75, 1.0001), na.color = "#ece9e2")
mrt_pal      <- colorBin(rev(heat), domain = d$subzones$mrt_km, bins = c(0, 0.4, 0.8, 1.2, 2, Inf), na.color = "#ece9e2")

# Lookups ---------------------------------------------------------------------
concept_choices <- setNames(names(d$concepts), vapply(d$concepts, \(x) x$description, ""))
concept_choices_all <- c("All concepts" = "all", concept_choices)   # "all" = no concept chosen
concept_short <- c(affordable_everyday_meal = "affordable everyday dining", premium_cafe = "a premium cafe",
                   family_casual_dining = "family casual dining", local_cuisine = "local cuisine",
                   fast_food_grab_go = "fast food / grab-and-go")

# Stations with at least one place within 1.5 km (n_places = places within 1 km)
stations_with_food <- d$station_places |> group_by(station) |> summarise(n_places = sum(dist_m <= 1000), .groups = "drop")
station_tbl <- d$stations |> st_drop_geometry() |>
  inner_join(stations_with_food, by = "station") |>
  left_join(d$areas |> st_drop_geometry() |> select(PLANNING_AREA, area_name), by = "PLANNING_AREA") |>
  filter(!is.na(area_name)) |>
  distinct(station, .keep_all = TRUE) |>     # Bukit Panjang, Choa Chu Kang, Punggol are MRT + LRT
  arrange(area_name, station)
station_choices <- split(station_tbl$station, station_tbl$area_name)
station_pts <- d$stations |> distinct(station, .keep_all = TRUE)
station_pts_3414 <- st_transform(station_pts, 3414)
rest_pts_3414 <- d$restaurants |> st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> st_transform(3414)

# Cuisines offered as tick boxes (categories with at least 15 places), most common first
cuisine_counts <- sort(table(d$restaurants$category_canon), decreasing = TRUE)
cuis_ch <- setNames(names(cuisine_counts)[cuisine_counts >= 15],
                    tools::toTitleCase(gsub("_", " ", names(cuisine_counts)[cuisine_counts >= 15])))
cuisine_picker <- function(id, label)   # dropdown with a tick box per cuisine, search and select/deselect all
  pickerInput(id, label, choices = cuis_ch, selected = unname(cuis_ch), multiple = TRUE, width = "100%",
              options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, liveSearchPlaceholder = "Search cuisines",
                                      selectAllText = "Select all", deselectAllText = "Deselect all",
                                      selectedTextFormat = "count > 2", countSelectedText = "{0} cuisines",
                                      noneSelectedText = "No cuisines selected", size = 10, style = "btn-cuisine"))

# Which restaurant categories count as each concept (used for prices and competition)
# come from archetypes.json: "categories" = these only, "categories_exclude" = all but these.
in_concept <- function(category, concept_key) {
  if (identical(concept_key, "all")) return(rep(TRUE, length(category)))
  cfg <- d$concepts[[concept_key]]
  if (!is.null(cfg[["categories"]])) category %in% unlist(cfg[["categories"]])
  else !category %in% unlist(cfg[["categories_exclude"]])
}

# Footfall: an average day per station (weekday and weekend weighted by day counts)
# Peak hours: breakfast 6-9am, lunch 11am-2pm, dinner 5-8pm (9 hours).
# Off-peak = other hours trains run, 5:30am-midnight (9.5 hours).
meal_bands <- tibble(band = c("Breakfast", "Lunch", "Dinner"), from = c(6, 11, 17), to = c(9, 14, 20),
                     label = c("6am - 9am", "11am - 2pm", "5pm - 8pm"))
is_peak <- function(h) h %in% c(6:8, 11:13, 17:19)
open_w  <- function(h) case_when(h == 5 ~ 0.5, h >= 6 ~ 1, TRUE ~ 0)
avg_hours <- function(days = "all") {
  h <- d$station_hourly
  if (days != "all") h <- h |> filter(day_type == days)
  h |> group_by(station, hour) |>
    summarise(per_hour = sum(per_day * n_days) / sum(n_days), .groups = "drop")
}
flow_from <- function(a) a |>
  group_by(station) |>
  summarise(peak_hr      = sum(per_hour[is_peak(hour)]) / 9,
            offpeak_hr   = sum(per_hour * pmax(open_w(hour) - is_peak(hour), 0)) / 9.5,
            breakfast_hr = mean(per_hour[hour %in% 6:8]), lunch_hr = mean(per_hour[hour %in% 11:13]),
            dinner_hr    = mean(per_hour[hour %in% 17:19]),
            per_day = sum(per_hour), .groups = "drop") |>
  filter(station %in% station_tbl$station)
avg_day <- avg_hours("all")
station_flow <- flow_from(avg_day)
foot_cut <- quantile(station_flow$peak_hr, c(1/3, 2/3))
rent_universe <- d$station_rent |> distinct(station, .keep_all = TRUE) |> filter(station %in% station_tbl$station)
rent_cut <- quantile(rent_universe$rent_psf, c(1/3, 2/3))
level3 <- function(x, cut) case_when(is.na(x) ~ NA_character_, x > cut[2] ~ "High", x > cut[1] ~ "Medium", TRUE ~ "Low")

rent_pts_3414 <- st_as_sf(d$rent_streets, coords = c("lon", "lat"), crs = 4326) |> st_transform(3414)
sz_median <- list(density = median(d$subzones$density[d$subzones$residents > 0]),
                  high_income = median(d$subzones$high_income_share, na.rm = TRUE),
                  area_high_income = median(distinct(st_drop_geometry(d$subzones), PLANNING_AREA, area_high_income_pct)$area_high_income_pct,
                                            na.rm = TRUE))

label_metric <- function(m) unname(ifelse(m %in% names(metric_names), metric_names[m], m))
fmt_metric <- function(m, v) case_when(
  is.na(v) ~ "n/a",
  m %in% c("meal_taps_hr", "lunch_dinner_taps_hr", "residents_nearby") ~ format(round(v), big.mark = ","),
  m == "rent_psf" ~ sprintf("$%.2f", v),
  m == "concept_outlets_per_1k_taps" ~ sprintf("%.1f", v),
  m == "mean_price_mid" ~ sprintf("$%.0f", v),
  grepl("_ratio$", m) ~ sprintf("%.0f%%", v * 100),
  TRUE ~ as.character(round(v, 1)))
fmt_n <- function(x) format(round(x), big.mark = ",")
title_area <- function(x) tools::toTitleCase(tolower(x))

score_bar <- function(label, note, value_txt, s) {
  col <- if (s >= 0.67) "#2f7d55" else if (s <= 0.33) rust else sand
  div(class = "mb-3",
      div(class = "d-flex justify-content-between small",
          span(strong(label), span(class = "text-muted", note)), span(value_txt)),
      div(style = "height:8px;background:#ebe7de;border-radius:4px;",
          div(style = glue("height:8px;width:{max(4, round(s * 100))}%;background:{col};border-radius:4px;"))))
}

# Station ranking for each concept ---------------------------------------------
# The measures each concept uses are set in data/reference/archetypes.json.
# Per station, using the sidebar radius where it applies:
#   footfall: peak-hour and lunch + dinner footfall per hour, afternoon / weekday / weekend
#     shares (LTA, all days)
#   rent: station rent, $ psf per month (URA)
#   residents and age shares: subzones whose centre is within the radius (SingStat)
#   income: the planning area's GHS 2025 shares; where the survey has no figure, the
#     housing mix within the radius, converted to the survey scale by percentile
#   concept_outlets_per_1k_taps: licensed Core F&B outlets within the radius (SFA)
#     x the concept's share of Google places within the radius, per 1,000 peak-hour
#     footfall per hour (lower = more customers per competitor)
# Ranked: stations with footfall data and 5+ places within the radius (Premium cafe
# also needs 5+ priced places). Rank rule: stations no other station beats on every
# measure (the Pareto front) come first; each group is ordered by the fit score
# (average percentile across the concept's measures), then the lead measure.
min_places <- 5
rest_lite <- d$restaurants |> select(place, category, category_canon, rating, price_mid)
afternoon <- avg_day |> group_by(station) |> summarise(afternoon_ratio = sum(per_hour[hour %in% 14:16]) / sum(per_hour), .groups = "drop")
stn_base <- station_tbl |> select(station, PLANNING_AREA, area_name, area_low_income_ratio,
                                  area_mid_income_ratio, area_high_income_ratio) |>
  left_join(d$station_footfall |> select(station, total_per_day, weekday_ratio, weekend_ratio), by = "station") |>
  left_join(station_flow |> transmute(station, meal_taps_hr = peak_hr,
                                      lunch_dinner_taps_hr = (lunch_hr + dinner_hr) / 2), by = "station") |>
  left_join(afternoon, by = "station") |>
  left_join(rent_universe |> select(station, rent_psf), by = "station")
# Housing shares (0-100% of residents) and survey income shares are on different
# scales, so a housing-based value is converted to the survey value at the same
# percentile: a station in the top 10% on condo / landed share gets the top-10%
# survey "$10k+" share.
to_survey_scale <- function(proxy, survey) {
  ok <- is.finite(proxy)
  if (sum(ok) < 2 || sum(!is.na(survey)) < 2) return(rep(NA_real_, length(proxy)))
  out <- rep(NA_real_, length(proxy))
  out[ok] <- quantile(survey, ecdf(proxy[ok])(proxy[ok]), na.rm = TRUE, names = FALSE, type = 7)
  out
}
station_metrics <- function(radius) {
  pl_raw <- d$station_places |> filter(dist_m <= radius) |> inner_join(rest_lite, by = "place")
  pl <- pl_raw |>
    group_by(station) |>
    summarise(n_places = n(), priced = sum(!is.na(price_mid)),
              mean_price_mid = mean(price_mid, na.rm = TRUE), .groups = "drop")
  shares <- bind_rows(lapply(names(d$concepts), \(k)
    pl_raw |> group_by(station) |> summarise(share = mean(in_concept(category_canon, k)), .groups = "drop") |>
      mutate(key = k)))
  sfa_n <- d$station_sfa |> filter(dist_m <= radius) |> count(station, name = "sfa_outlets")
  pop <- d$station_subzones |> filter(dist_m <= radius) |>
    group_by(station) |>
    summarise(across(c(age_total, children, young_adults, older_residents, n_hdb_small, n_hdb_large, n_condo_landed), sum),
              .groups = "drop")
  met <- stn_base |> left_join(pl, by = "station") |> left_join(pop, by = "station") |> left_join(sfa_n, by = "station") |>
    mutate(across(where(is.double), \(x) ifelse(is.nan(x), NA, x)),
           n_places = coalesce(n_places, 0L), priced = coalesce(priced, 0L), sfa_outlets = coalesce(sfa_outlets, 0L),
           housed = n_hdb_small + n_hdb_large + n_condo_landed,
           income_source = if_else(!is.na(area_mid_income_ratio), "survey", "housing"),
           non_high_income_ratio = if_else(income_source == "survey", area_low_income_ratio + area_mid_income_ratio,
                                           to_survey_scale((n_hdb_small + n_hdb_large) / housed,
                                                           area_low_income_ratio + area_mid_income_ratio)),
           high_income_ratio = if_else(income_source == "survey", area_high_income_ratio,
                                       to_survey_scale(n_condo_landed / housed, area_high_income_ratio)),
           residents_nearby = coalesce(age_total, 0),
           children_ratio = children / age_total, young_adults_ratio = young_adults / age_total,
           older_residents_ratio = older_residents / age_total,
           across(c(non_high_income_ratio, high_income_ratio, children_ratio, young_adults_ratio,
                    older_residents_ratio), \(x) ifelse(is.finite(x), x, NA)),
           eligible = !is.na(meal_taps_hr) & n_places >= min_places)
  # Concept-specific competition: estimated same-concept competitors per 1,000 peak-hour footfall
  for (k in names(d$concepts)) {
    sh <- shares |> filter(key == k)
    met[[paste0("concept_outlets_per_1k_taps__", k)]] <-
      met$sfa_outlets * coalesce(sh$share[match(met$station, sh$station)], 0) / (met$meal_taps_hr / 1000)
    met[[paste0("concept_outlets__", k)]] <- met$sfa_outlets * coalesce(sh$share[match(met$station, sh$station)], 0)
  }
  met
}
concept_pool <- function(met, key) {    # ranked stations for one concept, generic competition column
  pool <- met |> filter(eligible, key != "premium_cafe" | priced >= min_places)
  pool$concept_outlets_per_1k_taps <- pool[[paste0("concept_outlets_per_1k_taps__", key)]]
  pool
}
concept_eligible <- function(m, key) nrow(m) > 0 && isTRUE(m$eligible) && (key != "premium_cafe" || m$priced >= min_places)
pareto_flags <- function(m) {      # TRUE = no other row is >= on every column and > on one
  m <- as.matrix(m); k <- ncol(m)
  vapply(seq_len(nrow(m)), \(i) {
    ge <- rowSums(m >= matrix(m[i, ], nrow(m), k, byrow = TRUE)) == k
    gt <- rowSums(m >  matrix(m[i, ], nrow(m), k, byrow = TRUE)) > 0
    ge[i] <- FALSE
    !any(ge & gt)
  }, TRUE)
}
concept_scores <- function(pool, key) {   # filled, oriented values and percentile scores
  cfg <- d$concepts[[key]]; maxi <- unlist(cfg$maximize); mini <- unlist(cfg$minimize); crit <- c(maxi, mini)
  filled <- pool |> select(all_of(crit)) |> mutate(across(everything(), \(x) coalesce(x, median(x, na.rm = TRUE))))
  oriented <- filled |> mutate(across(all_of(mini), \(x) -x))
  list(crit = crit, mini = mini, filled = filled, oriented = oriented,
       pct = as_tibble(lapply(oriented, percent_rank)))
}
rank_concept <- function(met, key) {
  pool <- concept_pool(met, key)
  sc <- concept_scores(pool, key)
  pool |> mutate(concept_key = key, pareto_efficient = pareto_flags(sc$oriented),
                 fit_score = rowMeans(sc$pct), lead = sc$filled[[d$concepts[[key]]$sort_col]]) |>
    arrange(desc(pareto_efficient), desc(fit_score), desc(lead)) |>
    mutate(rank = row_number(), n_ranked = n()) |>
    select(concept_key, station, PLANNING_AREA, rank, n_ranked, pareto_efficient, fit_score)
}
rank_cache <- new.env()
get_ranks <- function(radius) {   # metrics + ranks for one radius, cached
  k <- as.character(radius)
  if (is.null(rank_cache[[k]])) {
    met <- station_metrics(radius)
    rank_cache[[k]] <- list(metrics = met, ranks = bind_rows(lapply(names(d$concepts), \(key) rank_concept(met, key))))
  }
  rank_cache[[k]]
}
station_profile <- function(concept_key, station, met) {
  pool <- concept_pool(met, concept_key)
  if (!station %in% pool$station) return(NULL)
  sc <- concept_scores(pool, concept_key); i <- which(pool$station == station)
  bind_rows(lapply(sc$crit, \(m) tibble(metric = m, direction = if (m %in% sc$mini) "lower is better" else "higher is better",
                                       value = pool[[m]][i], score = sc$pct[[m]][i])))
}
metric_names <- d$metric_labels

plot_style <- function(p, ...) {
  p |> layout(font = list(family = "Inter, system-ui, sans-serif", color = ink, size = 12),
              paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
              margin = list(l = 60, r = 20, t = 36, b = 50),
              legend = list(orientation = "h", y = -0.2), ...) |>
    config(displayModeBar = FALSE)
}
empty_plot <- function(msg) {
  plot_ly(type = "scatter", mode = "markers") |>
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           annotations = list(text = msg, showarrow = FALSE, font = list(color = muted, size = 14))) |>
    config(displayModeBar = FALSE)
}

# UI --------------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#faf8f3", fg = ink, primary = green, secondary = "#e7e3da",
  base_font = font_link("Inter", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
  heading_font = font_link("Plus Jakarta Sans",
                           href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@600;700;800&display=swap"),
  "border-radius" = "0.75rem", "card-border-color" = "#e7e3da")

css <- "
body { background:#faf8f3; }
body.no-side .bslib-sidebar-layout > .collapse-toggle { display:none !important; }
.navbar { background:#fffdf9 !important; border-bottom:1px solid #e7e3da; }
.navbar-brand { font-family:'Plus Jakarta Sans', sans-serif; font-weight:800; color:#1f4a35 !important; font-size:1.4rem; }
.navbar .nav-link { color:#5f675f; font-weight:500; }
.navbar .nav-link.active { color:#1d2a22 !important; font-weight:700; border-bottom:3px solid #2f5d45; }
.btn-green { background:#2f5d45; color:#fff; border:none; border-radius:10px; padding:12px 18px; font-weight:600; }
.btn-green:hover, .btn-green:focus { background:#244a37; color:#fff; }
.ideal-panel { background:#e6efe8; border:1px solid #c9dccf; border-radius:12px; padding:14px 14px 4px; }
.btn-cuisine { background:#fff; border:1px solid #c9dccf; border-radius:8px; color:#1d2a22; text-align:left; }
.btn-cuisine:hover, .btn-cuisine:focus, .show > .btn-cuisine { background:#fff; border-color:#2f5d45; color:#1d2a22; }
.bootstrap-select .dropdown-menu { border-radius:10px; border-color:#c9dccf; font-size:.92rem; }
.bootstrap-select .bs-actionsbox .btn { background:#e6efe8; border:none; color:#1f4a35; font-weight:600; font-size:.85rem; }
.bootstrap-select .dropdown-item.active, .bootstrap-select .dropdown-item:active { background:#2f5d45; color:#fff; }
.bootstrap-select .bs-searchbox .form-control { border-color:#c9dccf; }
.page-title { font-family:'Plus Jakarta Sans', sans-serif; font-size:2rem; margin:6px 0 6px; }
.page-title b { font-weight:800; } .page-title span { font-weight:500; color:#2b352e; }
.page-sub { color:#5f675f; margin:0 0 14px; }
.suggest { display:flex; gap:14px; align-items:center; background:#e6efe8; border:1px solid #c9dccf;
           border-radius:12px; padding:14px 18px; margin-bottom:14px; }
.icon-dot { width:46px; height:46px; min-width:46px; border-radius:50%; display:flex; align-items:center;
            justify-content:center; font-size:1.25rem; color:#1f4a35; }
.metric { background:#fff; border:1px solid #e7e3da; border-radius:12px; padding:14px 16px; height:100%;
          display:flex; gap:12px; }
.metric .lab { color:#2b352e; font-size:.9rem; font-weight:500; }
.metric .lvl { font-family:'Plus Jakarta Sans', sans-serif; font-weight:800; font-size:1.35rem; line-height:1.25; }
.metric .det { color:#5f675f; font-size:.8rem; }
.metric a { font-size:.8rem; font-weight:600; color:#2f5d45; }
.chart-note { color:#5f675f; font-size:.85rem; margin:0 0 6px; }
.stand-out { background:#e6efe8; border-radius:12px; padding:18px; height:100%; }
.stand-out h5 { font-family:'Plus Jakarta Sans', sans-serif; font-weight:800; }
.hero h1 { font-family:'Plus Jakarta Sans', sans-serif; font-weight:800; font-size:3rem; text-align:center;
           margin-top:28px; letter-spacing:-0.02em; }
.hero p.lead { text-align:center; color:#3c463f; font-size:1.2rem; }
.choice { border-radius:16px; padding:28px; height:100%; }
.choice h3 { font-family:'Plus Jakarta Sans', sans-serif; font-weight:800; margin-top:14px; }
.choice p { color:#3c463f; font-size:1.05rem; min-height:3em; }
.choice-a { background:#f1f4ef; border:1px solid #d8e2da; }
.choice-b { background:#fdf6ea; border:1px solid #f1dfbf; }
.steps { display:flex; justify-content:space-between; align-items:flex-start; max-width:840px; margin:30px auto 10px; }
.step { text-align:center; flex:1; position:relative; }
.step .num { width:38px; height:38px; border-radius:50%; background:#e6efe8; display:inline-flex; align-items:center;
             justify-content:center; font-weight:700; }
.step:not(:last-child)::after { content:''; position:absolute; top:19px; left:62%; width:76%; border-top:1px solid #d6d2c8; }
.fineprint { text-align:center; color:#7a8079; font-size:.85rem; margin-top:18px; }
.be-tile { border-radius:12px; padding:14px; text-align:center; height:100%; }
.be-tile .big { font-family:'Plus Jakarta Sans', sans-serif; font-weight:800; font-size:1.7rem; line-height:1.2; }
.disclaimer { background:#eef4ef; border-radius:10px; padding:10px 16px; color:#3c463f; font-size:.88rem; margin:14px 0; }
.pin-row { display:flex; justify-content:space-between; gap:8px; padding:6px 0; border-bottom:1px solid #f0ece4; font-size:.9rem; }
.pin-row .v { font-weight:600; text-align:right; }
"

metric_card <- function(icon, icon_bg, label, out_level, out_detail, more_id) {
  div(class = "metric",
      div(class = "icon-dot", style = glue("background:{icon_bg};"), bs_icon(icon)),
      div(div(class = "lab", label),
          div(class = "lvl", uiOutput(out_level, inline = TRUE)),
          div(class = "det", textOutput(out_detail, inline = TRUE)),
          actionLink(more_id, "Read more")))
}
label_icon <- function(icon, text) span(bs_icon(icon, class = "me-1"), text)

sidebar_ui <- sidebar(
  id = "side", open = "closed", width = 300,
  div(class = "ideal-panel",
      selectInput("concept", label_icon("shop", "Concept"), concept_choices_all, selected = concept_choices[[1]]),
      selectizeInput("station", label_icon("train-front", "MRT (location)"), station_choices,
                     selected = if ("Tampines" %in% station_tbl$station) "Tampines" else station_tbl$station[1]),
      cuisine_picker("cuis", label_icon("cup-hot", "Cuisine")),
      sliderInput("max_dist", label_icon("bullseye", "Catchment radius (metres)"), 200, 1500, 1000, step = 100))
)

# --- Start ----------------------------------------------------------------------
start_page <- nav_panel(
  "Start", value = "start", icon = bs_icon("house"),
  div(class = "hero container", style = "max-width:1000px;",
      h1("Where should your restaurant open?"),
      p(class = "lead", "Compare Singapore locations for your concept before committing to a lease."),
      layout_columns(col_widths = c(6, 6), class = "mt-4",
        div(class = "choice choice-a",
            div(class = "icon-dot", style = "background:#dfe9e1;width:64px;height:64px;font-size:1.6rem;", bs_icon("search")),
            h3("I'm exploring ideas"),
            p("Find promising concept and location combinations."),
            actionButton("start_explore", HTML("Discover &nbsp;&rarr;"), class = "btn-green w-100")),
        div(class = "choice choice-b",
            div(class = "icon-dot", style = "background:#fbe6c4;width:64px;height:64px;font-size:1.6rem;color:#7a4b12;",
                bs_icon("file-earmark-text")),
            h3("I have a concept in mind"),
            p("Compare areas against my business priorities."),
            actionButton("start_validate", HTML("Validate my plan &nbsp;&rarr;"), class = "btn-green w-100"))),
      div(class = "steps",
          div(class = "step", div(class = "num", "1"), div(class = "mt-2", "Choose a concept")),
          div(class = "step", div(class = "num", "2"), div(class = "mt-2", "Compare locations")),
          div(class = "step", div(class = "num", "3"), div(class = "mt-2", "Estimate break-even"))),
      p(class = "fineprint", "Early-stage decision support; verify premises and lease details before committing."))
)

# --- Discover ---------------------------------------------------------------------
disc_stations <- station_tbl |>
  inner_join(d$station_footfall |> select(station, peak_per_day), by = "station") |>
  inner_join(d$station_rent |> distinct(station, .keep_all = TRUE) |> select(station, rent_psf), by = "station") |>
  filter(!is.na(region)) |>
  left_join(st_coordinates(station_pts) |> as_tibble() |> mutate(station = station_pts$station) |>
              rename(lon = X, lat = Y), by = "station")
rent_rng   <- range(disc_stations$rent_psf)
rent_lim   <- c(floor(rent_rng[1] * 2) / 2, ceiling(rent_rng[2] * 2) / 2)
region_ch  <- sort(unique(disc_stations$region))
area_ch    <- sort(unique(disc_stations$area_name))
gap_skip   <- c("restaurant", "mall", "unknown", "hotel", "transport")
disc_radius <- 1000

discover_page <- nav_panel(
  "Discover", value = "discover", icon = bs_icon("compass"),
  div(class = "container-fluid",
  h2(class = "page-title", tags$b("Discover"), span(" | find a spot when you're still deciding")),
  p(class = "page-sub", "Leave anything at 'all' if you're not sure, or tell us what matters and we'll rank every station."),
  layout_columns(
    col_widths = c(4, 8),
    card(card_header("What matters to you?"),
      div(class = "ideal-panel",
        selectInput("d_concept", "F&B concept", concept_choices_all, selected = "all"),
        sliderInput("d_rent", sprintf("Budget: rent ($ psf per month; stations range from $%.2f to $%.2f)", rent_rng[1], rent_rng[2]),
                    rent_lim[1], rent_lim[2], rent_lim, step = 0.5, pre = "$"),
        checkboxGroupInput("d_region", "Location: regions", region_ch, selected = region_ch, inline = TRUE),
        selectizeInput("d_areas", "Location: only these planning areas (optional)", area_ch, multiple = TRUE,
                       options = list(placeholder = "All areas in the ticked regions")),
        sliderInput("d_price", "Your price point ($ per person; 60 = 60+)", 0, 60, c(0, 60), step = 1, pre = "$"),
        radioButtons("d_comp", "Competitiveness",
                     c("No preference" = "none", "Fewer competitors" = "fewer", "Busy food scene (proven demand)" = "busy"))),
      div(class = "mt-3",
        h6(class = "fw-bold", "How important is each factor? (0 = ignore, 5 = very)"),
        sliderInput("w_foot",  "MRT activity at peak hours", 0, 5, 3, step = 1),
        sliderInput("w_rent",  "Low rent", 0, 5, 3, step = 1),
        sliderInput("w_price", "Price match (nearby places charge your price)", 0, 5, 3, step = 1),
        sliderInput("w_comp",  "Competition (as chosen above)", 0, 5, 3, step = 1))),
    div(
      card(card_header(textOutput("disc_count", inline = TRUE)), leafletOutput("disc_map", height = 380)),
      card(card_header("Top 10 matching stations"),
           p(class = "chart-note", glue("Match score 0-100 = weighted average of the factor scores. Rivals and Avg price count places within ",
                                        "{disc_radius} m that match your concept; Rent is $ psf per month. ",
                                        "Explore opens the station in the Location overview with the same concept and its usual cuisines.")),
           p(class = "chart-note", HTML(paste0("Breakdown: ",
             "<span style='color:#2f5d45'>&#9632;</span> MRT activity ",
             "<span style='color:#c9a35a'>&#9632;</span> rent ",
             "<span style='color:#6c7fc4'>&#9632;</span> price match ",
             "<span style='color:#c0603f'>&#9632;</span> competition"))),
           DTOutput("disc_table")))
  ),
  card(id = "heat_card", full_screen = TRUE, card_header("What fits where: best station in each planning area for each concept"),
       p(class = "chart-note", "Each cell = the best-ranked station in that planning area for the concept (1 = best of all ranked stations, within 1 km). Follows your location filter. Click a cell to open that station in the Location overview."),
       plotlyOutput("disc_heat", height = 620)),
  card(id = "gap_card", card_header("Gap finder: busy stations with few places of a cuisine"),
       p(class = "chart-note", glue("For each matching station, the most under-served of Singapore's 12 most common cuisines ",
                                    "(among those that fit your concept): peak-hour MRT footfall per day divided by (places of that cuisine ",
                                    "within {disc_radius} m + 1). Higher = more people per existing place. ",
                                    "Restaurant data is a sample, so treat gaps as leads to check.")),
       DTOutput("disc_gaps")))
)

# --- Location overview -------------------------------------------------------------
overview_page <- nav_panel(
  "Location overview", value = "overview", icon = bs_icon("geo-alt"),
  div(class = "container-fluid",
  h2(class = "page-title", uiOutput("ov_title", inline = TRUE)),
  div(class = "suggest",
      div(class = "icon-dot", style = "background:#cfe2d5;", bs_icon("lightbulb")),
      div(strong("What this suggests"), div(uiOutput("ov_summary")))),
  layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
    metric_card("people-fill", "#e6efe8", "MRT activity", "lvl_foot", "det_foot", "more_foot"),
    metric_card("coin", "#fbe6c4", "Rent", "lvl_rent", "det_rent", "more_rent"),
    metric_card("shop-window", "#e6efe8", "Direct competition", "lvl_comp", "det_comp", "more_comp"),
    metric_card("cup-hot", "#e6efe8", "Nearby meal price", "lvl_price", "det_price", "more_price")),
  layout_columns(col_widths = c(8, 4), class = "mt-3",
    card(full_screen = TRUE, card_header("Heatmap of population density, income, housing and MRT access"),
         leafletOutput("map", height = 470),
         p(class = "chart-note mt-2", "Switch layers at the top left. Click anywhere to drop a pin on a potential site.")),
    card(card_header(span(bs_icon("pin-map-fill", class = "me-1"), "Drop a pin")), uiOutput("pin_panel"))),
  card(class = "mt-3", height = 640, full_screen = TRUE,
       card_header(textOutput("rest_tab_title", inline = TRUE)), DTOutput("restaurants")),
  div(class = "text-end my-3", actionButton("to_deep", HTML("Explore the evidence in Deep dive &nbsp;&rarr;"), class = "btn-green")))
)

# --- Deep dive ----------------------------------------------------------------------
chart_card <- function(title, note, output) card(full_screen = TRUE, card_header(title), p(class = "chart-note", note), output)

deep_page <- nav_panel(
  "Deep dive", value = "deep", icon = bs_icon("bar-chart-line"),
  div(class = "container-fluid",
  h2(class = "page-title", uiOutput("deep_title", inline = TRUE)),
  navset_pill(
    id = "deep_tab",
    nav_panel("Footfall", value = "footfall",
      div(class = "mt-3",
      card(card_header(uiOutput("foot_title", inline = TRUE)),
           p(class = "chart-note", "Station tap-ins and tap-outs, July to September 2026: an activity proxy, not pedestrian counts at a storefront."),
           radioButtons("foot_days", NULL, c("All days" = "all", "Weekdays" = "Weekday", "Weekends / holidays" = "Weekend / holiday"),
                        inline = TRUE),
           layout_columns(col_widths = c(9, 3),
             plotlyOutput("chart_hourly", height = 420),
             div(class = "stand-out", div(class = "icon-dot mb-2", style = "background:#cfe2d5;", bs_icon("bar-chart-fill")),
                 h5("What stands out"), uiOutput("foot_summary")))))),
    nav_panel("Rent", value = "rent",
      div(class = "mt-3",
      layout_columns(col_widths = c(7, 5),
        chart_card("Retail rent near this station",
                   "URA retail rent by street: average of quarterly medians, 2023 Q2 to 2026 Q2. Circle = 1 km around the station.",
                   leafletOutput("rent_map", height = 380)),
        chart_card("How does rent compare?",
                   "Rent near every MRT/LRT station ($ psf per month). The marked line is your station.",
                   plotlyOutput("chart_rent_dist", height = 380))),
      chart_card("How has rent changed?",
                 paste("Median rent of the same streets behind this station's rent figure, each quarter from 2023 Q2 to 2026 Q2.",
                       "Not every street has a deal every quarter, so jumps can come from which streets reported that quarter (hover to see how many)."),
                 plotlyOutput("chart_rent_trend", height = 320)))),
    nav_panel("Competition", value = "competition",
      div(class = "mt-3",
      layout_columns(col_widths = c(7, 5),
        card(full_screen = TRUE, card_header("How does this station compare for your concept?"), uiOutput("fit_panel")),
        chart_card("What is already nearby?",
                   "Places within your catchment radius, by cuisine (all cuisines). Your ticked cuisines are highlighted.",
                   plotlyOutput("chart_cuisine", height = 420))))),
    nav_panel("Price", value = "price",
      div(class = "mt-3",
      layout_columns(col_widths = c(5, 7),
        chart_card("Price range for your concept",
                   "Price per person of places matching your concept and cuisines: near the station, in the planning area, and across Singapore.",
                   plotlyOutput("chart_price_box", height = 400)),
        chart_card("Price vs rating nearby",
                   "Each dot is a nearby place matching your concept and cuisines; bigger = more reviews. Top-left = cheap and well rated: the toughest competitors. Click a dot to open it in Google Maps.",
                   plotlyOutput("chart_price_rating", height = 400)))))
  ),
  div(class = "text-end my-3", actionButton("to_be", HTML("Continue to break-even estimate &nbsp;&rarr;"), class = "btn-green")))
)

# --- Break-even ------------------------------------------------------------------------
be_input <- function(id, label, value, unit, tip, step = 1)
  div(class = "d-flex align-items-center justify-content-between mb-2 gap-2",
      span(label, tooltip(bs_icon("info-circle", class = "ms-1 text-muted"), tip)),
      div(class = "input-group", style = "max-width:190px;",
          tags$input(id = id, type = "number", class = "form-control", value = value, step = step, min = 0),
          span(class = "input-group-text", unit)))
heading4 <- function(txt) h4(class = "mb-0", style = "font-family:'Plus Jakarta Sans';font-weight:800;", txt)

breakeven_page <- nav_panel(
  "Break-even", value = "breakeven", icon = bs_icon("calculator"),
  div(class = "container-fluid",
  p(class = "page-sub mt-2", uiOutput("be_context", inline = TRUE), " \u00b7 ",
    actionLink("be_change", "Change concept or location")),
  layout_columns(col_widths = c(5, 7),
    card(card_header(heading4("Your assumptions")),
         p(class = "chart-note", "Edit your key assumptions to see how the break-even estimate changes. Rent and average spend start from this location's data."),
         be_input("be_size", "Unit size", 800, "sq ft", "Floor area of the unit. Used with the rent per sq ft near the station to suggest monthly rent."),
         be_input("be_rent", "Monthly rent", 8000, "SGD", "Pre-filled as rent per sq ft near the station x unit size. Replace it with the actual quote.", 100),
         be_input("be_fixed", "Staff and other fixed costs", 22000, "SGD", "Salaries, utilities, insurance, loan repayments and other costs that don't change with sales.", 500),
         be_input("be_spend", "Average spend per customer", 20, "SGD", "Pre-filled from nearby places matching your concept and cuisines."),
         be_input("be_var", "Variable cost per customer", 7, "SGD", "Ingredients, packaging, card fees and other costs for each customer served."),
         be_input("be_days", "Open days per month", 30, "days", "How many days a month you trade."),
         be_input("be_seats", "Seats", 50, "seats", "Used to show how many times each seat must turn over per day."),
         be_input("be_target", "Target customers per day", 120, "customers", "The number of customers a day you expect or are aiming for.")),
    div(
      card(card_header(heading4("Estimated break-even")),
           p(class = "chart-note", "Based on your assumptions, the customers needed to cover your monthly costs."),
           uiOutput("be_tiles"), uiOutput("be_extra")),
      card(card_header("Break-even customers per day vs monthly fixed costs"),
           p(class = "chart-note", "Higher fixed costs (rent + staff and other) need more customers per day to break even."),
           plotlyOutput("be_chart", height = 300)),
      card(card_header("Before committing"),
           p(class = "chart-note", "Key items to check and validate for this location."),
           checkboxGroupInput("be_check", NULL, choices = c(
             "Confirm actual unit rent and lease terms", "Confirm permitted use and required licences",
             "Check ventilation, utilities and fit-out needs", "Validate customer demand on site"))))),
  div(class = "disclaimer", bs_icon("info-circle", class = "me-1"),
      "Planning estimate only. Not a forecast or investment recommendation. Confirm the actual unit rent, lease conditions, ",
      "licensing and premises suitability before committing."))
)

ui <- page_navbar(
  id = "main_nav", fillable = FALSE,
  title = span(bs_icon("cup-straw", class = "me-1"), "DineSight"),
  theme = theme,
  header = tags$head(tags$style(HTML(css)),
    tags$script(HTML("Shiny.addCustomMessageHandler('scrollTo', function(id) {
      setTimeout(function() { var el = document.getElementById(id); if (el) el.scrollIntoView({behavior: 'smooth'}); }, 600); });
    function sideState() { var a = document.querySelector('#main_nav .nav-link.active');
      var v = a ? a.getAttribute('data-value') : 'start';
      document.body.classList.toggle('no-side', ['overview', 'deep', 'breakeven'].indexOf(v) < 0); }
    document.addEventListener('DOMContentLoaded', sideState);
    document.addEventListener('shown.bs.tab', sideState);
    $(document).on('rendered.bs.select loaded.bs.select changed.bs.select', '#cuis', function() {
      var n = $(this).find('option').length, k = ($(this).val() || []).length;
      if (n && k === n) $(this).closest('.bootstrap-select').find('.filter-option-inner-inner').text('All cuisines');
    });"))),
  sidebar = sidebar_ui,
  start_page, discover_page, overview_page, deep_page, breakeven_page
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # Current station; keeps the last valid one while the box is being retyped
  stn <- reactiveVal(isolate(input$station) %||% station_tbl$station[1])
  observeEvent(input$station, if (isTRUE(input$station %in% station_tbl$station)) stn(input$station))

  # Sidebar only on Location overview, Deep dive and Break-even
  side_tabs <- c("overview", "deep", "breakeven")
  toggle_side <- if (exists("toggle_sidebar", asNamespace("bslib"))) bslib::toggle_sidebar else bslib::sidebar_toggle
  observeEvent(input$main_nav, toggle_side("side", open = input$main_nav %in% side_tabs), ignoreInit = TRUE)

  # Navigation buttons
  observeEvent(input$start_explore, { nav_select("main_nav", "discover"); session$sendCustomMessage("scrollTo", "main_nav") })
  observeEvent(input$start_validate, nav_select("main_nav", "overview"))
  observeEvent(input$to_deep, nav_select("main_nav", "deep"))
  observeEvent(input$to_be, nav_select("main_nav", "breakeven"))
  observeEvent(input$be_change, nav_select("main_nav", "overview"))

  concept_cuisines <- function(concept) unname(cuis_ch)[in_concept(unname(cuis_ch), concept)]
  sel_cuis <- reactive(if (length(input$cuis) == length(cuis_ch)) NULL else input$cuis %||% character(0))
  all_concepts <- reactive(identical(input$concept, "all"))

  # Maps on hidden tabs are drawn when first opened; add layers only after that
  map_ready  <- reactiveVal(FALSE); observeEvent(input$map_zoom,      map_ready(TRUE))
  disc_ready <- reactiveVal(FALSE); observeEvent(input$disc_map_zoom, disc_ready(TRUE))

  area <- reactive(station_tbl$PLANNING_AREA[station_tbl$station == stn()][1])
  area_name <- reactive(station_tbl$area_name[station_tbl$station == stn()][1])
  market_type <- reactive(d$features$cluster_label[d$features$PLANNING_AREA == area()][1])
  profile <- reactive(if (all_concepts()) NULL else station_profile(input$concept, stn(), ranks_now()$metrics))
  ranks_now <- reactive(get_ranks(input$max_dist))
  rank_row <- reactive(ranks_now()$ranks |> filter(concept_key == input$concept, station == stn()))
  area_all_ranks <- reactive(ranks_now()$ranks |> filter(station == stn()) |> arrange(rank / n_ranked))
  stn_metric_row <- reactive(ranks_now()$metrics |> filter(station == stn()))
  not_ranked_msg <- reactive({
    m <- stn_metric_row()
    if (!nrow(m) || is.na(m$total_per_day)) glue("{stn()} has no LTA footfall data, so it isn't ranked.")
    else if (m$n_places < min_places) glue("{stn()} has only {m$n_places} places within {input$max_dist} m (at least {min_places} are needed), so it isn't ranked. Try a larger radius.")
    else glue("{stn()} has only {m$priced} priced places within {input$max_dist} m; a premium cafe needs at least {min_places} to compare prices, so it isn't ranked for this concept. Try a larger radius.")
  })
  concept_name <- reactive(if (all_concepts()) "All concepts" else names(concept_choices)[concept_choices == input$concept])

  nearby_all <- reactive(   # every place within the radius of this station (may also count for others)
    d$station_places |> filter(station == stn(), dist_m <= input$max_dist) |>
      inner_join(d$restaurants |> select(-any_of(c("station_dist_m", "nearest_station"))), by = "place") |>
      rename(station_dist_m = dist_m))
  nearby <- reactive({      # places near the station, cuisine filter only
    r <- nearby_all()
    if (!is.null(sel_cuis())) r <- r |> filter(category_canon %in% sel_cuis())
    r |> arrange(desc(rating), desc(reviews))
  })
  nearby_concept <- reactive(nearby() |> filter(in_concept(category_canon, input$concept)))
  concept_places <- reactive({
    r <- d$restaurants |> filter(in_concept(category_canon, input$concept))
    if (!is.null(sel_cuis())) r <- r |> filter(category_canon %in% sel_cuis())
    r
  })

  # Indicator levels --------------------------------------------------------------
  flow_now <- reactive(station_flow |> filter(station == stn()))
  rent_now <- reactive(d$station_rent |> filter(station == stn()) |> slice(1))
  comp_counts <- reactive({   # direct competitors near every station, same filters
    r <- d$restaurants |> filter(in_concept(category_canon, input$concept))
    if (!is.null(sel_cuis())) r <- r |> filter(category_canon %in% sel_cuis())
    pairs <- d$station_places |> filter(dist_m <= input$max_dist, place %in% r$place)
    tibble(station = station_tbl$station) |>
      left_join(pairs |> count(station), by = "station") |>
      mutate(n = coalesce(n, 0L))
  })
  comp_now <- reactive({
    cc <- comp_counts(); n <- cc$n[cc$station == stn()]
    cut <- quantile(cc$n, c(1/3, 2/3))
    list(n = n, cut = cut, level = level3(n, cut))
  })
  foot_level <- reactive(if (nrow(flow_now())) level3(flow_now()$peak_hr, foot_cut) else NA)
  rent_level <- reactive(level3(rent_now()$rent_psf, rent_cut))
  avg_price <- reactive({ r <- nearby_concept(); if (any(!is.na(r$price_mid))) mean(r$price_mid, na.rm = TRUE) else NA })

  lvl_span <- function(level, suffix = "") {
    if (is.na(level)) return(span("No data"))
    span(style = glue("color:{level_col[[level]]};"), paste0(level, suffix))
  }
  output$lvl_foot <- renderUI(lvl_span(foot_level()))
  output$det_foot <- renderText(if (!nrow(flow_now())) "No LTA data for this station" else
    glue("{fmt_n(flow_now()$peak_hr)} footfall per hour (peak hours)"))
  output$lvl_rent <- renderUI(lvl_span(rent_level(), " cost"))
  output$det_rent <- renderText({
    r <- rent_now()
    where <- if (r$n_streets_1km > 0) glue("{r$n_streets_1km} street{if (r$n_streets_1km > 1) 's' else ''}") else glue("nearest street {r$nearest_km} km")
    glue("${sprintf('%.2f', r$rent_psf)} psf/month, {r$rent_confidence} confidence, {where}")
  })
  output$lvl_comp <- renderUI(lvl_span(comp_now()$level))
  output$det_comp <- renderText(glue("{comp_now()$n} matching places within {input$max_dist} m, sample coverage"))
  output$lvl_price <- renderUI(span(style = glue("color:{ink};"), if (is.na(avg_price())) "n/a" else sprintf("$%.0f/person", avg_price())))
  output$det_price <- renderText(glue("{sum(!is.na(nearby_concept()$price_mid))} priced listings"))

  # Read more pop-ups
  observeEvent(input$more_foot, {
    f <- flow_now()
    showModal(modalDialog(title = "How MRT activity is measured", easyClose = TRUE, footer = modalButton("Close"),
      p("We use LTA's tap-in and tap-out counts at the nearest MRT/LRT station, averaged over July to September 2026. ",
        "It shows how many people pass through the station, not how many walk past a particular shopfront."),
      p(strong("Peak hours:"), " breakfast 6-9am, lunch 11am-2pm and dinner 5-8pm (9 hours). ",
        strong("Off-peak:"), " the other hours trains run, 5:30am to midnight (9.5 hours)."),
      p(strong("Level:"), glue(" footfall per hour in peak hours compared with all {nrow(station_flow)} stations. ",
        "High = top third (above {fmt_n(foot_cut[2])}), Medium = middle third, Low = bottom third (below {fmt_n(foot_cut[1])}).")),
      if (nrow(f)) tags$table(class = "table table-sm",
        tags$tr(tags$th(glue("{stn()}: footfall per hour")), tags$th("")),
        tags$tr(tags$td("Breakfast (6-9am)"), tags$td(fmt_n(f$breakfast_hr))),
        tags$tr(tags$td("Lunch (11am-2pm)"), tags$td(fmt_n(f$lunch_hr))),
        tags$tr(tags$td("Dinner (5-8pm)"), tags$td(fmt_n(f$dinner_hr))),
        tags$tr(tags$td("Off-peak"), tags$td(fmt_n(f$offpeak_hr)))),
      p(class = "small text-muted", "Hour-by-hour charts are in Deep dive > Footfall.")))
  })
  observeEvent(input$more_rent, {
    r <- rent_now()
    showModal(modalDialog(title = "How rent is estimated", easyClose = TRUE, footer = modalButton("Close"),
      p("URA publishes median retail rents by street. For each street we average its quarterly medians from 2023 Q2 to 2026 Q2. ",
        "A station's rent is the median of streets within 1 km of it; if there are none, the nearest street is used."),
      p(strong("Level:"), sprintf(" compared with all stations. High cost = top third (above $%.2f psf), Medium = middle third, Low cost = bottom third (below $%.2f psf).",
                                  rent_cut[2], rent_cut[1])),
      p(strong("Confidence:"), " High = 3 or more streets within 1 km, with deals in 6+ quarters on average. ",
        "Medium = fewer streets or fewer quarters. Low = no street within 1 km, so a farther street stands in."),
      p(strong(glue("{stn()}:")), sprintf(" $%.2f psf per month from %s; %s confidence.", r$rent_psf,
        if (r$n_streets_1km > 0) paste(r$n_streets_1km, "street(s) within 1 km") else paste0(r$nearest_street, " (", r$nearest_km, " km away)"),
        r$rent_confidence)),
      p(class = "small text-muted", "Street rents are broad averages: always confirm the actual unit rent with the landlord.")))
  })
  observeEvent(input$more_comp, {
    cn <- comp_now(); rr <- rank_row()
    fit <- if (all_concepts()) " pick a concept to see how this planning area ranks for it."
           else if (nrow(rr)) glue(" separately, {stn()} ranks #{rr$rank} of {rr$n_ranked} stations for {concept_short[[input$concept]]} within {input$max_dist} m. See Deep dive > Competition.")
           else paste0(" ", not_ranked_msg())
    showModal(modalDialog(title = "How direct competition is measured", easyClose = TRUE, footer = modalButton("Close"),
      p("We count every restaurant within your catchment radius of the station that matches ", strong("your concept and ticked cuisines"),
        " (a place near two stations counts for both). For example, with Premium cafe and Japanese, only Japanese places that are cafes, dessert shops or bakeries count."),
      p(strong("Level:"), glue(" compared with the same count around every station. High = top third (more than {round(cn$cut[2])} places), ",
        "Medium = middle third, Low = bottom third ({round(cn$cut[1])} or fewer).")),
      p(strong(glue("{stn()}:")), glue(" {cn$n} matching places within {input$max_dist} m.")),
      p(strong("Concept fit:"), fit),
      p(class = "small text-muted", "Restaurant listings come from a Google Maps sample of about 100 places per area, so counts are relative, not complete.")))
  })
  observeEvent(input$more_price, {
    showModal(modalDialog(title = "How the nearby meal price is calculated", easyClose = TRUE, footer = modalButton("Close"),
      p("The average price per person of places within your catchment radius that match your concept and ticked cuisines."),
      p("Google lists prices as ranges, so we use each range's midpoint ($10-20 becomes $15; $100+ becomes $100). Prices above $200 are ignored as menu-item errors."),
      p(class = "small text-muted", "Price ranges near the station, in the planning area and across Singapore are compared in Deep dive > Price.")))
  })

  # Titles and summary ------------------------------------------------------------
  output$ov_title <- renderUI(tagList(tags$b(stn()), span(" | Location overview")))
  output$deep_title <- renderUI(tagList(tags$b(stn()), span(" | Deep dive")))
  output$ov_summary <- renderUI({
    words <- c(High = "high", Medium = "moderate", Low = "low")
    fl <- foot_level(); rl <- rent_level(); cl <- comp_now()$level; pr <- avg_price(); r <- rank_row()
    lead <- if (all_concepts()) {
      a <- area_all_ranks()
      if (nrow(a)) glue("{stn()} looks strongest for {concept_short[[a$concept_key[1]]]} (#{a$rank[1]} of {a$n_ranked[1]} stations)") else glue("{stn()} has too little data to rank")
    } else if (nrow(r)) {
      verb <- if (r$rank <= r$n_ranked / 3) "may suit" else if (r$rank <= 2 * r$n_ranked / 3) "could work for" else "may be a harder fit for"
      glue("{stn()} {verb} {concept_short[[input$concept]]} (#{r$rank} of {r$n_ranked} stations)")
    } else glue("{stn()} has too little restaurant data nearby to rank for {concept_short[[input$concept]]}")
    parts <- c(if (!is.na(fl)) glue("MRT activity is {words[[fl]]}"),
               glue("rent is {words[[rl]]} ({tolower(rent_now()$rent_confidence)} confidence)"),
               glue("competition is {words[[cl]]}"),
               if (!is.na(pr)) glue("nearby meals average ${round(pr)} per person"))
    HTML(paste0(lead, ": ", paste(parts, collapse = ", "), ". Check unit-level rent and direct competitors before committing."))
  })

  # Overview map: Density / Income / Housing / MRT access + pin ----------------------
  density_legend <- function(p) addLegend(p, "bottomright", layerId = "legend", opacity = 0.9, title = "Residents per km\u00b2",
    colors = density_pal(c(50, 2500, 7500, 15000, 25000, 40000)),
    labels = c("Under 100", "100 - 5,000", "5,000 - 10,000", "10,000 - 20,000", "20,000 - 30,000", "30,000+"))
  output$map <- renderLeaflet({
    start <- st_coordinates(station_pts |> filter(station == isolate(stn())))
    sz <- d$subzones
    leaflet(options = leafletOptions(minZoom = 11)) |>
      addMapPane("outlines", zIndex = 450) |> addMapPane("points", zIndex = 460) |>
      setView(unname(start[1, 1]), unname(start[1, 2]), zoom = 14) |>
      add_basemap() |>
      addPolygons(data = sz, group = "Density", layerId = ~paste0("a|", SUBZONE),
                  fillColor = ~density_pal(density), fillOpacity = 0.65, color = "white", weight = 0.6,
                  label = ~lapply(paste0("<b>", subzone_name, "</b><br>", fmt_n(density), " residents per km\u00b2"), HTML)) |>
      addPolygons(data = sz, group = "Income (housing proxy)", layerId = ~paste0("b|", SUBZONE),
                  fillColor = ~income_pal(high_income_share), fillOpacity = 0.65, color = "white", weight = 0.6,
                  label = ~lapply(paste0("<b>", subzone_name, "</b><br>",
                                         ifelse(is.na(high_income_share), "No residents",
                                                paste0(round(high_income_share * 100), "% in condos / landed homes"))), HTML)) |>
      addPolygons(data = sz, group = "Housing", layerId = ~paste0("c|", SUBZONE),
                  fillColor = ~housing_pal(main_housing), fillOpacity = 0.65, color = "white", weight = 0.6,
                  label = ~lapply(paste0("<b>", subzone_name, "</b><br>", ifelse(is.na(housing_mix), "No residents", housing_mix)), HTML)) |>
      addPolygons(data = sz, group = "MRT access", layerId = ~paste0("d|", SUBZONE),
                  fillColor = ~mrt_pal(mrt_km), fillOpacity = 0.65, color = "white", weight = 0.6,
                  label = ~lapply(paste0("<b>", subzone_name, "</b><br>", mrt_km, " km to ", mrt_station), HTML)) |>
      addLayersControl(baseGroups = c("Density", "Income (housing proxy)", "Housing", "MRT access"),
                       position = "topleft", options = layersControlOptions(collapsed = FALSE)) |>
      density_legend()
  })
  observeEvent(input$map_groups, {   # swap the legend with the layer
    g <- input$map_groups; p <- leafletProxy("map")
    if ("Income (housing proxy)" %in% g)
      p |> addLegend("bottomright", layerId = "legend", opacity = 0.9, title = "In condos / landed (income proxy)",
                     colors = income_pal(c(0.05, 0.2, 0.4, 0.6, 0.9)), labels = c("Under 10%", "10 - 25%", "25 - 50%", "50 - 75%", "75%+"))
    else if ("Housing" %in% g)
      p |> addLegend("bottomright", layerId = "legend", colors = unname(housing_cols), labels = names(housing_cols),
                     title = "Main housing type", opacity = 0.9)
    else if ("MRT access" %in% g)
      p |> addLegend("bottomright", layerId = "legend", opacity = 0.9, title = "Distance to nearest MRT",
                     colors = mrt_pal(c(0.2, 0.6, 1, 1.5, 3)), labels = c("Under 400 m", "400 - 800 m", "0.8 - 1.2 km", "1.2 - 2 km", "2 km+"))
    else density_legend(p)
  }, ignoreInit = TRUE)

  observe({   # station, catchment circle and nearby restaurants
    req(map_ready())
    st <- station_pts |> filter(station == stn()); xy <- st_coordinates(st); r <- nearby()
    leafletProxy("map") |> clearGroup("station") |>
      addCircles(lng = unname(xy[1, 1]), lat = unname(xy[1, 2]), radius = input$max_dist, group = "station",
                 color = green, weight = 2, dashArray = "6", fill = FALSE, options = pathOptions(pane = "outlines")) |>
      addCircleMarkers(data = r, lng = ~lon, lat = ~lat, group = "station", radius = 4, stroke = FALSE,
                       fillColor = ink, fillOpacity = 0.75, options = pathOptions(pane = "points"),
                       label = ~paste0(name, " (", ifelse(is.na(rating), "no rating", paste(rating, "stars")), ")")) |>
      addCircleMarkers(data = st, group = "station", radius = 9, color = ink, weight = 2, fillColor = "#f2c14e",
                       fillOpacity = 1, label = ~paste(station, "MRT"), options = pathOptions(pane = "points"))
  })

  # Drop a pin: clicking the map places a pin and switches to the nearest station
  pin <- reactiveVal(NULL)
  observeEvent(stn(), {
    if (!is.null(pin()) && identical(pin()$station, stn())) return()   # keep the view on a dropped pin
    xy <- st_coordinates(station_pts |> filter(station == stn()))
    if (nrow(xy)) leafletProxy("map") |> setView(unname(xy[1, 1]), unname(xy[1, 2]), zoom = 14)
  }, ignoreInit = TRUE)
  last_click <- reactiveVal(0)
  place_pin <- function(lat, lng) {
    if (as.numeric(Sys.time()) - last_click() < 0.3) return()   # a click on a shape also fires a map click
    last_click(as.numeric(Sys.time()))
    pt <- st_sfc(st_point(c(lng, lat)), crs = 4326); pt_m <- st_transform(pt, 3414)
    cand <- station_pts_3414 |> filter(station %in% station_tbl$station)
    i <- st_nearest_feature(pt_m, cand)
    hit <- suppressMessages(st_intersects(pt, d$subzones, sparse = FALSE))[1, ]
    pin(list(lat = lat, lng = lng, station = cand$station[i],
             mrt_m = round(as.numeric(st_distance(pt_m, cand[i, ]))), subzone = d$subzones[hit, ],
             n300 = sum(as.numeric(st_distance(pt_m, rest_pts_3414)) <= 300)))
    leafletProxy("map") |> clearGroup("pin") |>
      addAwesomeMarkers(lng = lng, lat = lat, group = "pin", label = "Potential site",
                        labelOptions = labelOptions(noHide = TRUE, direction = "right"),
                        icon = awesomeIcons(icon = "map-marker", library = "fa", markerColor = "darkgreen", iconColor = "#ffffff"))
    updateSelectizeInput(session, "station", selected = cand$station[i])
  }
  observeEvent(input$map_shape_click, place_pin(input$map_shape_click$lat, input$map_shape_click$lng))
  observeEvent(input$map_click, place_pin(input$map_click$lat, input$map_click$lng))
  observeEvent(input$pin_clear, { pin(NULL); leafletProxy("map") |> clearGroup("pin") })

  output$pin_panel <- renderUI({
    p0 <- pin()
    if (is.null(p0)) return(div(
      div(class = "icon-dot my-2", style = "background:#e6efe8;", bs_icon("hand-index")),
      p(strong("Drop a pin on the map to investigate your potential store location.")),
      tags$ol(class = "small",
        tags$li("Choose a layer at the top left of the map: Density, Income (housing proxy), Housing or MRT access."),
        tags$li("Click the spot you're considering. A ", strong("Potential site"), " pin appears."),
        tags$li("This panel compares that spot with a typical Singapore neighbourhood, and the whole page switches to the nearest MRT station.")),
      p(class = "small text-muted", "Tip: switch layers with the pin in place to see the same spot on each measure.")))
    sz <- p0$subzone
    row <- function(k, v, note = NULL) div(class = "pin-row", span(k, if (!is.null(note)) span(class = "text-muted small d-block", note)),
                                           span(class = "v", v))
    has_res <- nrow(sz) > 0 && sz$residents[1] > 0
    tagList(
      p(class = "mb-1", strong("Potential site"), if (nrow(sz)) span(class = "text-muted", glue(" \u00b7 {sz$subzone_name[1]}"))),
      row("Nearest MRT", glue("{p0$station} \u00b7 {fmt_n(p0$mrt_m)} m")),
      row("Population density", if (has_res) glue("{fmt_n(sz$density[1])} /km\u00b2") else "No residents",
          glue("Typical neighbourhood: {fmt_n(sz_median$density)} /km\u00b2")),
      row("Households earning $10k+ a month",
          if (nrow(sz) && !is.na(sz$area_high_income_pct[1])) glue("{round(sz$area_high_income_pct[1])}%") else "No survey data",
          glue("{if (nrow(sz)) tools::toTitleCase(tolower(sz$PLANNING_AREA[1])) else 'This area'} planning area, GHS 2025. ",
               "Typical area: {round(sz_median$area_high_income)}%")),
      row("Condo / landed homes", if (has_res && !is.na(sz$high_income_share[1])) glue("{round(sz$high_income_share[1] * 100)}%") else "n/a",
          glue("Housing-based income proxy for this subzone. Typical: {round(sz_median$high_income * 100)}%")),
      row("Main housing", if (has_res && !is.na(sz$main_housing[1])) as.character(sz$main_housing[1]) else "n/a"),
      if (has_res && !is.na(sz$housing_mix[1])) div(class = "small text-muted mb-1", HTML(sz$housing_mix[1])),
      row("Places within 300 m", p0$n300, "All cuisines, Google sample"),
      div(class = "mt-2", actionLink("pin_clear", "Remove pin")))
  })

  # Restaurants table
  output$rest_tab_title <- renderText(glue("Nearby places: restaurants within {input$max_dist} m of {stn()} ({nrow(nearby())})"))
  output$restaurants <- renderDT({
    r <- nearby() |> transmute(Name = glue('<a href="{url}" target="_blank">{name}</a>'), Category = category,
                               Rating = rating, Reviews = reviews, Price = price, `Distance (m)` = station_dist_m)
    datatable(r, rownames = FALSE, escape = FALSE, options = list(pageLength = 25, order = list(list(2, "desc"))))
  })

  # Deep dive: Footfall ---------------------------------------------------------------
  output$foot_title <- renderUI(heading4(glue("{stn()} MRT activity")))
  days_hours <- reactive(avg_hours(input$foot_days))
  days_flow  <- reactive(flow_from(days_hours()))
  hourly_lines <- reactive({
    per <- days_hours() |>
      filter(station %in% station_flow$station) |>
      left_join(station_pts |> st_drop_geometry() |> select(station, cluster_label), by = "station")
    sg <- per |> group_by(hour) |> summarise(per_hour = mean(per_hour), .groups = "drop") |> mutate(series = "Singapore average")
    mine <- per |> filter(station == stn()) |> transmute(hour, per_hour, series = paste(stn(), "MRT (selected)"))
    mtype <- market_type()
    mt_line <- if (length(mtype) && !is.na(mtype)) per |> filter(cluster_label == mtype) |> group_by(hour) |>
      summarise(per_hour = mean(per_hour), .groups = "drop") |> mutate(series = paste(sub(" /.*", "", mtype), "average")) else NULL
    bind_rows(mine, mt_line, sg) |> filter(hour >= 5)
  })
  output$chart_hourly <- renderPlotly({
    l <- hourly_lines()
    if (!any(grepl("selected", l$series))) return(empty_plot("No LTA footfall data for this station"))
    series <- unique(l$series)
    l$series <- factor(l$series, levels = series)
    cols <- setNames(c(green, amber, "#7c8796")[seq_along(series)], series)
    shapes <- lapply(seq_len(nrow(meal_bands)), \(i) list(type = "rect", xref = "x", yref = "paper",
      x0 = meal_bands$from[i] - 0.5, x1 = meal_bands$to[i] - 0.5, y0 = 0, y1 = 1, layer = "below",
      fillcolor = if (i == 1) "#dfeadf" else "#f6e9cf", opacity = 0.7, line = list(width = 0)))
    ann <- lapply(seq_len(nrow(meal_bands)), \(i) list(x = (meal_bands$from[i] + meal_bands$to[i]) / 2 - 0.5, y = 1, yref = "paper",
      text = glue("<b>{meal_bands$band[i]}</b><br>{meal_bands$label[i]}"), showarrow = FALSE, yanchor = "top",
      font = list(size = 11, color = ink)))
    plot_ly(l, x = ~hour, y = ~per_hour, color = ~series, colors = cols, type = "scatter", mode = "lines+markers",
            line = list(shape = "spline", width = 3), marker = list(size = 6),
            hovertemplate = "%{x}:00  %{y:,.0f} taps<extra>%{fullData.name}</extra>") |>
      plot_style(shapes = shapes, annotations = ann,
                 xaxis = list(title = "", tickvals = seq(6, 24, 2),
                              ticktext = c("6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm", "12am"), range = c(4.6, 23.6)),
                 yaxis = list(title = "Average taps per hour", tickformat = ","),
                 margin = list(l = 70, r = 10, t = 10, b = 40))
  })
  output$foot_summary <- renderUI({
    sf <- days_flow()
    f <- sf |> filter(station == stn())
    if (!nrow(f)) return(p("No LTA data for this station."))
    sg_peak <- mean(sf$peak_hr)
    mtype <- market_type()
    mt_peak <- if (length(mtype) && !is.na(mtype)) {
      mt_st <- station_pts$station[station_pts$cluster_label %in% mtype]
      mean(sf$peak_hr[sf$station %in% mt_st])
    } else NA
    busiest <- c(Breakfast = f$breakfast_hr, Lunch = f$lunch_hr, Dinner = f$dinner_hr)
    ratio <- f$peak_hr / max(f$offpeak_hr, 1)
    cmp <- function(x) if (x >= 1.15) sprintf("%.1f times", x) else if (x <= 0.87) sprintf("%.0f%% of", x * 100) else "about the same as"
    first <- if (ratio >= 1.15) glue("Peak-hour traffic at {stn()} MRT is {sprintf('%.1f', ratio)} times its off-peak level")
             else glue("Traffic at {stn()} MRT is fairly even across peak and off-peak hours")
    mt_txt <- if (!is.na(mt_peak)) glue(" ({cmp(f$peak_hr / mt_peak)} the {sub(' /.*', '', mtype)} average)") else ""
    tagList(
      p(glue("{first}, and {cmp(f$peak_hr / sg_peak)} the Singapore average at peak hours{mt_txt}.")),
      p(glue("Busiest peak hour window: {names(which.max(busiest))} ({fmt_n(max(busiest))} footfall per hour).")),
      p(class = "small text-muted", glue("Peak hours {fmt_n(f$peak_hr)} vs off-peak {fmt_n(f$offpeak_hr)} footfall per hour; ",
                                         "{fmt_n(f$per_day)} footfall per day in total",
                                         if (input$foot_days == "all") "." else glue(" ({tolower(input$foot_days)}s only)."))))
  })

  # Deep dive: Rent -----------------------------------------------------------------
  output$rent_map <- renderLeaflet({
    st <- station_pts |> filter(station == stn()); xy <- st_coordinates(st)
    rp <- d$rent_streets
    pal <- colorNumeric(c("#f4efe4", "#e9c98f", "#d9895a", "#7a2b22"), domain = rp$rent_psf)
    near5 <- rp[order(as.numeric(st_distance(st_transform(st, 3414), rent_pts_3414)))[1:5], ]
    bb <- c(range(c(near5$lon, xy[1, 1])), range(c(near5$lat, xy[1, 2])))
    leaflet(rp) |> add_basemap() |>
      fitBounds(bb[1], bb[3], bb[2], bb[4], options = list(padding = c(40, 40))) |>
      addCircles(lng = unname(xy[1, 1]), lat = unname(xy[1, 2]), radius = 1000, color = ink, weight = 1, dashArray = "4", fillOpacity = 0.03) |>
      addCircleMarkers(~lon, ~lat, radius = 8, color = ink, weight = 1, fillColor = ~pal(rent_psf), fillOpacity = 0.9,
                       label = ~sprintf("%s: $%.2f psf (%d quarter%s; range $%.2f-$%.2f)", street, rent_psf,
                                        as.integer(n_quarters), ifelse(n_quarters == 1, "", "s"), rent_min_psf, rent_max_psf)) |>
      addCircleMarkers(data = st, radius = 8, color = ink, weight = 2, fillColor = "#f2c14e", fillOpacity = 1, label = ~station) |>
      addLegend("bottomleft", pal = pal, values = ~rent_psf, title = "$ psf / month", labFormat = labelFormat(prefix = "$"), opacity = 0.9)
  })
  output$chart_rent_dist <- renderPlotly({
    r <- rent_universe; me <- rent_now()
    pct <- round(mean(r$rent_psf <= me$rent_psf) * 100)
    plot_ly(r, x = ~rent_psf, type = "histogram", nbinsx = 25,
            marker = list(color = "#e3d6bb", line = list(color = "white", width = 1)),
            hovertemplate = "$%{x} psf<br>%{y} stations<extra></extra>") |>
      plot_style(xaxis = list(title = "Rent near station ($ psf per month)", tickprefix = "$", rangemode = "nonnegative"), yaxis = list(title = "Number of stations"),
                 shapes = list(list(type = "line", x0 = me$rent_psf, x1 = me$rent_psf, y0 = 0, y1 = 1, yref = "paper",
                                    line = list(color = rust, width = 3))),
                 annotations = list(list(x = me$rent_psf, y = 1, yref = "paper", yanchor = "bottom", showarrow = FALSE,
                                         text = glue("{stn()}: ${sprintf('%.2f', me$rent_psf)} (higher than {pct}% of stations)"),
                                         font = list(color = rust))))
  })
  output$chart_rent_trend <- renderPlotly({
    t <- d$station_rent_trend |> filter(station == stn()) |> arrange(quarter)
    if (!nrow(t)) return(empty_plot("No quarterly rent data near this station"))
    plot_ly(t, x = ~quarter, y = ~rent_psf, type = "scatter", mode = "lines+markers",
            line = list(color = "#c9a35a", width = 3), marker = list(color = "#c9a35a", size = 8), customdata = ~n_streets,
            hovertemplate = "%{x}: $%{y:.2f} psf<br>from %{customdata} street(s)<extra></extra>") |>
      plot_style(xaxis = list(title = "", type = "category"),
                 yaxis = list(title = "Rent ($ psf per month)", tickprefix = "$", rangemode = "tozero"))
  })

  # Deep dive: Competition (concept fit in plain language) ------------------------------
  output$fit_panel <- renderUI({
    if (all_concepts()) {
      a <- area_all_ranks()
      if (!nrow(a)) return(p(class = "text-muted", not_ranked_msg()))
      a <- a |> mutate(score = 1 - (rank - 1) / (n_ranked - 1))
      return(tagList(
        p(glue("How {stn()} ranks among MRT/LRT stations for each F&B concept (places within {input$max_dist} m), best first. Pick a concept in the sidebar to see why.")),
        lapply(seq_len(nrow(a)), \(i) score_bar(names(concept_choices)[concept_choices == a$concept_key[i]], "",
                                                 glue("#{a$rank[i]} of {a$n_ranked[i]}"), a$score[i]))))
    }
    pr <- profile(); r <- rank_row()
    if (is.null(pr) || !nrow(r)) return(p(class = "text-muted", not_ranked_msg()))
    n_crit <- nrow(pr); m <- stn_metric_row()
    verdict <- if (r$pareto_efficient)
      glue("No other station beats {stn()} on all {n_crit} measures that matter for {concept_short[[input$concept]]}, so it is one of the strongest all-round options.")
      else glue("At least one other station is as good as or better than {stn()} on all {n_crit} measures that matter for {concept_short[[input$concept]]}.")
    uses_income <- any(grepl("income", pr$metric))
    tagList(
      div(class = "suggest", div(class = "icon-dot", style = "background:#cfe2d5;", strong(glue("#{r$rank}"))),
          div(strong(glue("Ranked #{r$rank} of {r$n_ranked} stations")), div(class = "small", verdict))),
      p(class = "small text-muted", glue("Each bar shows where {stn()} sits among the {r$n_ranked} ranked stations on one measure (right = better). ",
                                         "Restaurant and resident measures use everything within {input$max_dist} m.")),
      lapply(seq_len(n_crit), \(i) score_bar(label_metric(pr$metric[i]), paste0(" (", pr$direction[i], ")"),
                                             fmt_metric(pr$metric[i], pr$value[i]), pr$score[i])),
      if (uses_income) p(class = "small text-muted", if (m$income_source == "survey")
        glue("Income: households in the {area_name()} planning area (SingStat GHS 2025).")
        else glue("Income: {area_name()} has no survey figure, so the housing mix within {input$max_dist} m is used (1-3 room HDB = lower, 4-5 room = middle, condo / landed = higher), ",
               "converted to the survey scale by percentile so it compares fairly with other stations.")),
      if ("concept_outlets_per_1k_taps" %in% pr$metric) {
        k <- input$concept; est <- m[[paste0("concept_outlets__", k)]]
        p(class = "small text-muted", glue("Competition: about {fmt_n(est)} same-concept competitors ({concept_short[[k]]}) within {input$max_dist} m ",
          "({fmt_n(m$sfa_outlets)} licensed F&B outlets (SFA) x {round(est / max(m$sfa_outlets, 1) * 100)}% of nearby Google places that fit the concept), ",
          "for {fmt_n(m$meal_taps_hr)} footfall per hour in peak hours."))
      },
      p(class = "small text-muted", glue("How the rank works: stations that no other station beats on every measure come first; ",
        "within each group, stations are ordered by their fit score, the average of the bars above ({round(r$fit_score * 100)} for {stn()}). ",
        "Stations need footfall data and at least {min_places} places within the radius to be ranked.")))
  })
  output$chart_cuisine <- renderPlotly({
    r <- nearby_all()
    if (!nrow(r)) return(empty_plot("No places within this distance"))
    cc <- r |> count(category_canon, sort = TRUE) |> slice_head(n = 15) |>
      mutate(label = tools::toTitleCase(gsub("_", " ", category_canon)), label = factor(label, levels = rev(label)),
             mine = !is.null(sel_cuis()) & category_canon %in% sel_cuis())
    plot_ly(cc, y = ~label, x = ~n, type = "bar", orientation = "h", marker = list(color = ifelse(cc$mine, rust, "#9fbfae")),
            hovertemplate = "%{y}: %{x} places<extra></extra>") |>
      plot_style(xaxis = list(title = "Number of places"), yaxis = list(title = ""), margin = list(l = 110, r = 20, t = 10, b = 50))
  })

  # Deep dive: Price ----------------------------------------------------------------
  output$chart_price_box <- renderPlotly({
    all <- concept_places() |> filter(!is.na(price_mid))
    groups <- bind_rows(nearby_concept() |> filter(!is.na(price_mid)) |> mutate(where = "Near station"),
                        all |> filter(PLANNING_AREA == area()) |> mutate(where = "Planning area"),
                        all |> mutate(where = "Singapore"))
    if (!nrow(groups)) return(empty_plot("No priced places match your concept and cuisines"))
    groups$where <- factor(groups$where, levels = c("Near station", "Planning area", "Singapore"))
    counts <- groups |> count(where)
    q <- groups |> group_by(where) |>
      summarise(q1 = quantile(price_mid, .25), med = median(price_mid), q3 = quantile(price_mid, .75), .groups = "drop")
    plot_ly(groups, x = ~where, y = ~price_mid, type = "box", color = ~where, colors = c(rust, sand, "#9fbfae"),
            boxpoints = "outliers", hoveron = "points", hovertemplate = "Outlier: $%{y:.0f}<extra></extra>") |>
      add_trace(data = q, x = ~where, y = ~med, type = "scatter", mode = "markers", inherit = FALSE, showlegend = FALSE,
                marker = list(size = 40, opacity = 0, color = ink),
                text = ~sprintf("<b>%s</b><br>Q3: $%.0f<br>Median: $%.0f<br>Q1: $%.0f", where, q3, med, q1),
                hovertemplate = "%{text}<extra></extra>") |>
      plot_style(showlegend = FALSE, xaxis = list(title = "", ticktext = paste0(counts$where, "<br>(", counts$n, " places)"),
                                                  tickvals = as.character(counts$where)),
                 yaxis = list(title = "Price per person", tickprefix = "$"))
  })
  output$chart_price_rating <- renderPlotly({
    r <- nearby_concept() |> filter(!is.na(price_mid), !is.na(rating))
    if (!nrow(r)) return(empty_plot("No nearby places with both price and rating match your concept and cuisines"))
    set.seed(1)
    r <- r |> mutate(size = 8 + 14 * sqrt(coalesce(reviews, 0) / max(coalesce(reviews, 0), 1)),
                     price_jit = price_mid * runif(n(), 0.94, 1.06))
    plot_ly(r, x = ~price_jit, y = ~rating, type = "scatter", mode = "markers",
            marker = list(size = ~size, color = green, opacity = 0.65, line = list(color = "white", width = 1)),
            customdata = ~url,
            text = ~paste0("<b>", name, "</b><br>", category, "<br>", price, " | ", rating, " stars | ", fmt_n(reviews), " reviews",
                           "<br><i>Click to open in Google Maps</i>"),
            hovertemplate = "%{text}<extra></extra>") |>
      plot_style(xaxis = list(title = "Price per person (midpoint of Google's range)", tickprefix = "$"), yaxis = list(title = "Google rating"),
                 shapes = list(list(type = "line", x0 = median(r$price_mid), x1 = median(r$price_mid), y0 = 0, y1 = 1, yref = "paper",
                                    line = list(color = muted, dash = "dot", width = 1)),
                               list(type = "line", y0 = median(r$rating), y1 = median(r$rating), x0 = 0, x1 = 1, xref = "paper",
                                    line = list(color = muted, dash = "dot", width = 1)))) |>
      htmlwidgets::onRender("function(el) { el.style.cursor = 'pointer';
        el.on('plotly_click', function(d) { var u = d.points[0].customdata; if (u) window.open(u, '_blank'); }); }")
  })

  # Break-even ------------------------------------------------------------------------
  output$be_context <- renderUI(span(glue("{concept_name()} \u00b7 {stn()} \u00b7 planning estimate")))
  num <- function(x, default = 0) { v <- suppressWarnings(as.numeric(x)); if (length(v) && !is.na(v)) v else default }
  observeEvent(list(stn(), input$be_size), {      # suggested rent = rent psf near the station x unit size
    updateNumericInput(session, "be_rent", value = round(rent_now()$rent_psf * num(input$be_size, 800) / 100) * 100)
  })
  observeEvent(list(stn(), input$concept, input$cuis), {   # suggested spend = nearby meal price
    if (!is.na(avg_price())) updateNumericInput(session, "be_spend", value = round(avg_price()))
  })
  be <- reactive({
    rent <- num(input$be_rent); fixed <- num(input$be_fixed); spend <- num(input$be_spend); var <- num(input$be_var)
    days <- max(num(input$be_days, 30), 1); seats <- max(num(input$be_seats, 1), 1); target <- num(input$be_target)
    margin <- spend - var
    per_month <- if (margin > 0) (rent + fixed) / margin else NA
    list(rent = rent, fixed = fixed, spend = spend, var = var, margin = margin, days = days, seats = seats, target = target,
         per_month = per_month, per_day = per_month / days, sales_day = per_month / days * spend)
  })
  output$be_tiles <- renderUI({
    b <- be()
    if (is.na(b$per_month)) return(div(class = "alert alert-warning",
      "Average spend must be higher than the variable cost per customer, otherwise every customer loses money."))
    ok <- b$target >= b$per_day
    tile <- function(icon, big, small, bg = "#eef4ef") div(class = "be-tile", style = glue("background:{bg};"),
      div(class = "fs-4", bs_icon(icon)), div(class = "big", big), div(small))
    layout_columns(col_widths = c(3, 3, 3, 3),
      tile("people", paste0("~", fmt_n(b$per_month)), "customers / month"),
      tile("calendar3", paste0("~", fmt_n(b$per_day)), "customers / day"),
      tile("cash-stack", paste0("~$", fmt_n(b$sales_day)), "daily sales"),
      tile(if (ok) "check-circle" else "exclamation-triangle",
           span(style = "font-size:1.15rem;", if (ok) "Target covers break-even" else "Target below break-even"),
           glue("target {fmt_n(b$target)} / day"), bg = if (ok) "#e2efe5" else "#fbe6c4"))
  })
  output$be_extra <- renderUI({
    b <- be(); f <- flow_now()
    if (is.na(b$per_month)) return(NULL)
    share <- if (nrow(f)) b$per_day / f$per_day * 100 else NA
    div(class = "small mt-3",
        p(bs_icon("arrow-repeat", class = "me-1"), strong(sprintf("%.1f seat turns per day", b$per_day / b$seats)),
          glue(" needed ({fmt_n(b$per_day)} customers across {fmt_n(b$seats)} seats).")),
        if (!is.na(share)) p(bs_icon("train-front", class = "me-1"), strong(sprintf("%.2f%% of daily station traffic", share)),
          glue(" at {stn()} MRT ({fmt_n(f$per_day)} taps a day) would need to become your customers.")),
        p(class = "text-muted", glue("Monthly costs: ${fmt_n(b$rent)} rent + ${fmt_n(b$fixed)} other = ${fmt_n(b$rent + b$fixed)}. ",
                                     "Each customer contributes ${b$spend} spend - ${b$var} variable cost = ${b$margin}.")))
  })
  output$be_chart <- renderPlotly({
    b <- be()
    if (is.na(b$per_month)) return(empty_plot("Average spend must be above variable cost"))
    tot <- b$rent + b$fixed
    xs <- seq(0, max(40000, tot * 1.5), length.out = 9)
    l <- tibble(fixed = xs, per_day = xs / b$margin / b$days)
    plot_ly(l, x = ~fixed, y = ~per_day, type = "scatter", mode = "lines+markers", line = list(color = green, width = 2),
            marker = list(color = green, size = 6), hovertemplate = "$%{x:,.0f} fixed: %{y:.0f} customers/day<extra></extra>") |>
      add_markers(x = tot, y = b$per_day, inherit = FALSE, marker = list(color = amber, size = 13), hovertemplate = "Your plan<extra></extra>") |>
      plot_style(showlegend = FALSE, xaxis = list(title = "Monthly fixed costs (SGD)", tickformat = ",", tickprefix = "$"),
                 yaxis = list(title = "Customers per day to break even", rangemode = "tozero"),
                 annotations = list(list(x = tot, y = b$per_day, text = glue("Break-even: ~{fmt_n(b$per_day)} / day"),
                                         ax = -70, ay = -35, arrowcolor = amber, font = list(color = amber))))
  })

  # Discover --------------------------------------------------------------------------
  high_best <- function(x) if (sum(!is.na(x)) < 2) rep(1, length(x)) else percent_rank(x)
  low_best  <- function(x) if (sum(!is.na(x)) < 2) rep(1, length(x)) else 1 - percent_rank(x)
  disc <- reactive({
    s <- disc_stations |>
      filter(rent_psf >= input$d_rent[1], rent_psf <= input$d_rent[2], region %in% (input$d_region %||% character(0)))
    if (length(input$d_areas)) s <- s |> filter(area_name %in% input$d_areas)
    if (!nrow(s)) return(s)
    near <- d$station_places |>
      filter(station %in% s$station, dist_m <= disc_radius) |>
      inner_join(d$restaurants |> select(place, category_canon, price_mid), by = "place") |>
      filter(in_concept(category_canon, input$d_concept)) |>
      group_by(station) |>
      summarise(competitors = n(), avg_price = mean(price_mid, na.rm = TRUE), .groups = "drop") |>
      mutate(avg_price = if_else(is.nan(avg_price), NA_real_, avg_price))
    hi <- if (input$d_price[2] >= 60) Inf else input$d_price[2]
    s <- s |> left_join(near, by = "station") |>
      mutate(competitors = coalesce(competitors, 0L),
             price_gap = case_when(is.na(avg_price) ~ NA_real_, avg_price < input$d_price[1] ~ input$d_price[1] - avg_price,
                                   avg_price > hi ~ avg_price - hi, TRUE ~ 0),
             s_foot = high_best(peak_per_day), s_rent = low_best(rent_psf),
             s_price = coalesce(low_best(price_gap), 0.5),
             s_comp = case_when(input$d_comp == "fewer" ~ low_best(competitors), input$d_comp == "busy" ~ high_best(competitors), TRUE ~ 0.5))
    w <- c(foot = input$w_foot, rent = input$w_rent, price = input$w_price, comp = if (input$d_comp == "none") 0 else input$w_comp)
    if (sum(w) == 0) w[] <- 1
    s |> mutate(c_foot = s_foot * w["foot"], c_rent = s_rent * w["rent"], c_price = s_price * w["price"], c_comp = s_comp * w["comp"],
                score = round(100 * (c_foot + c_rent + c_price + c_comp) / sum(w)),
                across(c(c_foot, c_rent, c_price, c_comp), \(x) 100 * x / sum(w))) |>
      arrange(desc(score), desc(peak_per_day)) |> mutate(pos = row_number())
  })
  output$disc_count <- renderText(glue("{nrow(disc())} of {nrow(disc_stations)} stations match your filters"))
  output$disc_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 11)) |> addMapPane("points", zIndex = 460) |> add_basemap() |> setView(103.82, 1.35, zoom = 11)
  })
  observe({
    req(disc_ready()); s <- disc()
    m <- leafletProxy("disc_map") |> clearMarkers() |> clearControls()
    if (!nrow(s)) return()
    pal <- colorNumeric(c("#ebe7de", "#9fbfae", "#2f7d55", "#1f4a35"), domain = c(0, 100))
    top <- s |> filter(pos <= 10)
    m |> addCircleMarkers(data = s, lng = ~lon, lat = ~lat, radius = 6, stroke = TRUE, color = "white", weight = 1,
                          fillColor = ~pal(score), fillOpacity = 0.9, options = pathOptions(pane = "points"),
                          label = ~paste0(station, ": match ", score)) |>
      addLabelOnlyMarkers(data = top, lng = ~lon, lat = ~lat, label = ~as.character(pos),
                          labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = FALSE,
                                                      style = list("font-weight" = "700", "padding" = "1px 5px"))) |>
      addLegend("bottomright", pal = pal, values = c(0, 100), title = "Match score", opacity = 0.9)
  })
  explore_btn <- function(station, cuisine = NA) {
    js <- if (is.na(cuisine)) glue("Shiny.setInputValue('explore', {{station: '{station}', nonce: Math.random()}})")
          else glue("Shiny.setInputValue('explore', {{station: '{station}', cuisine: '{cuisine}', nonce: Math.random()}})")
    as.character(tags$button(class = "btn btn-sm btn-outline-success", onclick = js, "Explore"))
  }
  disc_bar <- function(f, r, p, c) {
    seg <- \(w, col, lab) if (w > 0) glue('<span title="{lab}: {round(w)}" style="display:inline-block;height:10px;width:{w}%;background:{col}"></span>') else ""
    paste0('<div style="width:120px;background:#ebe7de;border-radius:3px;overflow:hidden;line-height:0">',
           seg(f, green, "MRT activity"), seg(r, "#c9a35a", "Rent"), seg(p, "#6c7fc4", "Price"), seg(c, rust, "Competition"), "</div>")
  }
  output$disc_table <- renderDT({
    s <- disc() |> filter(pos <= 10)
    if (!nrow(s)) return(datatable(tibble(Message = "No stations match. Try widening the budget or ticking more regions."),
                                   rownames = FALSE, options = list(dom = "t")))
    t <- s |> transmute(`#` = pos, Station = station, Area = area_name, Match = score,
                        Breakdown = mapply(disc_bar, c_foot, c_rent, c_price, c_comp, USE.NAMES = FALSE),
                        `Peak-hour footfall/day` = fmt_n(peak_per_day), Rent = sprintf("$%.2f", rent_psf), Rivals = competitors,
                        `Avg price` = ifelse(is.na(avg_price), "n/a", sprintf("$%.0f", avg_price)),
                        ` ` = vapply(station, explore_btn, ""))
    datatable(t, rownames = FALSE, escape = FALSE, selection = "none", class = "compact",
              options = list(dom = "t", ordering = FALSE, pageLength = 10, scrollX = TRUE)) |> formatStyle("Match", fontWeight = "bold")
  })
  output$disc_heat <- renderPlotly({
    areas_ok <- unique(disc_stations$PLANNING_AREA[disc_stations$region %in% (input$d_region %||% character(0))])
    if (length(input$d_areas)) areas_ok <- intersect(areas_ok, unique(disc_stations$PLANNING_AREA[disc_stations$area_name %in% input$d_areas]))
    rk <- get_ranks(disc_radius)$ranks
    n_r <- max(rk$n_ranked)
    r <- rk |> filter(PLANNING_AREA %in% areas_ok) |>
      group_by(concept_key, PLANNING_AREA) |> slice_min(rank, n = 1, with_ties = FALSE) |> ungroup()
    if (!nrow(r)) return(empty_plot("No planning areas in your location filter"))
    concept_lab <- c(affordable_everyday_meal = "Affordable<br>everyday", premium_cafe = "Premium<br>cafe",
                     family_casual_dining = "Family<br>casual dining", local_cuisine = "Local<br>cuisine",
                     fast_food_grab_go = "Fast food /<br>grab-and-go")
    order_areas <- r |> group_by(PLANNING_AREA) |> summarise(best = min(rank)) |> arrange(desc(best)) |> pull(PLANNING_AREA)
    r <- r |> mutate(area = factor(title_area(PLANNING_AREA), levels = title_area(order_areas)),
                     concept = factor(concept_lab[concept_key], levels = unname(concept_lab)))
    plot_ly(r, x = ~concept, y = ~area, z = ~rank, type = "heatmap", source = "heat",
            customdata = ~paste(concept_key, station, sep = "|"),
            colorscale = list(c(0, "#2f7d55"), c(0.5, "#f3e7c4"), c(1, "#c0603f")), zmin = 1, zmax = n_r,
            text = ~rank, texttemplate = "%{text}", xgap = 2, ygap = 2,
            hovertext = ~paste0(station, " MRT: #", rank, " of ", n_ranked, " stations"),
            hovertemplate = "%{y}<br>%{x}<br>Best station: %{hovertext}<extra></extra>",
            colorbar = list(title = "Best<br>station<br>rank", tickvals = round(c(1, n_r / 3, 2 * n_r / 3, n_r)))) |>
      plot_style(xaxis = list(title = "", side = "top", tickangle = 0), yaxis = list(title = "", dtick = 1, tickfont = list(size = 11)),
                 margin = list(l = 140, r = 20, t = 50, b = 20)) |>
      event_register("plotly_click")
  })
  observeEvent(suppressWarnings(event_data("plotly_click", source = "heat")), {
    cd <- suppressWarnings(event_data("plotly_click", source = "heat"))$customdata
    req(length(cd) == 1)
    parts <- strsplit(cd, "|", fixed = TRUE)[[1]]
    req(parts[2] %in% station_tbl$station)
    pin(NULL)
    updateSelectInput(session, "concept", selected = parts[1])
    updatePickerInput(session, "cuis", selected = concept_cuisines(parts[1]))
    updateSliderInput(session, "max_dist", value = disc_radius)
    updateSelectizeInput(session, "station", selected = parts[2])
    nav_select("main_nav", "overview")
  })
  output$disc_gaps <- renderDT({
    s <- disc()
    cats <- setdiff(unname(cuis_ch), gap_skip); cats <- cats[in_concept(cats, input$d_concept)]
    if (!nrow(s) || !length(cats)) return(datatable(tibble(Message = "No stations or cuisines to compare."), rownames = FALSE, options = list(dom = "t")))
    common <- d$restaurants |> filter(category_canon %in% cats) |> count(category_canon, sort = TRUE) |>
      slice_head(n = 12) |> mutate(city_share = n / nrow(d$restaurants))
    counts <- d$station_places |>
      filter(station %in% s$station, dist_m <= disc_radius) |>
      inner_join(d$restaurants |> select(place, category_canon), by = "place") |>
      filter(category_canon %in% common$category_canon) |>
      count(station, category_canon, name = "places")
    g <- tidyr::expand_grid(station = s$station, category_canon = common$category_canon) |>
      left_join(counts, by = c("station", "category_canon")) |>
      left_join(common |> select(category_canon, city_share), by = "category_canon") |>
      mutate(places = coalesce(places, 0L)) |>
      left_join(s |> select(station, area_name, peak_per_day), by = "station") |>
      mutate(per_place = peak_per_day / (places + 1)) |>
      group_by(station) |> arrange(desc(per_place), desc(city_share), .by_group = TRUE) |> slice_head(n = 1) |> ungroup() |>
      arrange(desc(per_place)) |> slice_head(n = 10)
    t <- g |> transmute(Station = station, Area = area_name, `Under-served cuisine` = tools::toTitleCase(gsub("_", " ", category_canon)),
                        `Peak-hour footfall/day` = fmt_n(peak_per_day), `Places nearby` = places, `Footfall per place` = fmt_n(per_place),
                        ` ` = mapply(explore_btn, station, category_canon, USE.NAMES = FALSE))
    datatable(t, rownames = FALSE, escape = FALSE, selection = "none", class = "compact",
              options = list(dom = "t", ordering = FALSE, pageLength = 10, scrollX = TRUE))
  })
  observeEvent(input$explore, {   # open the station in the Location overview with Discover's settings
    e <- input$explore
    pin(NULL)
    updateSelectizeInput(session, "station", selected = e$station)
    updateSelectInput(session, "concept", selected = input$d_concept)
    updatePickerInput(session, "cuis", selected = if (!is.null(e$cuisine)) e$cuisine else concept_cuisines(input$d_concept))
    updateSliderInput(session, "max_dist", value = disc_radius)
    nav_select("main_nav", "overview")
  })
}

shinyApp(ui, server)
