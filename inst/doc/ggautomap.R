## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(nswgeo)
library(ggautomap)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
head(covid_cases_nsw)

## ----scatter, fig.height = 3, fig.width = 7-----------------------------------
covid_cases_nsw %>%
  ggplot(aes(location = lga)) +
  geom_boundaries(feature_type = "nswgeo.lga") +
  geom_geoscatter(aes(colour = type), sample_type = "random", size = 0.5) +
  coord_automap(feature_type = "nswgeo.lga", xlim = c(147, 153), ylim = c(-33.7, -29)) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  theme_void()

## ----inset, fig.height = 3, fig.width = 7-------------------------------------
covid_cases_nsw %>%
  ggplot(aes(location = lga)) +
  geom_boundaries(feature_type = "nswgeo.lga") +
  geom_geoscatter(aes(colour = type), size = 0.5) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lga", inset = configure_inset(
    centre = "Blacktown", radius = 40, units = "km",
    scale = 7, translation = c(400, -100)
  )) +
  theme_void()

## ----packed, fig.height = 3, fig.width = 7------------------------------------
covid_cases_nsw %>%
  dplyr::filter(year >= 2021) %>%
  ggplot(aes(location = lhd)) +
  geom_boundaries(feature_type = "nswgeo.lhd") +
  geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 35), size = 1) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Sydney", radius = 80, units = "km", feature_type = "nswgeo.lhd",
    scale = 6, translation = c(650, -100)
  )) +
  facet_wrap(vars(year)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(strip.text = element_text(size = 12))

## ----choro-long, fig.height = 3, fig.width = 7--------------------------------
covid_cases_nsw %>%
  ggplot(aes(location = lhd)) +
  geom_choropleth() +
  geom_boundaries(
    feature_type = "nswgeo.lhd", colour = "black", linewidth = 0.1,
    outline.aes = list(colour = NA)
  ) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 5, translation = c(400, -100)
  )) +
  scale_fill_steps(low = "#e6f9ff", high = "#00394d", n.breaks = 5, na.value = "white") +
  theme_void()

## ----choro-wide, fig.height = 3, fig.width = 7--------------------------------
summarised_data <- data.frame(
  lhd = c("Western Sydney", "Sydney", "Far West", "Mid North Coast", "South Western Sydney"),
  cases = c(250, 80, 20, NA, 100)
)

summarised_data %>%
  ggplot(aes(location = lhd)) +
  geom_sf_inset(aes(fill = cases), stat = "automap", colour = NA) +
  geom_boundaries(
    feature_type = "nswgeo.lhd", colour = "black", linewidth = 0.1,
    outline.aes = list(colour = NA)
  ) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 3.5, translation = c(350, 0)
  )) +
  scale_fill_gradient(low = "#e6f9ff", high = "#00394d", na.value = "grey90") +
  theme_void()

## ----text-inset, fig.height = 3, fig.width = 7--------------------------------
covid_cases_nsw %>%
  ggplot(aes(location = lhd)) +
  geom_choropleth() +
  geom_boundaries(feature_type = "nswgeo.lhd") +
  geom_inset_frame() +
  geom_sf_label_inset(aes(label = lhd),
    stat = "automap_coords",
    data = ~ dplyr::slice_head(.x, by = lhd)
  ) +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 3.5, translation = c(350, 0)
  )) +
  labs(x = NULL, y = NULL) +
  theme_void()

## ----text-repel, fig.height = 4, fig.width = 7--------------------------------
library(ggrepel)

# label all features that have data
covid_cases_nsw %>%
  ggplot(aes(location = lhd)) +
  geom_choropleth() +
  geom_boundaries(feature_type = "nswgeo.lhd") +
  geom_inset_frame() +
  geom_label_repel(
    aes(
      x = after_stat(x_inset),
      y = after_stat(y_inset),
      label = lhd
    ),
    stat = "automap_coords",
    nudge_x = 3,
    nudge_y = 1,
    point.padding = NA,
    data = ~ dplyr::slice_head(.x, by = lhd)
  ) +
  scale_fill_distiller(direction = 1) +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 3.5, translation = c(350, 0)
  )) +
  labs(x = NULL, y = NULL) +
  theme_void()

# label all features in the map regardless of data, hiding visually overlapping labels
covid_cases_nsw %>%
  ggplot(aes(location = lhd)) +
  geom_choropleth() +
  geom_boundaries(feature_type = "nswgeo.lhd") +
  geom_inset_frame() +
  geom_label_repel(
    aes(
      x = after_stat(x_inset),
      y = after_stat(y_inset),
      geometry = geometry,
      label = lhd_name
    ),
    stat = "sf_coordinates_inset",
    data = cartographer::map_sf("nswgeo.lhd"),
    point.padding = NA,
    inherit.aes = FALSE
  ) +
  scale_fill_distiller(direction = 1, palette = 2) +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 4, translation = c(500, 0)
  )) +
  labs(x = NULL, y = NULL) +
  theme_void()

