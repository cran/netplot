## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----pkgs---------------------------------------------------------------------
library(netplot)
library(igraph)

## ----data---------------------------------------------------------------------
data("UKfaculty", package = "igraphdata")

set.seed(225)
l <- layout_with_fr(UKfaculty)

# A couple of extra attributes to play with. We qualify igraph::degree()
# explicitly because other packages (e.g. sna) also define a degree().
V(UKfaculty)$indeg  <- igraph::degree(UKfaculty, mode = "in")
V(UKfaculty)$is_hub <- V(UKfaculty)$indeg > stats::median(V(UKfaculty)$indeg)

## ----color-factor, fig.cap="Vertices colored by the categorical `Group` attribute, with an automatic legend."----
nplot(UKfaculty, layout = l, vertex.color = ~ Group)

## ----color-numeric, fig.cap="Vertices colored by in-degree (a numeric attribute) using a continuous gradient, with a color-bar legend."----
nplot(UKfaculty, layout = l, vertex.color = ~ indeg)

## ----color-logical, fig.cap="Vertices colored by a logical attribute (hub vs. non-hub)."----
nplot(UKfaculty, layout = l, vertex.color = ~ is_hub)

## ----shape, fig.cap="Vertex color *and* shape mapped from the same `Group` attribute."----
nplot(
  UKfaculty,
  layout        = l,
  vertex.color  = ~ Group,
  vertex.nsides = ~ Group
)

## ----size, fig.cap="Vertex size mapped from in-degree."-----------------------
nplot(
  UKfaculty,
  layout            = l,
  vertex.size       = ~ indeg,
  vertex.size.range = c(.01, .04, 4)
)

## ----combined, fig.cap="Color, shape, and size all mapped from graph attributes in a single call."----
nplot(
  UKfaculty,
  layout            = l,
  vertex.color      = ~ Group,   # color by department
  vertex.nsides     = ~ Group,   # distinct shape per department
  vertex.size       = ~ indeg,   # size by popularity (in-degree)
  vertex.size.range = c(.01, .04, 4)
)

## ----edge-width, fig.cap="Edge width mapped from the numeric `weight` edge attribute."----
nplot(
  UKfaculty,
  layout           = l,
  edge.width       = ~ weight,
  edge.width.range = c(1, 4, 4),
  skip.arrows      = TRUE
)

## ----edge-color, fig.width=7, fig.height=3, fig.cap="Varying the `mix` between `ego` and `alter` in the edge color formula. Left: alter only. Middle: an even blend. Right: ego only."----
gridExtra::grid.arrange(
  nplot(UKfaculty, layout = l, vertex.color = ~ Group,
        edge.color = ~ ego(mix = 0, alpha = .1) + alter(mix = 1)),
  nplot(UKfaculty, layout = l, vertex.color = ~ Group,
        edge.color = ~ ego(mix = .5, alpha = .1) + alter(mix = .5)),
  nplot(UKfaculty, layout = l, vertex.color = ~ Group,
        edge.color = ~ ego(mix = 1, alpha = .1) + alter(mix = 0)),
  ncol = 3
)

## ----set-gpar, fig.cap="Recoloring an existing plot by attribute using `set_vertex_gpar()`."----
np <- nplot(UKfaculty, layout = l)

set_vertex_gpar(np, element = "core", fill = ~ Group)

