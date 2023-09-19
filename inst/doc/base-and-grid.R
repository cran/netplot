## ----data-sim, warning=FALSE, message=FALSE, fig.width = 6, fig.height = 5----
library(igraph)

# Simulation parameters
set.seed(12)
n <- 100

# Points
x0  <- rnorm(n)
x1  <- x0*.4 + rnorm(n)

# network
net <- sample_smallworld(1, n, 4, .1)

## ----netplot, fig.width = 6, fig.height = 5-----------------------------------
library(netplot)
library(magrittr)

np <- nplot(net, bg.col = "gray70", vertex.nsides = "square") %>%
  set_edge_gpar("line", col = "gray50", alpha = .7) %>%
  set_vertex_gpar("frame", fill = "black", col = "black")

## ----baseplot, fig.width = 6, fig.height = 5----------------------------------
plot(x0, x1)

## ----alltogether, echo=FALSE, fig.width = 6, fig.height = 5-------------------
library(gridBase)
library(grid)

# Base plot and views
plot(x0, x1)
vp <- baseViewports() 

# Adding the plot to the current view
pushViewport(vp$plot)
pushViewport(viewport(.25, .75, width = .5, height = .5))
grid.draw(np)

## ----topright, fig.width = 6, fig.height = 5----------------------------------
plot(x0, x1)
# vp <- baseViewports() 
# pushViewport(vp$figure)
pushViewport(viewport(1, 1, just = c("right", "top"), width = .3, height = .3))
grid.draw(np)

## ----tilted, fig.width = 6, fig.height = 5------------------------------------
plot(x0, x1)
pushViewport(
  viewport(x = .8, y = .8, width = .3, height = .3, angle = -20)
  )
grid.draw(np)

