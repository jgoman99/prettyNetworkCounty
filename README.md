# prettyNetworkCounty

Lazy Sunday project to learn how to speed up R

packages:
* igraph
** converted sf to network for speed
* sf
* ggplot2
* plotly


# TODO
graph is re-rendering on every update, this is slow.
probable optimal solution is plotly proxy, with equivalent of leaflet add polygon. I can do this in leaflet, but not plotly so far