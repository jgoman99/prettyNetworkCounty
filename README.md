# prettyNetworkCounty

Labor day project to learn how to speed up R [Pretty Network County](https://john-friedman.shinyapps.io/prettyNetworkCounty/)

packages:
* igraph
    * converted sf to network for speed
* sf
    * simplifying polygons speeds up rendering massively
* ggplot2
    * rendering color takes a while
* plotly
    * plotly proxy documentation is not great yet - working on this
	* not implemented 9/5/22


# TODO
graph is re-rendering on every update, this is slow.
probable optimal solution is plotly proxy, with equivalent of leaflet add polygon. I can do this in leaflet, but not plotly so far
note: color slows rendering down significantly

# Data
GDP from bea.gov