
<!-- README.md is generated from README.Rmd. Please edit that file -->
neatmaps
========

[![](https://cranlogs.r-pkg.org/badges/grand-total/neatmaps)](https://CRAN.R-project.org/package=neatmaps) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/neatmaps)](https://CRAN.R-project.org/package=neatmaps)

### Overview

The goal of the neatmaps package is to simplify the exploratory data analysis process for multiple network data sets with the help of hierarchical clustering and heatmaps. Multiple network data consists of multiple disjoint networks that share common graph, node and edge variables. This package contains the necessary tools to convert this raw data into a dynamic report, summarizing the relationships between the graph, node and structural characteristics of the networks.

### Installation

``` r
# To install neatmaps, simply run the following code:
install.packages('neatmaps')
```

### Code Example

``` r
library(neatmaps)

# Create a "cleaned" dataframe using the netsDataFrame function
df <- netsDataFrame(net.attr.df = networkAttrDF,
                    node.attr.df = nodeAttrDF,
                    edge.df = edgeDF)

# Run the neatmap function on the dataframe. This will produce a list
# consisting of the dendrogram of the networks' variables, the significant
# clusters of the bootstrap analysis, the results of the statistical analysis
# and the heatmap.
resultsList <- neatmap(df, scale.df = "basic", mainTitle = "Heatmap", 
                   xlabel = "", ylabel = "Networks",
                   link.method = "single", dist.method = "euclidean",
                   nBootRep = 100, xlabCex = 1, ylabCex = 1,
                   heatmapMargins = c(100, 50, 50, 100))
```

To display the heatmap:

``` r
resultsList[[4]]
```

### Dynamic Report

A dynamic report template has been included in the `inst/rmd` directory. Follow the instruction at the top of `template.Rmd` to create a dynamic report using your own data.

### Documentation

Available on [CRAN](https://CRAN.R-project.org/package=neatmaps/neatmaps.pdf).
