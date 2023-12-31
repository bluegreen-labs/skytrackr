% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stk_cluster.R
\name{stk_cluster}
\alias{stk_cluster}
\title{Cluster geolocator data}
\usage{
stk_cluster(df, k = 2, method = "kmeans")
}
\arguments{
\item{df}{a skytrackr data frame}

\item{k}{number of k-means/hierarchical clusters to consider}

\item{method}{method to use, kmeans by default, hclust can be set}
}
\value{
original data frame with attached cluster labels
}
\description{
Uses k-means and hierarchical clustering to group geolocator
covariates into consistent groups for visual analysis
}
