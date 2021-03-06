% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/empirical_controls.R
\name{empirical_controls}
\alias{empirical_controls}
\title{Empirical controls calculation function}
\usage{
empirical_controls(exprs, n = 40, q = 4)
}
\arguments{
\item{exprs}{An p x m input matrix of gene expression values with p genes and m samples. This matrix MUST have rownames.}

\item{n}{Number of pairs to create within each quantile bin (default 40)}

\item{q}{Number of quantiles desired (default 4)}
}
\value{
A (n*q) x m matrix with TSPs in rows and samples in columns
}
\description{
Generate candidate top-scoring pairs (TSPs) by finding "empirical
control" genes within quantiles of the data.
}
\details{
We intend to use TSPs as the features in our gene signature. Examining all features
would require generating $p$ choose 2 comparisons, most of which would be uninformative. We prioritize
the creation of features where one gene is expressed at a constant level across classes and the other
gene is expressed high and low across classes. To unearth pairs that behave like this, we rank all
genes by their average expression and break them into quantile groups. We then rank all genes within
each group by their variance and compute comparisons between the top $n$ most variable and least variable genes.
The result of this function provides a matrix of size $(n*q)$ x $m$, where each row is a binary vector indicating
the value of I($g_1 > g_2$) for two selected.
}

