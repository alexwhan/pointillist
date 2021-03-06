% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pointillist.R
\name{sample_df}
\alias{sample_df}
\title{Randomly sample a img df to reduce points}
\usage{
sample_df(img_df, frac)
}
\arguments{
\item{img_df}{A tidy data frame - output from \code{img_df()}}

\item{frac}{Numeric, between 0 and 1, fraction of total points to be included}
}
\value{
A tidy data frame with a reduced number of rows
}
\description{
Randomly sample a img df to reduce points
}
\examples{
img_path <- system.file("extdata/images", "seurat.png", package = "pointillist")

png_df <- img_df(img_path)
nrow(png_df)
png_df_sample <- sample_df(png_df, 0.3)
nrow(png_df_sample)
}

