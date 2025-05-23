% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting.R
\name{generate_report}
\alias{generate_report}
\title{Generate Comprehensive Analysis Report}
\usage{
generate_report(data, result, output_file)
}
\arguments{
\item{data}{Cleaned port operations data (tibble/data.frame)}

\item{result}{Model result object from \code{\link{fit_arima_forecast}}}

\item{output_file}{Path for output HTML report (e.g., "analysis_report.html")}
}
\value{
Invisibly returns the path to generated HTML report
}
\description{
Creates an HTML report containing data summary, model diagnostics,
performance metrics, and visualizations for port operations analysis.
}
\details{
Report includes:
\itemize{
\item Data summary statistics and structure
\item ARIMA model parameters and coefficients
\item Training/test set performance metrics (R², RMSE)
\item Prediction visualization plots
}
}
\examples{
\dontrun{
data <- load_port_data("data.csv") \%>\% clean_port_data()
model <- fit_arima_forecast(data$berth_duration, exog_matrix)
generate_report(data, model, "port_analysis.html")
}
}
