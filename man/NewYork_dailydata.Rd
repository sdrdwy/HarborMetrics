% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{NewYork_dailydata}
\alias{NewYork_dailydata}
\title{New York Daily Port Operations Data}
\format{
A data frame with 365 rows and 13 columns:
\describe{
\item{date}{Date of observation (YYYY-MM-DD)}
\item{net_contribution}{Net contribution to port efficiency (USD), e.g., 267894.57}
\item{dwt_contribution}{Deadweight tonnage contribution (tons), e.g., 551280.4}
\item{berth_duration}{Total berth duration in hours, e.g., 257.78}
\item{moor_duration}{Total mooring duration in hours, e.g., 0}
\item{moor_num}{Number of mooring events per day, e.g., 0}
\item{berth_num}{Number of berth events per day, e.g., 7.12}
\item{DailyAverageTemperature}{Daily average temperature in degrees Celsius, e.g., 5.4°C}
\item{DailyAverageRelativeHumidity}{Daily average relative humidity in percent, e.g., 0\%}
\item{DailyAverageWindSpeed}{Daily average wind speed in miles per hour (mph), e.g., 0 mph}
\item{DailyPrecipitation}{Daily precipitation in inches, e.g., 0 inches}
\item{DailySnowDepth}{Daily snow depth in inches, e.g., 0 inches}
\item{DailySnowfall}{Daily snowfall in inches, e.g., 0 inches}
}
}
\source{
Simulated data for demonstration purposes.
Data includes port operational metrics and weather measurements.
Intended for use in modeling and analysis of port efficiency under varying weather conditions.
}
\usage{
NewYork_dailydata
}
\description{
A dataset containing daily port operations metrics and weather conditions for New York.
}
\examples{
# Load the dataset
data("NewYork_dailydata")

# View the first few rows
head(NewYork_dailydata)

# Summary statistics
summary(NewYork_dailydata)
}
\keyword{datasets}
