% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swalim_climate.R
\name{swalim_climate}
\alias{swalim_climate}
\title{Download climate data from FAO SWALIM}
\usage{
swalim_climate(years = 2015:2023)
}
\arguments{
\item{years}{A vector of years for which the climate data is to be
downloaded (default is 2015 to 2023). Data only goes as far back as 2002}
}
\value{
A data frame containing the combined climate data for the specified
years.
}
\description{
This function downloads climate data for a specified range of years from
the FAO SWALIM (Somali Water and Land Information Management) website.
It takes a vector of years as an argument and returns a combined dataset
containing climate data for all the years within the specified range.
Note: As per FAO SWALIM, the current values of CDI do not include the NDVI
values, but rather a combination of temperature and rainfall. Since the
beginning of 2021, there has been a poor correlation between the NDVI and
ground information, resulting in false values of CDI. In this regard, the
use of NDVI in generating CDI values has been halted by FAO SWALIM
(see here: https://cdi.faoswalim.org/index/cdi).
}
\details{
The following datasets are downloaded for all districts in Somalia across
the given years:
\itemize{
\item CDI: Combined Drought Index
\item PDI: Precipitation Drought Index
\item TDI: Temperature Drought Index
\item NDVI: Normalized Difference Vegetation Index (not included in CDI
since 2021)
\item RFE: Rainfall Estimates
}
}
\examples{

# Download climate data for the years 2015 to 2023
climate_data_SOM <- swalim_climate(years = 2015:2023)

}
