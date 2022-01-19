# Loading libraries -------------------

library(dplyr)
library(lubridate)

# Loading data and transformation and basic overview --------------------------

# Loading dataset

dd <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')
lapply(dd, class)

dd$date = ymd((dd$date))
min(dd$date)
max(dd$date)

# Creating variable to get monthly data in further processes

dd$is_new_month = floor_date(dd$date, 'month') == dd$date
summary(dd)

# Creating dataframe of columns, we have most observations of
dd_f <- dd[, which(colMeans(!is.na(dd)) > 0.5)]

# Areas of research --------------------------

# Looking at the hypothesis that strict monetary policies help the spread of covid
# Creating a data frame containing correlations between stringency index and total cases
corr_stringency_total_cases = data.frame(Country = c("0"), correlation = c(0))

countries = unique(dd_f$location)

for (country in countries)
{
  dd_country = dd[dd$location == country,]
  dd_country = dd_country[,colnames(dd_country) %in% c("location", "stringency_index", "total_cases")]
  dd_country = na.omit(dd_country)
  tryCatch( { correlation = cor(dd_country$stringency_index, dd_country$total_cases, use="complete.obs") }
            , error = function(e) {correlation = NULL})
  
  df = data.frame(Country = c(country), correlation = c(correlation))
  corr_stringency_total_cases = bind_rows(corr_stringency_total_cases,df)
  
}

summary(corr_stringency_total_cases)
# Basic data exploration -----------------------------------
# Feature selection --------------------------------------
features <- c("total_cases", "total_cases_per_million", "total_deaths", "total_deaths_per_million", "icu_patients", "icu_patients_per_million",
              "hosp_patients", "hosp_patients_per_million", "reproduction_rate", "total_vaccinations_per_hundred", "total_vaccinations", "stringency_index")
