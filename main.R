# Loading libraries -------------------

library(dplyr)
library(lubridate)
library(leaps)
library(DataExplorer)
library(tidyverse)

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
# Create a general report
dd %>%
  create_report(
    output_file = "covid_19_report",
    output_dir = "ML/",
    y = "stringency_index",
    report_title = "covid_19_report"
  )


dd %>% introduce()
dd %>% plot_intro()

aux = data.frame(dd$total_cases, 
                 dd$total_cases_per_million, 
                 dd$total_deaths, 
                 dd$total_deaths_per_million, 
                 dd$icu_patients, 
                 dd$icu_patients_per_million,
                 dd$hosp_patients, 
                 dd$hosp_patients_per_million,
                 dd$reproduction_rate,
                 dd$total_vaccinations_per_hundred, 
                 dd$total_vaccinations, 
                 dd$stringency_index)
create_report(aux, y = "dd.stringency_index")

#Missing values -----
dd %>% plot_missing()
plot_missing(aux)
profile_missing(aux)

#Continuous features----
plot_histogram(aux)
windows()




plot_correlation(na.omit(aux), type = "c")


# Feature selection --------------------------------------
features <- c("total_cases", "total_cases_per_million", "total_deaths", "total_deaths_per_million", "icu_patients", "icu_patients_per_million",
              "hosp_patients", "hosp_patients_per_million", "reproduction_rate", "total_vaccinations_per_hundred", "total_vaccinations", "stringency_index")

df_of_interest = dd[, features]
fs=regsubsets(stringency_index~.,data=df_of_interest,nvmax=20)
summary(fs)$which
names(summary(fs))
summary(fs)$adjr2
write.csv(df_of_interest, 'stringency_index.csv')

predict_future_cases_df = dd[, c('location', 'date', 'total_cases')]
write.csv(predict_future_cases_df, 'total_cases.csv')
