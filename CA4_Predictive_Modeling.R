# Martin McParland - L00143723
# Msc Big Data Analytics Class A
# Data Science - CA4 Predictive Modeling - Health in Ireland
# Submitted 24th May 2019

# The dataset to be used includes Unemployment rates and death rates
# by month for the period 2005 to 2016. The years are categorized as
# Celtic_Tiger for years 2005 to 2008, Crash for years 2009 to 2012 and
# Recovery for years 2013 to 2016. The CSV file is read into a data frame.

unemployment_and_deaths <- read.csv("Unemployment and Death rates by Month 2005 - 2016.csv", 
                                    header = TRUE, stringsAsFactors = FALSE)


# View the head and structure of unemployment_and_deaths
head(unemployment_and_deaths)
str(unemployment_and_deaths)


# Rename first column header to Period and convert Period to a Factor
names(unemployment_and_deaths)[1] <- "Period"
unemployment_and_deaths$Period <- factor(unemployment_and_deaths$Period)
str(unemployment_and_deaths)

# The variables are visualised by year 

library(gplots)

par(mar=c(5,5,2,2)) # set the border sizes for the graph area
par(mfrow = c(1, 2)) # divide graph area into 2 cols

plotmeans(deaths_per_capita ~ Year, data = unemployment_and_deaths,
          xlab = "Year",
          ylab = "Deaths",
          main = "Mean Plot with 95% CI")

plotmeans(unemployment_rate ~ Year, data = unemployment_and_deaths,
          xlab = "Year",
          ylab = "Unemployment Rate",
          main = "Mean Plot with 95% CI")

