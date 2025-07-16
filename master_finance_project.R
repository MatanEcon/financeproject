#Master Finance Project
options(scipen = 999)

library(quantmod)
library(ggplot2)
library(data.table)
library(collapse)
library(haven)
library(countrycode)
library(wbstats)
library(fixest)

setwd("C:/Users/matan/OneDrive/Documents/Masters/Finance/final project")

output <-'output/'

source('get_financial_data.R')

#Checks for random walk 1 and 3
source('rw_test.R')

#Add GDP, unemployment, Inflation, Intrest, and Country size, averaged to 5 year
#periods to the panel
source('add_country_characteristics.R')

#run regression and make graphs
source('results.R')
