library(WDI)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(dbplyr)
library(readxl)
library(xlsx)
library(stargazer)

#inital check to see syntax
a<- WDI(
  country ="US",
  indicator = "NY.GDP.DEFL.KD.ZG",
  start = 2008,
  end = 2008,
  extra = FALSE,
  cache = NULL, 
  latest = NULL,
  language = "en"
)

#checking for correct ECOWAS countries syntax

b <-WDI(
  country =c("CV","GM",
             "GN",
             "GW",
             "LR",
             "ML",
             "SN",
             "SL",
             "BJ",
             "BF",
             "GH",
             "CI",
             "NE",
             "NG",
             "TG"),
  indicator = "NY.GDP.DEFL.KD.ZG",
  start = 2008,
  end = 2008,
  extra = FALSE,
  cache = NULL, 
  latest = NULL,
  language = "en"
)

#running dataframe on 2002 to 2012
c<-b <-WDI(
  country =c("CV","GM",
             "GN",
             "GW",
             "LR",
             "ML",
             "SN",
             "SL",
             "BJ",
             "BF",
             "GH",
             "CI",
             "NE",
             "NG",
             "TG"),
  indicator = c( "inf"= "NY.GDP.DEFL.KD.ZG", "GDPGROWTH" = "NY.GDP.MKTP.KD.ZG", "EXCRATE"= "PA.NUS.FCRF","UNEMP" = "SL.UEM.TOTL.ZS", "POP" = "SP.POP.TOTL"),
  start = 2002,
  end = 2012,
  extra = FALSE,
  cache = NULL, 
  latest = NULL,
  language = "en"
)


#creating CFA dummy variable
d<- c%>%
  mutate(CFA = 0)


#adding 1 for all CFA particapting countries in ECOWAS
d$CFA[d$iso2c== "NE" ] <- 1
d$CFA[d$iso2c== "ML" ] <- 1
d$CFA[d$iso2c== "GW" ] <- 1
d$CFA[d$iso2c== "SN" ] <- 1
d$CFA[d$iso2c== "SL" ] <- 1
d$CFA[d$iso2c== "BF" ] <- 1
d$CFA[d$iso2c== "CI" ] <- 1
d$CFA[d$iso2c== "TG" ] <- 1

#changing names
d$country[d$country== "Gambia, The" ] <- "Gambia"
d$country[d$country == "Cote d'Ivoire"] <- "Ivory Coast"
d$country[d$country == "Cabo Verde"] <- "Cape Verde"





#CBI data
aa<-read_xlsx("/Users/jeremymassaquoi/Spring 2022/Econometrics/Research Question/Garriga_CBI dataset v.July 2019.xlsx")

#renaming column for join
aa<-aa%>%
  rename(country = cname)
z<-d



data_mergel <- merge(z, aa, by = c("country", "year")) 



#checking for inaccuracies 
check1<-data_merge1%>%
  group_by(country)%>%
  count(country)
check2<-b%>%
  group_by(country)%>%
  count(country)


#removing unwanted data
clean_data<-data_mergel%>%
  select(iso2c, country, year, inf, GDPGROWTH, EXCRATE, UNEMP, CFA, POP, lvaw_garriga)

clean_data$lvaw_garriga[is.na(clean_data$lvaw_garriga) ] <- 0.54939
  

#summary statistics
stargazer(clean_data, type = "text")

#linear regression
library(lmtest)
library(nlme)
library(broom)
log(clean_data$POP)

ols<-lm(inf ~ CFA + GDPGROWTH + log(EXCRATE) + UNEMP + lvaw_garriga+ log(POP), data = clean_data)





bptest(ols) #testing for heteroskedasticity 

stargazer(ols, type ="text")








