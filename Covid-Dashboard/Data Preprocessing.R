library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(zoo)
library(bit64)
library(dplyr)

# Confirmed cases

df_confirmed_global = fread("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
setDT(df_confirmed_global)
head(df_confirmed_global)

df_confirmed_global <- df_confirmed_global[is.na(Lat) == FALSE & is.na(Long) == FALSE]
#df_confirmed_global <- df_confirmed_global[is.na(df_confirmed_global)==FALSE]
total_cases <- sum(df_confirmed_global[,ncol(df_confirmed_global)])

names(df_confirmed_global)[1:2] = c("Province", "Country")
df_confirmed_global[Province=="Hong Kong", Country := "Hong Kong"]
df_confirmed_global[Province=="Macau", Country := "Macao"]
df_confirmed_global[Country=="Taiwan*", Country := "Taiwan"]
df_confirmed_global[Country=="Korea, South", Country := "RepublicofKorea"]
df_confirmed_global[Country=="Congo (Brazzaville)" | Country=="Republic of the Congo", Country := "Congo"]
df_confirmed_global[Country=="Congo (Kinshasa)", Country := "Democratic Republic of the Congo"]
df_confirmed_global[Country=="Cote d'Ivoire", Country := "CotedIvoire"]
df_confirmed_global[Country=="Gambia, The", Country := "TheGambia"]
df_confirmed_global[Country=="Bahamas, The", Country := "TheBahamas"]
df_confirmed_global[Country=="Cabo Verde", Country := "CapeVerde"]
df_confirmed_global[Country=="Timor-Leste", Country := "TimorLeste"]
df_confirmed_global[Country=="Guinea-Bissau", Country := "GuineaBissau"]

df_confirmed_global <- df_confirmed_global[Country != "Micronesia"]
df_confirmed_global <- df_confirmed_global[Country != "SummerOlympics2020"]

df_confirmed_global$Country = df_confirmed_global$Country %>% str_replace_all(., " ", "") 
dates = names(df_confirmed_global)[which(names(df_confirmed_global)=="1/22/20"):ncol(df_confirmed_global)]

df_confirmed_global <- df_confirmed_global[, -c("Province", "Long", "Lat")]

df_confirmed_global <- df_confirmed_global[,
                                           lapply(.SD,sum),
                                            by = .(Country)]

df_confirmed_global <- data.table(cn=names(df_confirmed_global), transpose(df_confirmed_global))
names(df_confirmed_global) <- df_confirmed_global %>% slice(1) %>% unlist()
df_confirmed_global <- df_confirmed_global %>% slice(-1)
df_confirmed_global <- df_confirmed_global %>% rename_with( ~ paste0(.x, "_cases"))
setnames(df_confirmed_global, "Country_cases", "Date")

df_confirmed_global = data.frame(df_confirmed_global)
df_confirmed_global$Date = dates
rownames(df_confirmed_global) = 1:nrow(df_confirmed_global)
df_confirmed_global$Date = format(as.Date(df_confirmed_global$Date,"%m/%d/%y"))
df_confirmed_global

# cols <- df_confirmed_global[, -which(names(df_confirmed_global) == "Date")] 
#cols <- names(df_confirmed_global)
#cols <- cols[2:198]

x <- ncol(df_confirmed_global)

for(i in 2:x) 
{
  df_confirmed_global[[i]] <- as.numeric(df_confirmed_global[[i]])
}


#Deaths

df_deaths_global = fread("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
setDT(df_deaths_global)
head(df_deaths_global)

df_deaths_global <- df_deaths_global[is.na(Lat) == FALSE & is.na(Long) == FALSE]
#df_deaths_global <- ddf_deaths_global[is.na(df_deaths_global)==FALSE]
total_deaths <- sum(df_deaths_global[,ncol(df_deaths_global)])

names(df_deaths_global)[1:2] = c("Province", "Country")
df_deaths_global[Province=="Hong Kong", Country := "Hong Kong"]
df_deaths_global[Province=="Macau", Country := "Macao"]
df_deaths_global[Country=="Taiwan*", Country := "Taiwan"]
df_deaths_global[Country=="Korea, South", Country := "RepublicofKorea"]
df_deaths_global[Country=="Congo (Brazzaville)" | Country=="Republic of the Congo", Country := "Congo"]
df_deaths_global[Country=="Congo (Kinshasa)", Country := "Democratic Republic of the Congo"]
df_deaths_global[Country=="Cote d'Ivoire", Country := "CotedIvoire"]
df_deaths_global[Country=="Gambia, The", Country := "TheGambia"]
df_deaths_global[Country=="Bahamas, The", Country := "TheBahamas"]
df_deaths_global[Country=="Cabo Verde", Country := "CapeVerde"]
df_deaths_global[Country=="Timor-Leste", Country := "TimorLeste"]
df_deaths_global[Country=="Guinea-Bissau", Country := "GuineaBissau"]

df_deaths_global <- df_deaths_global[Country != "Micronesia"]
df_deaths_global <- df_deaths_global[Country != "SummerOlympics2020"]

df_deaths_global$Country = df_deaths_global$Country %>% str_replace_all(., " ", "") 
dates = names(df_deaths_global)[which(names(df_deaths_global)=="1/22/20"):ncol(df_deaths_global)]

df_deaths_global <- df_deaths_global[, -c("Province", "Long", "Lat")]

df_deaths_global <- df_deaths_global[,
                                           lapply(.SD,sum),
                                           by = .(Country)]

df_deaths_global <- data.table(cn=names(df_deaths_global), transpose(df_deaths_global))
names(df_deaths_global) <- df_deaths_global %>% slice(1) %>% unlist()
df_deaths_global <- df_deaths_global %>% slice(-1)
df_deaths_global <- df_deaths_global %>% rename_with( ~ paste0(.x, "_deaths"))
setnames(df_deaths_global, "Country_deaths", "Date")

df_deaths_global = data.frame(df_deaths_global)
df_deaths_global$Date = dates
rownames(df_deaths_global) = 1:nrow(df_deaths_global)
df_deaths_global$Date = format(as.Date(df_deaths_global$Date,"%m/%d/%y"))
df_deaths_global

x <- ncol(df_deaths_global)

for(i in 2:x) 
{
  df_deaths_global[[i]] <- as.numeric(df_deaths_global[[i]])
}

# merge dataframes 
jhu_merge = merge(df_confirmed_global, df_deaths_global, by = "Date")
jhu_merge$Date = as.Date(jhu_merge$Date, format="%Y-%m-%d")
jhu_merge$update = 1:nrow(jhu_merge)
#write.csv(jhu_merge, "input_data/jhu_data.csv")

#*****************************************************************

# shift from daily to weekly updates in collated_data
weekly_ind = seq(1, nrow(jhu_merge), 7)
ind_plus = nrow(jhu_merge) - max(weekly_ind)
if (ind_plus>0) { weekly_ind = c(1,weekly_ind+ind_plus) } 
jhu_merge = jhu_merge[weekly_ind,]

# load country data
countries = read.csv("input_data/countries_codes_and_coordinates.csv")

# check all jhu country names have corresponding country data
jhu_country_list = names(jhu_merge)[grepl("_cases", names(jhu_merge))] %>% str_replace_all(., "_cases", "") 
if (all(jhu_country_list %in% countries$jhu_ID)==FALSE) {
  stop(paste0("Error: mapping data lacking for the following countries: ",jhu_country_list[(jhu_country_list %in% countries$jhu_ID)==FALSE]))
}

collated_data = NULL
# loop to add new data for each new situation report
for (i in c(1:nrow(jhu_merge))) {
  
  # extract subset of data for date in row i
  jhu_subset = jhu_merge[i,]
  jhu_subset_cases = jhu_subset[,which(grepl("_cases", names(jhu_subset)))]
  jhu_subset_cases = jhu_subset_cases[,colSums(jhu_subset_cases)>0]
  jhu_subset_deaths = jhu_subset[,which(grepl("_deaths", names(jhu_subset)))]
  
  #browser()
  
  # build new dataframe to add updated data
  new_data = data.frame(jhu_ID = names(jhu_subset_cases) %>% str_replace_all(., "_cases", ""),
                        date = format(as.Date(jhu_subset$Date[1],"%Y-%m-%d")),
                        update = i,
                        cases = NA, new_cases = 0,
                        deaths = 0, new_deaths = 0)
  
  # update column names in new_jhu dataframes to include country names only
  colnames(jhu_subset_cases) = colnames(jhu_subset_cases) %>% str_replace_all(., "_cases", "") 
  colnames(jhu_subset_deaths) = colnames(jhu_subset_deaths) %>% str_replace_all(., "_deaths", "") 
  
  # loop to update cases
  for (j in 1:nrow(new_data)) {
    # update case numbers
    country_name = as.character(new_data$jhu_ID[j])
    new_data$cases[j] = jhu_subset_cases[,country_name]
    new_data$deaths[j] = jhu_subset_deaths[,country_name]
  }
  
  # append new data to collated dataframe
  collated_data = rbind(collated_data, new_data)
  collated_data$jhu_ID = as.character(collated_data$jhu_ID)
  
  # calculate new cases and deaths
  if (i == 1) {
    collated_data$new_cases = collated_data$cases
    collated_data$new_deaths = collated_data$deaths
  }
  
  if (i > 1) {
    # split it into date i and date i-1
    today = subset(collated_data, update==i)
    yesterday = subset(collated_data, update==(i-1))
    
    for (k in 1:nrow(today)) {
      country_name = today$jhu_ID[k]
      
      # if present in yesterday's data, calculate new cases by subtraction
      if (country_name %in% yesterday$jhu_ID) {
        collated_data$new_cases[collated_data$jhu_ID==country_name & collated_data$update==i] = today$cases[today$jhu_ID==country_name] - yesterday$cases[yesterday$jhu_ID==country_name] 
        collated_data$new_deaths[collated_data$jhu_ID==country_name & collated_data$update==i] = today$deaths[today$jhu_ID==country_name] - yesterday$deaths[yesterday$jhu_ID==country_name] 
      } else {
        # if absent from yesterday's data, new observations = total observations
        collated_data$new_cases[collated_data$jhu_ID==country_name & collated_data$update==i] = today$cases[today$jhu_ID==country_name] 
        collated_data$new_deaths[collated_data$jhu_ID==country_name & collated_data$update==i] = today$deaths[today$jhu_ID==country_name]  
      }
    }
  }
}
# allow for repatriation or reassigned cases without negative new_cases and new_deaths counts
collated_data$new_cases[collated_data$new_cases<0] = 0
collated_data$new_deaths[collated_data$new_deaths<0] = 0

# update country names
collated_data = merge(collated_data, countries[,c("jhu_ID", "country")], by = "jhu_ID")

# re-order
collated_data = collated_data[order(as.Date(collated_data$date, format="%Y-%m-%d"), -collated_data$cases, collated_data$country),]

# add rolling 7-day and 30-day averages for new cases and new deaths
collated_data$new_deaths_rolling7  = collated_data$new_cases_rolling7 = NA
country_list = unique(collated_data$jhu_ID)

# update time stamp
collated_data$last_update = NA
collated_data$last_update[nrow(collated_data)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))

for (i in 1:length(country_list)) {
  country_sub = subset(collated_data, jhu_ID==country_list[i])
  
  # add rolling 7-day average from 7th day onwards
  if (nrow(country_sub)>1) {
    for (j in 2:nrow(country_sub)) {
      country_sub$new_cases_rolling7[j] = round(country_sub$new_cases[j]/7,1)
      country_sub$new_deaths_rolling7[j] = round(country_sub$new_deaths[j]/7,1)
    }
  }
  
  # integrate with parent dataframe
  collated_data$new_cases_rolling7[collated_data$jhu_ID==country_list[i]] = country_sub$new_cases_rolling7
  collated_data$new_deaths_rolling7[collated_data$jhu_ID==country_list[i]] = country_sub$new_deaths_rolling7
  
}

# save file
write.csv(collated_data, "input_data/coronavirus.csv", row.names=F)
rm(list = ls())
