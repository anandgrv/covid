############################################################
###LEFT TO DO:
#* deaths vs time and Delta(deaths) vs time (can be useful to determine where the peak happened)
#* https://www.kaggle.com/tarunkr/covid-19-case-study-analysis-viz-comparisons
#Add testing dataset? (number of tests done for each country per day)
#* Possibility of using covid_19_ita data (in particular covid_19_ita/COVID-19-master/legacy/dati-andamento-nazionale is interesting)

###################################################
#IMPORTANT VARIABLES
#1. global_covid19 --> "global_covid19.csv"
#--> for each country we studied: 
####* confirmed - recovered (= atm), 
####* confirmed_cases, 
####* deaths, 
####* incidence, 
####* recovered
#--> incidence = deaths/confirmed_cases

#2. global_covid19_diff --> "global_covid19_diff.csv"
#--> for each country country is the difference row by row (meaning day by day) 
#    for confirmed_cases, deaths, recovered

#3. overall_covid19 --> "overall_covid19.csv"
#--> the overall trend in the world is summarized

###################################################

################# --> PLOTS <-- ################
###FOLDER plots_country --> plots regarding a specific country
#--> "Country.png": plot of confirmed cases, deaths and recovered
#--> "Country_bell.png": plot of difference between confirmed cases and recovered
#--> "Country_difference.png": plot of daily observed variables
#--> "Country_incidence.png": plot of deaths/confirmed_cases

###FOLDER plots_continent --> plots regarding each continent
#--> "Country.png": plot of confirmed cases, deaths and recovered
#--> "Country_bell.png": plot of difference between confirmed cases and recovered
#--> "Country_difference.png": plot of daily observed variables
#--> "Country_incidence.png": plot of deaths/confirmed_cases

###FOLDER plots_global --> plots regarding the overall global trend

###FOLDER plots_top10 --> plots regarding the top10 countries for each variable studied
################################################

#############################################################
library(magrittr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggthemes)
library(scales)
library(iterators)
library(plyr)
library(countrycode)


setwd("~/Dropbox/stat_learn_project/main_dataset/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series")
#############################################################

###########################################################
# global_covid19 <- read.csv('global_covid19.csv',na.strings = c("", "NA"), stringsAsFactors = FALSE)
# global_covid19_diff <- read.csv('global_covid19_diff.csv',na.strings = c("", "NA"), stringsAsFactors = FALSE)
# global_covid19_continent <- read.csv('global_covid19_continent.csv',na.strings = c("","NA"), stringsAsFactors = FALSE)
# overall_covid19 <- read.csv('overall_covid19.csv', na.strings = c("", "NA"), stringsAsFactors = FALSE)
#######################################################

#################
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
#################

##### --> DATASET - TIME SERIES - <-- ######
global_conf <- read.csv('time_series_covid19_confirmed_global.csv', na.strings = c("", "NA"), stringsAsFactors = FALSE)
global_deaths <- read.csv('time_series_covid19_deaths_global.csv', na.strings = c("", "NA"),stringsAsFactors = FALSE)
global_recovered <- read.csv('time_series_covid19_recovered_global.csv', na.strings = c("", "NA"), stringsAsFactors = FALSE)

#Assigning to "days" all the days between first and last measurements
start_date <- ymd("20-01-23")
end_date <- ymd("20-08-24")
days_diff <- interval(start_date, end_date)/days(1)
days <- format(start_date + days(0:days_diff), format="%y-%m-%d")

dfList <- list(global_conf, global_deaths, global_recovered)
#Cleaning, renaming, and manipulating global_conf, global_deaths and global_recovered
for (i in 1:length(dfList)){
  dfList[[i]][c(1,3,4)] <- list(NULL) #Dropping Province,Latitude and Longitude
  
  col_names <- unique(dfList[[i]][,'Country.Region'])
  
  #NOT INTERESTED IN PROVINCES -> we can sum all the cases for all the provinces in each country
  dfList[[i]][,-1] <- lapply(dfList[[i]][,-1],as.numeric)
  dfList[[i]] <- dfList[[i]] %>%
      group_by(Country.Region) %>%
          summarise_at(-1,sum)
  
  #Transposing dataframe
  dfList[[i]] <- data.frame(t(dfList[[i]]))  
  names(dfList[[i]]) <- col_names
  dfList[[i]] <- dfList[[i]][-1,]
  rownames(dfList[[i]]) <- days 
}

global_conf <- dfList[[1]]
global_deaths <- dfList[[2]]
global_recovered <- dfList[[3]]

############################
#Merging the 3 datasets global_conf_t_tot, global_deaths_t_tot, global_recovered_t_tot
#into global_covid19

#To do that first we have to change the name of the columns of each one of the 3
#For example global_conf column will be Country_conf
subs <- c("conf","deaths","recovered")
for (i in 1:length(dfList)){
  for (col_name in col_names){
    names(dfList[[i]])[names(dfList[[i]]) == col_name] <- paste(col_name,subs[i],sep='_')
    dfList[[i]]$Date <- rownames(dfList[[i]])
  }
}
global_covid19 <- join_all(dfList, by = 'Date', type =  'full')

#Reordering the columns:
global_covid19 <- global_covid19[, order(names(global_covid19))]
global_covid19 <- global_covid19[c(grep("Date",names(global_covid19)), 1:ncol(global_covid19))]
global_covid19[grep("Date.",names(global_covid19))] <- NULL

#As factor 
global_covid19 %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
global_covid19[,2:ncol(global_covid19)] <- sapply(global_covid19[,2:ncol(global_covid19)], as.factor)

#Date column set as date with lubridate
global_covid19$Date <- ymd(global_covid19$Date)

#Renaming misspelled columns
#We standardize the columns to the following list of countries grouped per continent:
#Reading the list of continents with respective countries
continents <- read.csv("country-and-continent-codes-list.csv", na.strings = c("", "NA"), sep = ",", stringsAsFactors =  FALSE)
misspelled <-      c("BurkinaFaso" ,"Bosniaand Herzegovina" ,"CaboVerde",  "Congo","CostaRica", "Coted.Ivoire",   "ElSalvador",  "Korea.South","NewZealand", "NorthMacedonia","SaintKitts",            "SaintLucia","SaintVincent","SanMarino","SaoTome","SaudiArabia","SierraLeone","SouthAfrica","SriLanka",                                             "Timor.Leste","Trinidadand","UnitedArab","UnitedKingdom","WesternSahara","CentralAfrican","US", "DominicanRepublic","Kosovo")
correct_country <- c("Burkina Faso","Bosnia and Herzegovina","Cape Verde", "Congo","Costa Rica","Cote d'Ivoire", "El Salvador", "Korea",      "New Zealand","Macedonia",     "Saint Kitts and Nevis", "Saint Lucia",  "Saint Vincent and the Grenadines", "San Marino", "Sao Tome and Prinicipe","Saudi Arabia", "Sierra Leone", "Africa","Sri Lanka","Timor-Leste","Trinidad and Tobago","Saudi Arabia","United Kingdom of Great Britain & Northern Ireland", "Western Sahara","Central African Republic","United States of America","Dominican Republic","Europe")

length(misspelled) == length(correct_country)

#Replacing misspelled columns with correct ones

for (i in seq(1,length(misspelled),1)){
  idx <- grep(misspelled[i], colnames(global_covid19))
  for (k in idx){
    this_col <- colnames(global_covid19)[k]
    colnames(global_covid19)[k] <- paste(correct_country[i],gsub(".*_","",this_col), sep = "_")
  }
}

#Removing columns that are not countries
not_a_country <- c("MS Zaandam","Diamond Princess")
for (not_country in not_a_country){
  global_covid19[grep(not_country,colnames(global_covid19))] <- NULL
}

#######################################################

#Congo appears two time, sum the columns

global_covid19$new_conf <- as.numeric(unlist(global_covid19[119])) + as.numeric(unlist(global_covid19[122]))
global_covid19$new_deaths <- as.numeric(unlist(global_covid19[120])) + as.numeric(unlist(global_covid19[123]))
global_covid19$new_recovered <- as.numeric(unlist(global_covid19[121])) + as.numeric(unlist(global_covid19[124]))
global_covid19[grep('Congo',names(global_covid19))] <- list(NULL)
names(global_covid19)[names(global_covid19) == "new_conf"] <- "Congo_conf"
names(global_covid19)[names(global_covid19) == "new_deaths"] <- "Congo_deaths"
names(global_covid19)[names(global_covid19) == "new_recovered"] <- "Congo_recovered"

global_covid19 <- global_covid19[, order(names(global_covid19))]
global_covid19 <- global_covid19[c(grep("Date",names(global_covid19)), 1:ncol(global_covid19))]
global_covid19[grep("Date.",names(global_covid19))] <- NULL

#countries_global contains all the countries name
countries_global <- names(global_covid19)
for(idx in 1:length(countries_global)){
  countries_global[idx] <- gsub("\\_.*","",countries_global[idx])
}
countries_global <-unique(countries_global[-1])

###### --> MISSING DATA <-- #########
which(is.na(global_covid19)) #No NA found

###########################################################
#Defining Incidence = deaths/confirmeds
#Create a column Country_incidence in global_covid19\
##Adding (confirmed - recovered) as "Country_atm" for each country in global_covid19

for (this_country in countries_global){
  print(this_country)
  this_incidence <- paste(this_country, "_incidence", sep="")
  this_atm <- paste(this_country, "_atm", sep="")
  
  confirmeds <- as.numeric(unlist(global_covid19[names(global_covid19) == paste(this_country,"_conf",sep="")]))
  deaths <- as.numeric(unlist(global_covid19[names(global_covid19) == paste(this_country,"_deaths",sep="")]))
  recovered <- as.numeric(unlist(global_covid19[names(global_covid19) == paste(this_country,"_recovered",sep="")]))
  # print(this_country)
  # print(length(confirmeds))
  # print(length(deaths))
  # print(length(recovered))

  global_covid19[this_incidence] <-  deaths / confirmeds
  global_covid19[this_atm] <-  confirmeds - recovered
  
  global_covid19[is.nan(global_covid19)] <- 0  
}

#Reordering columns name
global_covid19 <- global_covid19[, order(names(global_covid19))]
global_covid19 <- global_covid19[c(grep("Date",names(global_covid19)), 1:ncol(global_covid19))]
global_covid19[grep("Date.",names(global_covid19))] <- NULL
#############################################################

###########################################################
##CREATING glbal_covid19_diff containing daily observations
global_covid19_diff <- global_covid19

for (idx in seq(2, length(colnames(global_covid19)),1)){
  this_col <- as.name(paste(colnames(global_covid19)[idx], "_diff", sep=""))
  next_row <- as.numeric(unlist(global_covid19_diff[idx]))
  pre_row <- lag(as.numeric(unlist(global_covid19_diff[idx])))
  global_covid19_diff[idx] <-  next_row - pre_row
  # colnames(global_covid19_diff)[length(colnames(global_covid19)) + idx] <- this_col  
  idx <- idx + 1
}
#First row diff generated NAs --> replace with 0
global_covid19_diff <- global_covid19_diff %>% replace(is.na(.),0)
#############################################################



##########################################
#Summarizing countries into their respective continents
continents <- read.csv("country-and-continent-codes-list.csv", na.strings = c("", "NA"), sep = ",", stringsAsFactors =  FALSE)
all_continents <- unique(continents$Continent_Name)
all_continents <- all_continents[-3]  #We have no data of Antarctica
print(all_continents)
global_covid19_continent <- global_covid19

#Renaming columns from country to its respective continent
for (country in countries_global){
  for (idx in grep(country, names(global_covid19))){
    this_col <- colnames(global_covid19_continent)[idx]
    this_country <- gsub("\\_.*","",this_col)
    continent <- continents$Continent_Name[grep(this_country,continents$Country_Name)[1]]
    if (!is.na(continent)){
      new_col <- paste(continent, gsub(".*_","",this_col), sep = "_")
    }
    else {
      new_col <- this_country
    }
    colnames(global_covid19_continent)[idx] <- new_col
  }
}

#Summing up all continents for the 5 different variables we defined
patterns <- c("_atm","_conf","_deaths","_incidence","_recovered")
for (continent in all_continents) {
  for(pattern in patterns){
    idx <- intersect(grep(continent,names(global_covid19_continent)), grep(pattern, names(global_covid19_continent)))
    new_col_name <- gsub(" \\_","_", paste(continent,pattern,""))
    if (!is.na(idx) & length(idx) > 1){
      new_col_values <- rowSums(sapply(global_covid19_continent[,idx],as.numeric))
      global_covid19_continent$new_col_name <- new_col_values
      colnames(global_covid19_continent)[grep("new_col_name",names(global_covid19_continent))] <- new_col_name
    }
  }
}

length(global_covid19) == length(global_covid19_continent)-6*5 #ANTARTICA HAS NO DATA

#Dropping all columns that arent continents
global_covid19_continent <- global_covid19_continent[, -c(2:length(global_covid19))]

#incidence column has to be recalculated 
cont_matrix <- data.matrix(global_covid19_continent)
for (continent in all_continents){
  this_inc <- paste(continent,"_incidence",sep="")
  this_death <- paste(continent,"_deaths",sep="")
  death_idx <- grep(this_death,names(global_covid19_continent))
  inc_idx <- grep(this_inc, names(global_covid19_continent))
  this_conf  <- paste(continent,"_conf",sep="")
  conf_idx <- grep(this_conf,names(global_covid19_continent))

  global_covid19_continent[,inc_idx] <- cont_matrix[,death_idx]/cont_matrix[,conf_idx]
}


global_covid19_continent[is.nan(global_covid19_continent)] <- 0


###########################################################
##COVID 19 OVERALL IN THE WORLD
overall_covid19 <- data.frame(global_covid19_continent$Date)
names(overall_covid19)[1] <- "Date"
for (pattern in patterns){
  idx <- grep(pattern,names(global_covid19_continent))
  overall_covid19$new_col <- rowSums(sapply(global_covid19_continent[,idx],as.numeric))
  new_col <- paste("Global",pattern,sep="")
  names(overall_covid19)[names(overall_covid19) == "new_col"] <- new_col
}
overall_covid19["Global_incidence"] <- data.matrix(overall_covid19[grep("deaths",names(overall_covid19))]) / data.matrix(overall_covid19[grep("conf",names(overall_covid19))]) 

#COVID 19 DIFFERENCE OVERALL IN THE WORLD
overall_covid19_diff <- data.frame(overall_covid19)

for (idx in seq(2, ncol(overall_covid19),1)){
  this_col <- as.name(paste(colnames(overall_covid19_diff)[idx], "_diff", sep=""))
  next_row <- as.numeric(unlist(overall_covid19_diff[idx]))
  pre_row <- lag(as.numeric(unlist(overall_covid19_diff[idx])))
  overall_covid19_diff[idx] <-  next_row - pre_row
}
#First row diff generated NAs --> replace with 0
overall_covid19_diff <- overall_covid19_diff %>% replace(is.na(.),0)
#############################################################


###########################################################
##################  PLOTS   ####################
####PLOT FOR EACH COUNTRY
#with confirmed cases, deaths and recovered up to today
counter <- 1 
for (idx in seq(3,564,length(patterns))){ #564
  this_country <- countries_global[counter]
  counter <- counter + 1
  temp_ <- global_covid19[c(1,idx,idx+1,idx+3)]
  temp_covid <- gather(temp_, country, cases, 2:4, factor_key = TRUE)
  temp_covid$Date <- ymd(temp_covid$Date)
  temp_covid <- temp_covid[order(as.Date(temp_covid$Date, format="%Y-%m-%d")),]
  temp_covid$cases <- as.numeric(temp_covid$cases)
  #Generate the plot
  ggplot(temp_covid,aes(as.Date(Date),cases)) +
    geom_line(aes(color = country)) +
    ggtitle(this_country) +
    theme_bw() +
    scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
    scale_color_manual(values = c("#348ce5","#a63636","#5fb03b"),labels = c("confirmed","deaths","recovered")) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  ggsave(filename = file.path("plots_country",paste(this_country,".png",sep="")))
}

###########################################
#DIFFERENCE BETWEEN ONE DAY AND THE PREVIOUS
####Doing the same plotting as before, but now with global_covid19_diff
counter <- 1 #used just to take countr name from country_global
for (idx in seq(3,564,length(patterns))){ #564
  this_country <- countries_global[counter]
  counter <- counter + 1
  temp_ <- global_covid19_diff[c(1,idx,idx+1,idx+3)]
  temp_covid <- gather(temp_, country, cases, 2:4, factor_key = TRUE)
  temp_covid$Date <- ymd(temp_covid$Date)
  temp_covid <- temp_covid[order(as.Date(temp_covid$Date, format="%Y-%m-%d")),]
  temp_covid$cases <- as.numeric(temp_covid$cases)
  #Generate the plot
  ggplot(temp_covid,aes(as.Date(Date),cases)) +
    geom_line(aes(color = country)) +
    ggtitle(this_country) +
    theme_bw() +
    scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
    scale_color_manual(values = c("#348ce5","#a63636","#5fb03b"),labels = c("confirmed","deaths","recovered")) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  ggsave(filename = file.path("plots_country",paste(this_country,"_difference.png",sep="")))
}

######################################################
#PERCENTAGE OF DEATHS EACH DAY

####Doing the same plotting as before, but now with global_covid19_diff
counter <- 1 #used just to take countr name from country_global
for (idx in grep("_incidence",names(global_covid19_diff))){ #564
  this_country <- countries_global[counter]
  counter <- counter + 1
  
  temp_covid <- global_covid19[c(1,idx)]
  print(colnames(temp_covid))
  #Generate the plot
  ggplot(temp_covid,aes(as.Date(Date),as.numeric(unlist(temp_covid[2])))) +
    geom_line(aes(color = country)) +
    ggtitle(this_country) +
    theme_bw() +
    scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
    scale_color_manual(values = c("#348ce5","#a63636","#5fb03b"),labels = c("incidence","deaths","recovered")) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  ggsave(filename = file.path("plots_country",paste(this_country,"_incidence.png",sep="")))
}

##########################################################
###confirmed - recovered from global_covid19
####Doing the same plotting as before using the Country_atm columns
counter <- 1 #used just to take country name from country_global
for (idx in grep("_atm",names(global_covid19))){ 
  this_country <- countries_global[counter]
  counter <- counter + 1
  temp_covid <- global_covid19[c(1,idx)]
  
  #Generate the plot
  ggplot(temp_covid,aes(as.Date(Date),as.numeric(unlist(temp_covid[2])))) +
    geom_line(aes(color = country)) +
    ggtitle(this_country) +
    theme_bw() +
    scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
    scale_color_manual(values = c("#a63636"),labels = c("confirmed - recovered")) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  ggsave(filename = file.path("plots_country",paste(this_country,"_bell.png",sep="")))
}

################
#TOP 10 BAR PLOT
colori <- data.frame(t(rbind(patterns,c("blue","darkblue","darkred","darkorange","darkgreen"))))

names(colori) <- c("Patterns","Colours")
top_10 <- data.frame(t(colSums(data.matrix(global_covid19[,-1]))))
top_10[grep("incidence",names(top_10))] <- top_10[grep("incidence",names(top_10))]/length(global_covid19$Date)
for (pattern in patterns){
  idx <- grep(pattern,names(top_10))
  sorted <- data.frame(sort(top_10[idx], decreasing = TRUE)[1:10])
  names(sorted) <- gsub("\\_.*","",names(sorted))
  names(sorted)[grep("Kingdom", names(sorted))] <- "UK"
  names(sorted)[grep("United.States",names(sorted))] <- "US"
  print(names(sorted))
  sorted <- melt(sorted)
  print(rownames(sorted))
  p <- ggplot(sorted,aes(x = value  ,y = variable)) +
    geom_bar(stat = "identity", fill = colori$Colours[colori$Patterns == pattern]) +
    ggtitle(paste("Top 10",pattern,sep = "")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  print(p)
  ggsave(filename = file.path("plots_top10",paste("Top 10",pattern,".png",sep="")))
}



##########################################################
#PLOTS FOR EACH CONTINENT: confirmed, deaths and recovered
for (continent in all_continents){
  idx <- grep(continent,names(global_covid19_continent))
  print(idx)
  print(names(global_covid19[c(1,idx[2],idx[3],idx[5])]))
  temp_covid <- gather(global_covid19[c(1,idx[2],idx[3],idx[5])], country, cases, c(2,3,4), factor_key = TRUE)
  temp_covid$Date <- ymd(temp_covid$Date)
  temp_covid <- temp_covid[order(as.Date(temp_covid$Date, format="%Y-%m-%d")),]
  temp_covid$cases <- as.numeric(temp_covid$cases)
  #Generate the plot
  ggplot(temp_covid,aes(as.Date(Date),cases)) +
    geom_line(aes(color = country)) +
    ggtitle(continent) +
    theme_bw() +
    scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
    scale_color_manual(values = c("#348ce5","#a63636","#5fb03b"),labels = c("confirmed","deaths","recovered")) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  ggsave(filename = file.path("plots_continent",paste(continent,".png",sep="")))
}

##incidence and confirmed - recovered
for (continent in all_continents){
  for (pattern in c("atm","incidence")){
    this_country <- continent
    this_col <- paste(continent,pattern,sep="_")
    print(this_col)
    idx <- grep(this_col,names(global_covid19_continent))
    ggplot(global_covid19_continent,aes(as.Date(Date),as.numeric(unlist(global_covid19_continent[idx])))) +
      geom_line(aes(color = country)) +
      ggtitle(this_country) +
      theme_bw() +
      scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
      scale_color_manual(values = c("#a63636"),labels = c(pattern)) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
            axis.text.x = element_text(angle = 90),  
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "top") 
    ggsave(filename = file.path("plots_continent",paste(this_col,".png",sep="")))
  }
}


##########################################################
#PLOTS FOR GLOBAL SITUATION

#confirmed, deaths, recovered
idx <- grep("Global",names(overall_covid19))
print(names(overall_covid19[c(1,idx[2],idx[3],idx[5])]))
temp_covid <- gather(overall_covid19[c(1,idx[2],idx[3],idx[5])], country, cases, c(2,3,4), factor_key = TRUE)
temp_covid$Date <- ymd(temp_covid$Date)
temp_covid <- temp_covid[order(as.Date(temp_covid$Date, format="%Y-%m-%d")),]
temp_covid$cases <- as.numeric(temp_covid$cases)
#Generate the plot
ggplot(temp_covid,aes(as.Date(Date),cases)) +
  geom_line(aes(color = country)) +
  ggtitle("Global") +
  theme_bw() +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  scale_color_manual(values = c("#348ce5","#a63636","#5fb03b"),labels = c("confirmed","deaths","recovered")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
        axis.text.x = element_text(angle = 90),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") 
ggsave(filename = file.path("plots_global",paste("Global",".png",sep="")))

##Incidence and confirmed - recovered
for (pattern in c("atm","incidence")){
  continent <- "Global"
  this_col <- paste(continent,pattern,sep="_")
  idx <- grep(this_col,names(overall_covid19))
  ggplot(overall_covid19,aes(as.Date(Date),as.numeric(unlist(overall_covid19[idx])))) +
    geom_line(aes(color = country)) +
    ggtitle("Global") +
    theme_bw() +
    scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
    scale_color_manual(values = c("#a63636"),labels = c(pattern)) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
          axis.text.x = element_text(angle = 90),  
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") 
  ggsave(filename = file.path("plots_global",paste(this_col,".png",sep="")))
}

#Overall covid19 difference: confirmed, deaths and recovered
#confirmed, deaths, recovered
idx <- grep("Global",names(overall_covid19_diff))
print(names(overall_covid19_diff[c(1,idx[2],idx[3],idx[5])]))
temp_covid <- gather(overall_covid19_diff[c(1,idx[2],idx[3],idx[5])], country, cases, c(2,3,4), factor_key = TRUE)
temp_covid$Date <- ymd(temp_covid$Date)
temp_covid <- temp_covid[order(as.Date(temp_covid$Date, format="%Y-%m-%d")),]
temp_covid$cases <- as.numeric(temp_covid$cases)
#Generate the plot
ggplot(temp_covid,aes(as.Date(Date),cases)) +
  geom_line(aes(color = country)) +
  ggtitle("Global_diff") +
  theme_bw() +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  scale_color_manual(values = c("#348ce5","#a63636","#5fb03b"),labels = c("confirmed","deaths","recovered")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, color = "black", size = 14, face = "bold.italic"),
        axis.text.x = element_text(angle = 90),  
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") 
ggsave(filename = file.path("plots_global",paste("Global_diff",".png",sep="")))




#######################################################
#Writing most important dataframes to csv
write.csv(global_covid19,"global_covid19.csv")
write.csv(global_covid19_diff, "global_covid19_diff.csv")
write.csv(global_covid19_continent, "global_covid19_continent.csv")
write.csv(overall_covid19, "overall_covid19.csv")
write.csv(overall_covid19_diff,"overall_covid19_diff.csv")
#######################################################
