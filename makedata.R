library(tidyverse)
library(readxl)
library(rvest)
library(ggplot2)

#WEB SCRAPING

#saved url and webpage for ease
url <-"https://en.wikipedia.org/wiki/2020_United_States_presidential_election"
webpage <-read_html(url)

#use rvest package to grab table from selected webpage
tables <- webpage|>
  html_nodes('table')
selected_table <- tables[[31]]

#use rvest package to convert wanted table into dataframe
df <- selected_table |> html_table(fill = TRUE)

#writes the table to a text file for ease of manipulation
write.table(df, "election_results.txt", sep= "\t",row.names=FALSE)




#CLEAN VOTER DATA

df <- read.table("election_results.txt",header = T)

#grab the columns I want
colnames(df)[1:7] <- c("States","DemVotes","DemPerc","DemEV","RepVotes","RepPerc","RepEV")

results <- df|>
  select(States,DemPerc,RepPerc)

#get rid of the districts within states
NotWanted <- c(1,22,23,32,33,34,58,59)
results <- slice(results, -NotWanted)

# get rid of % symbols, make everything numeric and then calculate difference between voters
results <- results |>
  mutate(
    DP = as.numeric(str_remove_all(DemPerc,"%")),
    RP = as.numeric(str_remove_all(RepPerc,"%")), 
    dif = (DP-RP))|>
  select(States,DP,RP,dif)

# clean up astrisks and things
results$States[[20]] <- "Maine"
results$States[[28]] <- "Nebraska"
results$States[[29]] <- "Nevada"
results$States[[31]] <- "New Jersey"
results$States[[44]] <- "Texas"


parties <- results |>
  mutate(Party = case_when(
    dif < -5 ~ "Republican",
    dif >= -5 & dif <= 5 ~ "Swing",
    dif > 5 ~ "Democrat"
  ))|>
  select(States,Party)


# VACCINATION DATA

StateStats<-read_excel('CDCDATA.xlsx', sheet='state_data_only')|>
  select(NAME,total_population,medincome,percent_healthcare_employment,percent_healthcare_employment)|>
  rename(States=NAME,total_state_pop=total_population)

StateStats <- as.data.frame(StateStats)


StateVaccineData<-read_excel('CDCDATA.xlsx', sheet='state')|>
  select(LongName,Doses_Administered,Administered_Dose1_Recip,Administered_Dose2_Recip,Doses_Distributed,Series_Complete_Yes
  )|>
  rename(States=LongName,Admin_Dose1=Administered_Dose1_Recip,Admin_Dose2=Administered_Dose2_Recip)

StateVaccineData <- as.data.frame(StateVaccineData)





df <- parties |>
  left_join(StateStats, join_by("States"))|>
  left_join(StateVaccineData, join_by("States"))|>
  mutate(med_inc = case_when(
    medincome < 50000 ~ "<50k",
    medincome >= 50000 & medincome <= 60000 ~ "50k-60k",
    medincome >= 60000 & medincome <= 70000 ~ "60k-70k",
    medincome > 70000 ~ ">70k"
  ))


df <- df|>
  mutate(perc_first =
           Admin_Dose1/total_state_pop)|>
  mutate(perc_finish = 
           Series_Complete_Yes/total_state_pop)|>
  mutate(wasted_doses = Doses_Distributed-Doses_Administered)|>
  select(-c(Admin_Dose1,Admin_Dose2,Series_Complete_Yes,Doses_Administered,Doses_Distributed))


write_csv(df, file = "vaccine_data_clean.csv")




