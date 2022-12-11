library(tidyverse)
library(dplyr)
library(janitor)

# function to calculate percentage change
perc <- function(x) {
  return(  round((x / sum(x) )*100, digits = 2 ))
}

#load data
data <- read.csv("erasmus_clean.csv")


#split data and counting gender sum grouping by country
receiving_gender_country <- tabyl(data, Receiving.Country.Code, Participant.Gender)
sender_gender_country <- tabyl(data, Sending.Country.Code, Participant.Gender)

# Compute percetage
receiving_gender_country$total <- receiving_gender_country$Male + receiving_gender_country$Female
receiving_gender_country$perc_female <- round( 100 * (receiving_gender_country$Female / receiving_gender_country$total), digits=2)
receiving_gender_country$perc_male <-round( 100 * (receiving_gender_country$Male / receiving_gender_country$total), digits=2 )

sender_gender_country$total <- sender_gender_country$Male + sender_gender_country$Female
sender_gender_country$perc_female <- round( 100 * (sender_gender_country$Female / sender_gender_country$total), digits=2 )
sender_gender_country$perc_male <-round( 100 * (sender_gender_country$Male / sender_gender_country$total), digits  =2 )




#keep onlu percentage
df1 <- receiving_gender_country[, -c(2:4)]
colnames(df1) <- list("country", "recv_female", "recv_male")

df2 <- sender_gender_country[, -c(2:4)]
colnames(df2) <- list("country", "send_female", "send_male")

# compute percentage of education level grouped by country and keep only percentage
df3 <- tabyl(data, Receiving.Country.Code, Education.Level)
colnames(df3) <- list("country", "recv_short", "recv_bs", "recv_ms", "recv_phd", "recv_nc")
df3[,2:6] <- t(apply(df3[,2:6],1, perc ) )

df4 <- tabyl(data, Sending.Country.Code, Education.Level)
colnames(df4) <- list("country", "send_short", "send_bs", "send_ms", "send_phd", "send_nc")
df4[,2:6] <- t(apply(df4[,2:6],1, perc ) )




#put all data frames into list
df_list <- list(df1, df2, df3, df4)

#merge all data frames in list
merged <- df_list %>% reduce(full_join, by='country')

write.csv(merged, "percentage_by_country.csv")


#################################################
#### 


