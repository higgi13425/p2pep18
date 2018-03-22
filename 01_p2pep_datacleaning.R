# preparing schedules for p2pep18

# load libraries ####
library(tidyverse)
library(ponyexpress)
library(magrittr)
library(knitr)
library(rmarkdown)
library(janitor)

#load eventbrite file ####
#exported file from eventbrite,
# custom question responses, as csv
#Read in csv file
df<- read_csv("p2pep_customreport_22mar18.csv")
# split choices column (24) into distinct columns, discard > 10 choices
#separated by pipe, escape the pipe with 2 backslashes
# name new columns a-j
# fill missing with NA
df <- df %>% separate(24, c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), 
                sep = "\\|") %>% select(`Order Date`:Email, `Attendee #`, a:j)
df <- clean_names(df) #cleans vaiable names with janitor package
#now clean attendee names ####
#first names, last names that are lowercase
df$first_name[df$attendee == 911643049] <- "Sharon"
df$last_name[df$attendee == 911643049] <- "Shaw"
df$first_name[df$attendee == 912089192] <- "Teruhiko"
df$last_name[df$attendee == 912089192] <- "Yamazaki"
df$first_name[df$attendee == 912089193] <- "Misako"
df$last_name[df$attendee == 912089193] <- "Yamazaki"
df$first_name[df$attendee == 913084334] <- "Haruko"
df$first_name[df$attendee == 913432846] <- "John"
df$last_name[df$attendee == 913432846] <- "Greenough"
df$first_name[df$attendee == 913432847] <- "Suzi"
df$last_name[df$attendee == 913432847] <- "Wilson"
df$first_name[df$attendee == 913466024] <- "Kim"
df$last_name[df$attendee == 913466024] <- "Waldo"
df$first_name[df$attendee == 913466025] <- "Mary"
df$last_name[df$attendee == 913466025] <- "Waldo"
df$first_name[df$attendee == 914693345] <- "Stephen"
df$last_name[df$attendee == 914693345] <- "Seagall"
df$first_name[df$attendee == 918066217] <- "Anita"
df$last_name[df$attendee == 918066217] <- "Mehra"
df$first_name[df$attendee == 928946818] <- "John"
df$last_name[df$attendee == 928946818] <- "Scripps"
df$last_name[df$attendee == 900896052] <- "Malamet"
df$first_name[df$attendee == 930037491] <- "Philip"
df$last_name[df$attendee == 930037491] <- "Rea"
df$first_name[df$attendee == 930620305] <- "Kim"
df$last_name[df$attendee == 930620305] <- "Fitzpatrick 1"
df$first_name[df$attendee == 930620306] <- "Kim"
df$last_name[df$attendee == 930620306] <- "Fitzpatrick 2"
df$last_name[df$attendee == 900923877] <- "Mandel 1"
df$last_name[df$attendee == 903914703] <- "Mandel 2"

# consider gather with one col for sessions

df2<- gather(df, key="choice", value= "session", -(order_date:attendee)) %>% 
  select(-choice) 
df2$session <- str_trim(df2$session)
df2 <- df2[df2$last_name!="Mandel 2",]

df3 <- as.data.frame(table(df2$session))
names(df3) <- c("session", "count")
df3[rev(order(df3$count)),]
df3$rank <- as.integer(21-rank(df3$count))

df4 <- left_join(df2, df3, by = "session")  
df4 <- df4[!is.na(df4$session),]
df4 <- df4%>% arrange(attendee, rank)

# can group, count in order by attendee
# count occurrences, merge count table with attendee table
# then give everyone their most and least popular 
# start filling least and most pop
# watch 2 rooms for 2 popular
# watch overlapping times
# add breakouts
