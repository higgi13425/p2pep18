# preparing schedules for p2pep18
# conceptual grid
# topic - title of talk
# time_slot 1-5, time slots
# rooms 1-6 used
#talknum combines room and time_slot as 2 digit #, unique for each topic
# talknum tens unit - indicates time_slot
# talknum ones unit - indicates Room

# load libraries ####
library(tidyverse)
library(ponyexpress)
library(magrittr)
library(knitr)
library(rmarkdown)
library(janitor) #for clean_names, get_dupes
library(Hmisc) # for Lag

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
df <- clean_names(df) #cleans variable names with janitor package
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

#chose not to do this - fill in missing choices later
# fill in NA values for people who made fewer than 5 choices
# for b, c, which have few missing - put in big, popular talks
# for d-e, less popular talks to give them a boost
# use f- k to fill in missing slots, as people may have picked only talks in 
#timeslot 1 or 2
# note that this will create some duplicates 
# which need to be removed later
# df$b[is.na(df$b)] <- "Diets and Dietary Research in IBD"
# df$c[is.na(df$c)] <- "IBD and Your Emotional Health"
# df$d[is.na(df$d)] <- "Diet, Growth, and Puberty in IBD"
# df$e[is.na(df$e)] <- "Parenting as an IBD patient"
# 
# # fill in time_slot 1
# df$f[is.na(df$f)] <- "Research Update: What is the Future in IBD?"
# df$g[is.na(df$g)] <- "Research Update: What is the Future in IBD?"
# df$k <- "Research Update: What is the Future in IBD?"
# 
# # fill in time_slot 2
# df$i[df$attendee < 913151741] <- "Diets and Dietary Research in IBD"
# df$i[df$attendee < 925026229] <- "Taking Responsibility for Your IBD Care"
# df$i[df$attendee >= 925026229] <- "Protecting Your Bones in IBD"
# 
# # fill in time_slot 4
# df$h[df$attendee < 913151741] <- "New IBD Research You Need To Know About"
# df$h[df$attendee < 925026229] <- "Fast Pass to the Next Generation of IBD Treatment"
# df$h[df$attendee >= 925026229] <- "Supporting a Partner or Family Member with IBD"
# #fill in time_slot 5
# df$j[df$attendee < 913151741] <- "Alternative Therapies in IBD"
# df$j[df$attendee < 925026229] <- "Food Related Anxiety and Disordered Eating"
# df$j[df$attendee >= 925026229] <- "Patient Medication Panel"

#  gather with one col for topics

df2<- gather(df, key="choice", value= "topic", -(order_date:attendee)) %>% 
  select(-choice) 
df2$topic <- str_trim(df2$topic)


# randomly assign for repeated topics
# Diet and Emo to A or B version to each attendee in df2
set.seed(123)
df2$rand <- runif(nrow(df2),min=0, max=1)
df2$topic[df2$topic=="Diets and Dietary Research in IBD" & df2$rand<=0.75]<-
  "Diets and Dietary Research in IBD 1"
df2$topic[df2$topic=="Diets and Dietary Research in IBD" & df2$rand>0.75]<-
  "Diets and Dietary Research in IBD 2"
df2$topic[df2$topic=="IBD and Your Emotional Health" & df2$rand>=0.5]<-
  "IBD and Your Emotional Health B"
df2$topic[df2$topic=="IBD and Your Emotional Health" & df2$rand<0.5]<-
  "IBD and Your Emotional Health A"
#df2 <- df2 %>% select(-rand) #remove rand variable to allow to find duplicates
#df2 <- df2[!duplicated(df2),] #and now remove them

# can group, count in order by attendee
df3 <- as.data.frame(table(df2$topic))
names(df3) <- c("topic", "count")
df3[rev(order(df3$count)),]
set.seed=12
df3$rank <- as.integer(23-rank(df3$count, ties.method = "random"))

# add room number, time_slot to each topic in df3
df3$topic <- as.character(df3$topic)
#assign talknum as first digit = time_slot, 2nd digit = room
# five time slots, 6 rooms used
df3$talknum <- c(51,55,45,12,21,42,53,13,14,52,23,44,46,41,25,54,22,11,43,24,26,15)
df3 <- df3 %>% mutate(room = case_when(df3$talknum %% 10 == 1 ~ "Forum Hall",
              df3$talknum %% 10==2 ~ "Great Lakes Central",
              df3$talknum %% 10==3 ~ "Great Lakes North",
              df3$talknum %% 10==4 ~ "Great Lakes South",
              df3$talknum %% 10==5 ~ "Boardroom 1",
              df3$talknum %% 10==6 ~ "Boardroom 2"))
df3 <- df3 %>% mutate(room_limit = case_when(df3$talknum %% 10 == 1 ~ 140,
              df3$talknum %% 10 ==2 ~ 30,
              df3$talknum %% 10 ==3 ~ 30,
              df3$talknum %% 10==4 ~ 30,
              df3$talknum %% 10==5 ~ 15,
              df3$talknum %% 10==6 ~ 15))
df3$time_slot <- floor(df3$talknum/10)
# count occurrences, merge count table with attendee table
df4 <- left_join(df2, df3, by = "topic")  
df4 <- df4[!is.na(df4$topic),]
df4 <- df4
df4 <- df4%>% arrange(attendee, rank)



## -----------------
## code from 2017

# arrange df4
df4 <- df4 %>% arrange(attendee, time_slot, count)

#remove rows with duplicate choices for the same topic
# keep if time_slot changes or if attendee changes
df4temp <- df4[1,] #save row 1 of data
df4 <- df4[ (df4$time_slot != Lag(df4$time_slot)) | (df4$attendee != Lag(df4$attendee)), ]
#note that this is problematic for the first line of datafram - get NA for lag
# now rbind row1 back in
df4 <- rbind(df4temp, df4)
df4 <- df4 %>% remove_empty_rows()

#join to bring in topic data
#already done
#df4 <- df4 %>% left_join(df3) %>% select( -n) %>% arrange(attendee, topic, choice)

#some missing rows - some of the 138 people with no choices for particular topic
table(df4$time_slot)

# fill in missing rows for patients who selected no choices in a particular topic
df4 %>% complete(attendee, time_slot) %>% arrange(attendee, time_slot) -> df5

#map differences between df5 and df4
#discrep <- mapply(setdiff, df5, df4)
table(df5$time_slot)

#list filled in (but empty) rows
df5 %>% filter(is.na(topic)) %>% arrange(time_slot) %>% print(n=Inf)

#count which topics underfilled
df5 %>% dplyr::count(topic) %>% 
  arrange(desc(n)) %>% 
  print(n=Inf)

#sort out which time_slots have NAs
df5 %>% filter(is.na(topic)) %>% 
  select(topic, time_slot) %>% 
  group_by(time_slot) %>% 
  summarise(count = n())

#fill missing topics with low subscribed topics
#time_slot 1
# one extra for Meditate
# 9 NAs
# all to Diets 2
vec1 <- df5$topic== "Guided Meditation Exercise" & df5$time_slot==1
df5$topic[which(vec1)[1]] <- "Diets and Dietary Research in IBD 2"
df5$topic[is.na(df5$topic) & df5$time_slot==1]<- "Diets and Dietary Research in IBD 2"
vec10 <- (df5$topic=="Diets and Dietary Research in IBD 1" & df5$time_slot==1)
df5$topic[which(vec10)[1:25]]<- "Research Update: What is the Future in IBD?"
df5$topic[which(vec10)[26:41]]<- "Diets and Dietary Research in IBD 2"

#fill missing topics with low subscribed topics
#time_slot 2
# 8 extra for Wish
# 30 NAs
# 13 to IBD Emo A
# 10 to Bones
# 7 to Diet Research 1
vec2 <- df5$topic== "What I Wish I Knew When I Was First Diagnosed"& df5$time_slot==2
df5$topic[which(vec2)[1:4]] <- "Diets and Dietary Research in IBD 1"
df5$topic[which(vec2)[5:8]] <- "Protecting Your Bones in IBD"
vec3 <-((is.na(df5$topic)) & (df5$time_slot==2))
df5$topic[which(vec3)[1:13]] <- "IBD and Your Emotional Health A"
df5$topic[which(vec3)[14:24]] <- "Protecting Your Bones in IBD"
df5$topic[which(vec3)[25:30]] <- "Diets and Dietary Research in IBD 1"
df5$topic[df5$topic=="Diets and Dietary Research in IBD 2"] <-
  "Diets and Dietary Research in IBD 1"
#fill missing topics with low subscribed topics
#time_slot 4
# 3 extra for fastpass
# 2 extra for support fam
# 14 NAs for time_slot 4
# 3 to IBD Emo B
# 10 to New IBD Res
# 6 to Diet Growth
vec4 <- df5$topic== "Fast Pass to the Next Generation of IBD Treatment" & 
        df5$time_slot==4
df5$topic[which(vec4)[1:3]] <- "IBD and Your Emotional Health B"
vec5 <- df5$topic== "Supporting a Partner or Family Member with IBD" & 
  df5$time_slot==4
df5$topic[which(vec5)[1:2]] <- "New IBD Research You Need To Know About"
vec6 <-((is.na(df5$topic)) & (df5$time_slot==4))
df5$topic[which(vec6)[1:8]] <- "New IBD Research You Need To Know About"
df5$topic[which(vec6)[9:14]] <- "Diet, Growth, and Puberty in IBD"


#fill missing topics with low subscribed topics
#time_slot 5
# 1 extra for med panel
# 4 extra for transitions
# 27 NAs for time_slot 5
# 9 to Grocery
# 24 to Alt Rx
vec7 <- df5$topic== "Patient Medication Panel" & 
  df5$time_slot==5
df5$topic[which(vec7)[1]] <- "How to Grocery Shop with IBD"
vec8 <- df5$topic== "Dealing with Transitions: School, College, Adult Care" & 
  df5$time_slot==5
df5$topic[which(vec8)[1:4]] <- "How to Grocery Shop with IBD"
vec9 <-((is.na(df5$topic)) & (df5$time_slot==5))
df5$topic[which(vec9)[1:4]] <- "How to Grocery Shop with IBD"
df5$topic[which(vec9)[5:27]] <- "Alternative Therapies in IBD"

# note that filling in topics has left some NAs
# fix by merging with join
# have not figured this one out.

# df25 <- df2 %>% select(attendee, order_date, first_name, last_name, email)
# df5 <- semi_join(df5, df25, by="attendee")
# 
# df35 <- df3 %>% select(topic, count, rank, talknum, room, room_limit, time_slot)
# df5 <- semi_join(df5, df35, by="topic")

#check each session
table(df5$topic[df5$time_slot==1])
table(df5$topic[df5$time_slot==2])
 
table(df5$topic[df5$time_slot==4])
table(df5$topic[df5$time_slot==5])

#clean up attendee table
attend <- df %>% select(1:5) 

#clean up topics table
topics <- df3 %>% rename(votes = count)

#set up schedule table
schedule <- df5 %>% select(attendee, time_slot, topic)

#set up breakouts
breakouts <- attend %>% select(attendee)
breakouts$time_slot <- 3
breakouts$topic <- "Lunch with Breakout Group"

#set up rooms
rooms <- df5 %>% select(topic) %>% distinct() %>% arrange(topic) %>% pull()
rooms[22] <- "Diets and Dietary Research in IBD 2"
rooms[23] <- "Lunch with Breakout Group"
talknum <- c(51,55,45,21,42,53,13,14,52,23,44,46,41,25,54,22,11,43,24,26,15,12,30)
roomsdf <- data.frame(rooms, talknum) %>% arrange(talknum)
names(roomsdf) <- c("topic", "talknum")
roomsdf<- roomsdf %>% mutate(room = 
          case_when(roomsdf$talknum %% 10 == 1 ~ "Forum Hall",
                    roomsdf$talknum %% 10==2 ~ "Great Lakes Central",
                    roomsdf$talknum %% 10==3 ~ "Great Lakes North",
                    roomsdf$talknum %% 10==4 ~ "Great Lakes South",
                    roomsdf$talknum %% 10==5 ~ "Boardroom 1",
                    roomsdf$talknum %% 10==6 ~ "Boardroom 2",
                    roomsdf$talknum %% 10==0 ~ "Atrium and Breakout Rooms"))


#bind_rows to get breakouts into schedule
schedule <- bind_rows(schedule, breakouts)
schedule <- schedule %>% group_by(attendee) %>% arrange(attendee, time_slot)
##--------------


#add rooms - no data on what breakouts they want this year
# df6$room<- ""
# df6$room[df6$breakout=="Breakout for Crohn's Disease"]<-"Great Lakes North"
# df6$room[df6$breakout=="Breakout for Ulcerative Colitis"]<-"Great Lakes Central"
# df6$room[df6$breakout=="Breakout for Family and Friends"]<-"Great Lakes South"
# df6$room[df6$breakout=="Breakout for J pouch and Ostomy"]<-"Boardroom 1"
# df6$room[df6$breakout=="Other or prefer not to say"]<-"Atrium for Lunch"

#add time to topics
#create time intervals table
time <- c("10:40-11:10", "11:20-11:50", "12:10-1:00", "1:10-1:40", 
          "1:50-2:20")
time_slot <- 1:5
time_intervals <- data.frame(time, time_slot)

#merges to build up full table
sched2 <- left_join(schedule, attend, by = "attendee")
sched3 <- left_join(sched2, time_intervals)
sched4 <- left_join(sched3, roomsdf, by= "topic") %>%  select(-talknum)

# now nest the schedule part, create
# table df7 with attendee, session=time_slot, topic, time, room in that order
# arrange by attendee, time_slot
# then df7 %>% nest(-attendee) -> sched_nested
df7 <- sched4 %>% select(attendee, Session = time_slot, Topic = topic, 
                         Time = time, Room = room)
df7 %>% nest(-attendee)  -> sched_nested

#then form new df, df8
attend %>% select(attendee,first_name, last_name) -> df8
colnames(df8) <- c("attendee", "firstname", "lastname")

# join nested schedule with names
left_join(sched_nested, df8) %>% arrange(lastname) -> df9

#about 30 walkups each year - what to do with them?
# add a schedule to give to late arrivals - put all in largest room
# Forum Hall can handle the overflow
# everyone gets same schedule - print 30
df9[140,1] <-  930661199
df9[140,2] <-  df9[139,2]
df9[140,3] <-  "I-forgot-to-sign-up"
df9[140,4] <-  "But-I-am-here!"
walkups <- unnest(df9[140,2])
walkups[c(1:2, 4:5), 4]<- "Forum Hall"
walkups[c(1:5), 2]<- c("Research Update: What is the Future in IBD?",
                       "Diets and Dietary Research in IBD 1",
                       "Lunch with Breakout Group",
                       "New IBD Research You Need To Know About",
                       "Alternative Therapies in IBD")
df9[140,2] <-  nest(walkups)
#now make printable schedules
# after Mine C-R at rmarkdown.rstudio.com/articles_mail_merge.html
#using df9 as input data

for (i in 1:nrow(df9)) {
  rmarkdown::render(
    input = "schedules.Rmd",
    output_format = "word_document",
    output_file = paste("schedule_", i, ".doc", sep = "")
  )
}


