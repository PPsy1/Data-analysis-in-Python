library(sqldf)
library(ggplot2)
library(dplyr)
library(zoo)
library(reshape2)
library(scales)

us=read.csv("users.csv")
ev=read.csv("events.csv")
us$install_date = as.Date(a$install_date, "%Y-%m-%d")
all_=merge(us,ev,by = "user_id")

#Monthly active users

qq=sqldf("
         select user_id,app,event_timestamp, strftime('%Y-%m', event_timestamp) as dt, app_version, count(strftime('%Y%m', event_timestamp)) as count
         from all_
         group by user_id, strftime('%Y-%m', event_timestamp), app_version
         having count > 1
         ")

az=sqldf("select count(*) as nu,app_version,dt from qq group by strftime('%m',event_timestamp),app_version ")
head(az)

az$year = substr(az$dt, 1, 4)
az$month = substr(az$dt, 6,7)

g = ggplot(az, aes(x = month, y = nu)) +
  geom_bar(stat="identity") +
  facet_grid(app_version ~ year) + 
  xlab("Month") +
  ylab("Count") +
  ggtitle("Count by Month, Year & App Version")
g

#
#Total Number of unique Users


unique_u=sqldf('select count(*) as "unique_usr",app_version from us group by app_version')


p<-ggplot(data=unique_u, aes(x=app_version, y=unique_usr,fill=app_version)) +
  geom_bar(stat="identity") + labs(title='Number of Unique Users',x='App Version',y='Unique Users')+ theme(plot.title = element_text(hjust = 0.5))

p

tpap= sqldf('select app_version,app,round(avg(time_seconds),0) as time_app from all_ group by app,app_version')
head(tpap)
sw<-ggplot(data=tpap, aes(x=app, y=time_app,fill=app)) +
  geom_bar(stat="identity")  +facet_grid(app_version ~. )+labs(title='Session Time',subtitle='In each part of the app',x='App Part',y='Seconds')+ theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
sw
#Retention rates

#write.csv(az, file = "Data1.csv")

df <- read.csv('Data1', header = TRUE) %>%
  mutate(month = format(as.Date(event_timestamp), format = "%Y-%m"))

installs <- #calculates all installs for all versions of the app by month
  df %>% 
  group_by(user_id) %>%
  slice(1) %>%
  group_by(month) %>%
  summarise(tot_installs = n())

last_use_date <- #finds the last time a user actually used any version of the app (i.e., when they "churned" away)
  df %>%
  group_by(user_id, month) %>%
  summarise(tot_uses = n()) %>%
  group_by(user_id) %>%
  filter(month == max(month)) %>%
  group_by(month) %>%
  summarise(stopped_using = n())

installs %>%
  full_join(last_use_date) %>%
  mutate(cum_sum_install = cumsum(tot_installs), 
         cum_sum_stopped = cumsum(stopped_using), 
         Churn = cum_sum_stopped/cum_sum_install, 
         Retention = 1 - Churn) %>%
  select(month, Churn, Retention) %>%
  melt(id.vars = "month") %>% # melt the data frame for easy plotting
  ggplot(aes(x = month, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "", values = c("red", "blue")) +
  labs(x = "Month", y = "") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#ANOVA for session time per version
fit <- aov(w$time_seconds ~ w$app_version, data=all_) 
summary(fit)
TukeyHSD(fit)

