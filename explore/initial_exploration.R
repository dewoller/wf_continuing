source("functions.R")

usage_df  <- get_usage_df()


# a term is 6 months long

# New user : a patient who did not use opioid the entire previous year
# Episodic user : the patient used opioid previously but not more than four terms continuously (during the study period)
# Continuing user : the patient used opioid in the immediate previous year and used more than four terms continuously (during the study period)
# User in a year : I used this term (user in a year) for patients who could not be ascertained with any of the above three categories.

# unit of analysis - user in a year?
# New user - over N years, have N-1 times to be a new user
# Episodic user - Over N years, patient has N-2 times to be episodic user
# - a user who has used in the past, but has had at least a 2 year period of not using
# Continuing user - user who has used for a long period in the past (once per every 6 months) over a 4 year period

# We are going to get more continuing / episodic users later in the study period, because we have more data about them
# it seems to me that we should look at users as a whole, individually

a* <- "
Questions for mofi - does it matter if they are using different opioids?  Or is any opioid as good as another?
Is there a threshold for 'usage'?  Ie, is 1 pill 'usage'?
You can't find out their YOB, because they throw away ages after 100+, and 720 pp were age > 100 ONLY during the study

use "Age at the start of the study?  Age at first drug?"

    min     |    max     
------------+------------
 2013-01-01 | 2017-03-31

 Throw away 2017 data, using 4 years of data alone?


Suggested categories:

1) users who only used during one period (eg not longer than 6 months) max - min
2) users who used for longer than 6 months but had periods of not using (at least 12 months?) k
3) users who used continiously - at least once / 6 months over 4 years, continously
"

get_usage_df() %>%
  mutate( age_group_20 = ifelse( age=="100+", "100+", paste0(sprintf("%02d", floor(as.numeric(age)/5)), "-", sprintf( "%02d", floor(as.numeric(age)/5)+4))))  %>%
  {.} -> usage_df 

# error checking
usage_df %>% 
  mutate( period = paste0("Y", substring(period,3,4), "H", substring(period,6,6))) %>%
  filter(endsWith(period, '3'))

usage_df %>% 
  mutate( period = paste0("Y", substring(period,3,4), "H", substring(period,6,6))) %>%
  filter( period != "Y17H1") %>%
  spread( key=period, value=quantity) %>%
  {.} -> usage_df_wide


usage_df_wide %>% 
 filter( pin==143567 ) %>%
  gather( key=period, value=qty, -(pin:lga)) %>%
  mutate( qty = ifelse( is.na(qty), -1, qty)) %>%
  separate( period, into=qw("year chunk"), sep='H') %>%
  filter(year < "Y17") %>%
  group_by(pin) %>%
  arrange(pin, year, chunk ) %>%
  mutate( new_user = qty>0 & year > 2013 & lag(qty) == -1 & lag( qty, 2 ) == -1,
    episodic_user = qty>0 & year > 2013 & lag(qty) == -1 & lag( qty, 2 ) >0,
    continuing_user = qty>0 & year > 2014 & lag(qty) > 0 & lag( qty, 2 ) >0 & lag( qty, 3 ) >0 & lag( qty, 4 ) >0) %>% 
  filter( new_user | episodic_user | continuing_user ) %>%
  {.} -> user_status_df


user_status_df %>%
  select(pin, year, new_user, episodic_user, continuing_user ) %>%
  gather(variable, value, -(pin:year)) %>%
  unite( temp, variable, year) %>%
  group_by( pin, temp ) %>%
  summarise( value = any(value) ) %>% 
  mutate( value = ifelse( value, value, NA)) %>%
  spread(temp, value) -> user_status_wide

  user_status_wide %>%
 filter( pin==72784 )

usage_df_wide %>%
  left_join(  user_status_wide, by="pin") %>%
  {.} -> final_usage_df

final_usage_df %>% View()

write.dta( final_usage_df, "/store/aarnet_owncloud/Shared/Opioid/Opioid/new_vs_continuing/datasets/six_monthly_usage.dta")

u nser_status_df %<>% 
  mutate( age_group = ifelse( age=="100+"
                             , "100+"
                             , paste0(sprintf("%02d", floor(as.numeric(age)/5) * 5), "-", sprintf( "%02d", (floor(as.numeric(age)/5))*5+4 ))))

user_status_df %<>% 
  mutate( age_group20 = ifelse( age=="100+"
                             , "100+"
                             , paste0(sprintf("%02d", floor(as.numeric(age)/20) * 20), "-", sprintf( "%02d", (floor(as.numeric(age)/20)+1)*20 ))))


user_status_df %>%
  filter( new_user )  %>%
  group_by(year, age_group20 ) %>%
  summarise( n = n()) %>%
  ggplot() +
  geom_line( mapping=aes(x = year, y= n, color=age_group20, group=age_group20), stat="identity")


user_status_df %>%
  filter( episodic_user )  %>%
  group_by(year, age_group20 ) %>%
  summarise( n = n()) %>%
  ggplot() +
  geom_line( mapping=aes(x = year, y= n, color=age_group20, group=age_group20), stat="identity")



user_status_df %>%
  filter( continuing_user )  %>%
  group_by(year, age_group20 ) %>%
  summarise( n = n()) %>%
  ggplot() +
  geom_line( mapping=aes(x = year, y= n, color=age_group20, group=age_group20), stat="identity")


