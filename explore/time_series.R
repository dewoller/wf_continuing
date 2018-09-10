

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


results:
"

source("functions.R")


get_continuing_rr_df() %>%
  group_by(pin, item_code, generic_name) %>%
  arrange( pin, item_code, supply_date ) %>%
  mutate( difference = supply_date - lag(supply_date)
         , rate = (quantity * days) / as.integer(difference )
         ) %>%
  filter( !is.na( difference)) %>%
  arrange(pin) %>%
  ungroup() %>%
  group_by( pin, item_code, generic_name ) %>%
  summarise( sd = sd( rate )
            , mn = mean( rate )
            , n=n()
            , first = min(supply_date)
            , last = max(supply_date)
            , len = last - first 
            ) %>%
  arrange((sd)) %>%
  filter( n > 5 ) %>% View

get_continuing_rr_df() %>%
filter(pin=='448216859')

  mutate( age_group_20 = ifelse( age=="100+", "100+", age_grouping( age, 20 )))  %>%
  {.} -> usage_df 



usage_df %>% 
  head() -> a



geom_histogram()




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


