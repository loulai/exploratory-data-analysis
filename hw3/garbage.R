# Most likely to stop
# This is some hacky stuff!

OfficerRaces <- df %>% 
  group_by(Officer_Race) %>% 
  summarize(n())

DriverRaces <- df %>% 
  group_by(Driver_Race) %>% 
  summarize(n())

# some data cleaning required: select only certain races, eliminate outliers
# from the above, we see that the top races in both categories are Black, White, Asian, Hispanic
df$Driver_Race <- as.character(df$Driver_Race)
df$Officer_Race <- as.character(df$Officer_Race)

likelydf <- df %>% 
  select(Officer_Race, Driver_Race) %>% 
  filter(Driver_Race == "White" | Driver_Race == "Black" | Driver_Race == "Hispanic" | Driver_Race == "Asian") %>%  # grab top driver races
  filter(Officer_Race == "White" | Officer_Race == "Black/African American" | Officer_Race == "Asian / Pacific Islander" | Officer_Race=="Hispanic/Latino") %>% # grab top officer races
  mutate(Officer_Race = replace(Officer_Race, Officer_Race == "Black/African American", "Black")) %>%  # make officer races uniformly named to match drivers'
  mutate(Officer_Race = replace(Officer_Race, Officer_Race == "Asian / Pacific Islander", "Asian")) %>% 
  mutate(Officer_Race = replace(Officer_Race, Officer_Race == "Hispanic/Latino", "Hispanic")) 

likelydf <- likelydf %>% 
  group_by(Officer_Race) %>% 
  mutate(pplStopped=n())

summaryLikely <- likelydf %>% 
  group_by(Officer_Race, Driver_Race) %>% 
  mutate(percent_stopped = n()/pplStopped) %>% 
  summarize(p = 100 * mean(percent_stopped)) %>% 
  arrange(desc(p)) %>% 
  top_n(3)

likelydf %>% group_by(Driver_Race) %>% summarize(n())

hackydf <- data.frame("Officer_Race" = c("White", "Black", "Asian", "Hispanic"), 
                      "Starting" = c(1.75, 1.25, 0.75, 0.25),
                      "Ending" = c(1.25, 1.25, 1.25, 1.25))

hackydf %>% 
  ggplot() + 
  geom_point(aes(x=c(0,0,0,0), y=hackydf$Starting)) +
  geom_abline(intercept = 1.75, slope=-0.5) +
  geom_abline(intercept = 1.25, slope=0) +
  geom_abline(intercept = 0.75, slope=0.5) +
  geom_abline(intercept = 0.25, slope=1) +
  geom_point(aes(x=c(1,1,1,1), y=hackydf$Ending)) +
  xlim(0,0.99) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="Officers (by race), most likely to stop drivers of race:")


########################
```{r}
# Given that some legal action resulted, how many of those arrested are of each ethnicity?

# first, let's explore. How many legal actions were taken?
totalStops <- nrow(df) # 79,884
totalLegalActions <- df %>% filter(Result_of_Stop == "Citation Issued" | Result_of_Stop == "Arrest") %>% nrow() # 34,506
totalLegalActions/totalStops # 43% of stops result in a legal action

legalActiondf <- df %>% filter(Result_of_Stop == "Citation Issued" | Result_of_Stop == "Arrest")

df2 <- legalActiondf %>% 
  group_by(Driver_Race) %>% 
  summarize(ethnicityArrestedPercent = 100 * n()/totalLegalActions) %>% 
  arrange(desc(ethnicityArrestedPercent)) # black = 53%, White 42%

pie <- df2 %>% 
  mutate(ethnicityArrestedPercent = ethnicityArrestedPercent) %>% 
  ggplot(aes(x="", y=ethnicityArrestedPercent, fill=Driver_Race)) +
  geom_bar(width=1, stat="identity") + 
  coord_polar("y", start=0) +
  labs(title="Given arrested, what is their ethnicity?", xlab="", ylab="") +
  geom_text(aes(y = ethnicityArrestedPercent/5 + c(0, cumsum(ethnicityArrestedPercent)[-length(ethnicityArrestedPercent)]), 
                label = percent(ethnicityArrestedPercent/100)), size=3)+ 
  scale_fill_brewer()

pie

# Wow. Now, let's say you've been stopped. Given that you're X race, 
# are you more or less likely to be arrested?

# create a legal action column and select only relevant columns
legalActiondf <- df %>% 
  mutate(legal_action_taken = Result_of_Stop == "Citation Issued" | Result_of_Stop == "Arrest") %>% 
  select(-1, -5, -7)

likelihood_to_be_arrested_given_race <- legalActiondf %>% 
  group_by(Driver_Race) %>% 
  summarize(percent_legal_action_taken = 100 * sum(legalAction == T)/n()) %>% 
  ungroup() %>% 
  arrange(desc(percent_legal_action_taken))

likelihood_to_be_arrested_given_race
pie2 <- df2 %>% 
  mutate(ethnicityArrestedPercent = ethnicityArrestedPercent) %>% 
  group_by(ethnicityArrestedPercent) 

pie2

# Given ethnicity, how likely are you to be stopped? Actually can't answer this! Coz we only see people who have already been stopped

###################################################################
# Data Exploration
glimpse(df) # 79884 observations, 12 variables

# How many officers, by race
df %>% 
  count(Officer_Race) %>% 
  arrange(desc(n)) 

df %>% 
  mutate(Officer_Race = Officer_Race %>% fct_infreq()) %>% 
  ggplot(aes(Officer_Race)) +
  geom_bar() +
  xlab("Race of the Officer") +
  ylab("Number of Officers") +
  labs(title="Count of Officers by Race") +
  scale_y_continuous(breaks=seq(0, 60000, 10000), labels=comma_format()) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    panel.background = element_rect(fill="lightblue"),
    panel.grid.minor = element_line(size=0.25, linetype='solid',
                                    color='white'),
    text=element_text(family="Helvetica"))

# How many drivers stopped, by race
df %>% 
  count(Driver_Race) %>% 
  arrange(desc(n))

df %>% 
  mutate(Driver_Race = Driver_Race %>% fct_infreq()) %>% 
  ggplot(aes(Driver_Race)) +
  geom_bar() +
  xlab("Race of the Driver") +
  ylab("Number of Drivers") +
  labs(title="Count of Drivers by Race") +
  scale_y_continuous(breaks=seq(0, 50000, 10000), labels=comma_format()) +
  theme_classic() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    panel.background = element_rect(fill="lightblue"),
    panel.grid.minor = element_line(size=0.25, linetype='solid',
                                    color='white'),
    text=element_text(family="Helvetica"))

# we see that it's mostly white officers, by a large amount. Come back and see percentages.
```

```{r}
# Do officers have a biases towards stopping certain races?

# how many stops per officer race?
dfStoppedRace <-  df %>% 
  group_by(Officer_Race) %>% 
  mutate(driver_stops_made = sum(n())) %>% 
  ungroup()

# get a count of how many stops made, by ethnicity
dfStoppedRaceSummarized <-  df %>% 
  group_by(Officer_Race) %>% 
  summarize(driver_stops_made = sum(n())) %>% 
  arrange(desc(driver_stops_made)) %>% 
  top_n(5)


dfStoppedRace %>% 
  group_by(Officer_Race, Driver_Race) %>% 
  summarize(percent_race_stopped = 100 * n()/driver_stops_made[1]) %>% 
  ungroup() %>%
  group_by(Officer_Race) %>% 
  arrange(desc(percent_race_stopped), .by_group=T) 

dfStoppedRace$driver_stops_made[2]

# What percent of stops of people actually lead to an arrest?
# 42

# Driver race ~ officer race <<<<<<<<< good! Could add count but meh
dfStoppedRace %>% 
  mutate(Officer_Race = Officer_Race %>% fct_infreq()) %>% 
  ggplot(aes(Officer_Race)) +
  geom_bar(aes(fill=Driver_Race)) + 
  scale_fill_brewer(palette="Green", name="Driver Race") +
  theme(axis.text.x=element_text(angle=10, hjust=1)) +
  xlab("Officer Race") + 
  ylab("Count of Drivers Stopped")

#############################################################################################
# What about initial Reason for Stop?
likelyAlluvia2 <- df %>% 
  mutate(legal_action_taken = Result_of_Stop == "Citation Issued" | Result_of_Stop == "Arrest") %>% 
  group_by(legal_action_taken, Reason_for_Stop, Driver_Gender) %>% 
  summarise(Frequency=n())

is_alluvia_form(likelyAlluvia2, axes=1:3, silent=TRUE)

likelyAlluvia2 %>% 
  ggplot(aes(y = Frequency, axis2 = Driver_Gender, axis1=Reason_for_Stop)) +
  geom_alluvium(aes(fill = legal_action_taken), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Gender", "Reason for Stop"), expand = c(.1, .1)) +
  scale_fill_brewer(type = "qual", palette = "Set1", name="Legal Action Taken") +
  ggtitle("Was Legal Action Taken? By Initial Reason for Stop and Gender")

