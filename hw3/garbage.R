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
