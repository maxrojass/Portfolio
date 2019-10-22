library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(formattable)
library(wordcloud)
library(RWeka)
library(qdap)
library(tm)

spotify_data <- finaltracks
daily_spotify <- spotifydata
#info about dataframes
glimpse(spotify_data)
summary(spotify_data)
#transform milisecond into minutes
spotify_data$duration_ms <- (spotify_data$duration_ms / 1000/60)
names(spotify_data)[names(spotify_data) == "duration_ms"]<- "duration(min)"
# Data Analysis

#Artists on the Top 200 Songs List with more than 5 songs
#using joins and pipes
top_artists <- spotify_data %>%
  group_by(Artist)  %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 5) %>%
  arrange(desc(n_apperance))

#in order to visualise the list in descending order 
top_artists$Artist <- factor(top_artists$Artist, levels = top_artists$Artist[order(top_artists$n_apperance)]) 
#graph the info
ggplot(top_artists, aes(x = Artist, y = n_apperance)) +
  geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) + 
  labs(title = "Top Artists in swedish summer 2019", x = "Artist", y = "Number of Apperance on the Top 200 during summer 2019") +
  theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
  coord_flip()

## Top Artists by the Total Playing Time
se_daily_spotify <- daily_spotify %>%
  group_by("Track") %>%
  summarise(total_streams = sum(Streams))

#names(daily_spotify)[1] <- paste("name") # in order to make the joining easier

top_by_playtime <- spotify_data %>%
  left_join(se_daily_spotify, by = "Track") %>%
  select(Track, Artist, duration, total_streams) %>%
#in order to convert seconds into hours
  mutate(total_time = duration * total_streams / 60) 

top20_by_playtime <-  top_by_playtime %>%
  group_by(Artist)  %>%
  summarise(n_time = sum(total_time)) %>%
  arrange(desc(n_time)) %>%
  top_n(20)

top20_by_playtime$Artist <- factor(top20_by_playtime$Artist, levels = top20_by_playtime$Artist [order(top20_by_playtime$n_time)]) # in order to visualise the list in descending order

ggplot(top20_by_playtime, aes(x=Artist, y=n_time, color=Artist)) +
  geom_point(size=3) + 
  geom_segment(aes(x=Artist,xend=Artist, y=0, yend=n_time)) +
  labs(title = "Top Artists of 2019 by Playing time") +
  theme_bw() +
  theme(legend.position = 'none', plot.title = element_text(size=17,hjust = -0.7, face = "bold"), axis.title.y = element_text(face = "bold"), axis.title.x = element_text(angle = 120)) +
  coord_flip()

#tracks that stay longer in the top 200
top_tracks <- daily_spotify %>%
  group_by(Track)  %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 91) %>%
  arrange(desc(n_apperance))
#graph the info
top_tracks$Track <- factor(top_tracks$Track, levels = top_tracks$Track[order(top_tracks$Track$n_apperance)]) 
ggplot(top_tracks, aes(x = Track, y = n_apperance)) +
  geom_bar(stat = "identity",  fill = "darkgreen", width = 0.6 ) + 
  labs(title = "Songs that stay longer in top 200", x = "Track", y = "Number of days in the top 200") +
  theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
  coord_flip()
# Get the two more listened tracks 
top2_tracks <- daily_spotify %>%
  group_by(Track)  %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 93) %>%
  arrange(desc(n_apperance))
#graph the info
top2_tracks$Track <- factor(top2_tracks$Track, levels = top2_tracks$Track[order(top2_tracks$Track$n_apperance)]) 
ggplot(top2_tracks, aes(x = Track, y = n_apperance)) +
  geom_bar(stat = "identity",  fill = "blue", width = 0.6 ) + 
  labs(title = "top 2 songs during summer 2019", x = "Track", y = "Number of days in the top 200") +
  theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
  coord_flip()