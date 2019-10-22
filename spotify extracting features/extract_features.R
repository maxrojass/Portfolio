library(dplyr)
library(stringr)
library(spotifyr)
library(ggplot2)
library(tidyr)
#Remove duplicate rows of the dataframe considering the track name as a variable
newspotifydata <- distinct(spotifydata,Track, .keep_all= TRUE)
#Remove column X which is duplicated in the dataframe
newspotifydata <- subset(newspotifydata, select = -X)
#login into the spotify's API with credentials
client_id <- "YOUR_CLIEND_ID"
client_secret <- "YOUR_CLIENT_SECRET"
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
#create a list of the artist in the top 200 in the selected period
artists <- newspotifydata$Artist %>% unique()
artists <- as.data.frame(artists)
#create a dataframe for the tracks
tracks <- newspotifydata
#remove url to only have the uri of each track
tracks$id <- gsub("https://open.spotify.com/track/", "", tracks$id)
#dividing the list in 7 because maximum require is 100 id's
tracks2 <- tracks$id[c(1:100)]
tracks3 <- tracks$id[c(101:200)]
tracks4 <- tracks$id[c(201:300)]
tracks5 <- tracks$id[c(301:400)]
tracks6 <- tracks$id[c(401:500)]
tracks7 <- tracks$id[c(501:600)]
tracks8 <- tracks$id[c(601:677)]
#extract features of the songs in the previous lists
trackfeatures1 <- get_track_audio_features(tracks2, authorization = get_spotify_access_token())
trackfeatures2 <- get_track_audio_features(tracks3, authorization = get_spotify_access_token())
trackfeatures3 <- get_track_audio_features(tracks4, authorization = get_spotify_access_token())
trackfeatures4 <- get_track_audio_features(tracks5, authorization = get_spotify_access_token())
trackfeatures5 <- get_track_audio_features(tracks6, authorization = get_spotify_access_token())
trackfeatures6 <- get_track_audio_features(tracks7, authorization = get_spotify_access_token())
trackfeatures7 <- get_track_audio_features(tracks8, authorization = get_spotify_access_token())
#append all the list in one 
trackfeatures <- rbind(trackfeatures1, trackfeatures2, trackfeatures3, trackfeatures4, trackfeatures5, trackfeatures6, trackfeatures7)
#change name of column Url to id in tracks dataframe
names(tracks)[names(tracks) == "Url"] <- "id"
#mix track features with the previous tracks dataframe
finaltracks <- merge(tracks, trackfeatures, by = "id")
#to remind 
#finaltracks has song names, artist and features, streams and duration
#spotifydata has all the lsit in the timeframe without features
#newspotifydata artist,songs, streams withouth timeframe
#save tibble into a .csv for analysis
write.csv(finaltracks,"finaltracks.csv") 




