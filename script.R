#loading "The Rolling Stones" dataset
dataset <- read.csv('stones.csv',stringsAsFactors = F)

library(dplyr)
grouped.songs.year.rel <- dataset %>% 
  group_by(Year.Released) %>%
  summarize(Count = n())

library(ggplot2)
ggplot(grouped.songs.year.rel, aes(x = `Year.Released`, y = Count)) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Godina", y = "Broj pesama", title = "Broj izdatih pesama po godinama") +
# Customize the x-axis
scale_x_continuous(breaks = seq(min(grouped.songs.year.rel$`Year.Released`), max(grouped.songs.year.rel$`Year.Released`), by = 5))+
theme_bw()

dataset[dataset$Year.Released == 1970,1]

# number of positions that song has achieved
count.positions <- numeric()
for (row in 1:nrow(dataset)) {
  count.row <- sum (dataset[row, 22:34] != '-')
  count.positions <- c(count.positions, count.row)
}
dataset$PositionsCount <- count.positions
# sorting songs by position count
sorted.songs <- dataset[order(-dataset$PositionsCount),]
top10.song <- head(sorted.songs[,c(1,36)],8)
top10.song
ggplot(top10.song, aes(x = `Title`, y = `PositionsCount`), width = 6, height = 4) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ime pesme", y = "Broj pozicija na top listama", title = "Top 10 pesama po broju plasmana na top liste") +
  scale_y_continuous(breaks =seq(0,12,by=1) )
  theme_bw()

# grouping songs by album
grouped.songs.albumName <- dataset %>% 
    group_by(Album.name) %>%
    summarize(Count = n())
# top 5 albums with most songs
grouped.songs.albumName <- head(grouped.songs.albumName[order(-grouped.songs.albumName$Count),],5)

ggplot(grouped.songs.albumName, aes(x = `Album.name`, y = `Count`)) +
  geom_bar(stat = "identity", width = 0.4) +
  labs(x = "Ime albuma", y = "Broj pesama na albumu", title = "Top 5 albuma po broju pesama") +
  scale_y_continuous(breaks =seq(0,12,by=1) )+
theme_bw()

# grouping by album type
grouped.songs.albumType <- dataset %>% 
  group_by(Album.type) %>%
  summarize(Count = n())
# pie chart for album type 
grouped.songs.albumType$Percentage <- (grouped.songs.albumType$Count / sum(grouped.songs.albumType$Count)) * 100
  
 ggplot(grouped.songs.albumType, aes(x = "", y = `Count`, fill = `Album.type`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Broj pesama po tipu albuma")+
   geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))
# lists for each value in Certification column
 unique(dataset$Certification)
awarded.songs <- dataset[dataset$Certification != '-',c(1,3,4,35)]
# plotting awarded songs
library(ggrepel)
ggplot(awarded.songs, mapping = aes(`Year.Released`, Certification, group = Certification, color = Album.name)) +
  ggrepel::geom_label_repel(aes(label = Title),
                            size = 2,
                            label.size = 0,
                            segment.color = NA
  ) +
  geom_point() + theme(legend.position = "bottom")  

# oldest recorded
oldest.rec <- dataset[dataset$Year.Recorded == min(dataset$Year.Recorded), 1]
oldest.rec
# youngest recorded
youngest.rec <- dataset[dataset$Year.Recorded == max(dataset$Year.Recorded), 1]
youngest.rec
# oldest released
oldest.rel <- dataset[dataset$Year.Released == min(dataset$Year.Released), 1]
oldest.rel
# youngest recorded
youngest.rec <- dataset[dataset$Year.Recorded == max(dataset$Year.Recorded), 1]
youngest.rec


 # Lead vocalist
unique(dataset$Lead.vocal.s.)
count.richards <- sapply(dataset$Lead.vocal.s., function(x) sum(grepl("Richards", x)))
total.rows.with.richards <- sum(count.richards > 0)
total.rows.with.richards
#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

sum(is.na(dataset$acousticness))

library(Amelia)
missmap(obj = dataset, main = 'Vizualizacija NA vrednosti', legend = F)

#checking missing values in dataset
apply(dataset, MARGIN = 2,FUN = function(x) sum(is.na(x)))
#Track.number has 4 NA, Song duration has 4 NA, acousticness, danceability, energy,
#instrumentalness, liveness, loudness, speechiness, tempo and valence each have 16 NA values
apply(dataset, MARGIN = 2,FUN = function(x) sum(x == "", na.rm = T))
#there are not any "" values
apply(dataset, MARGIN = 2,FUN = function(x) sum(x == " ", na.rm = T))
#the are not any " " values

dataset$Song.duration
dataset[1:3,c(1,8)]
#transforming Song duration variable in number of seconds
for (i in 1:nrow(dataset)) {
  # Split into minutes and seconds
  time.parts <- unlist(strsplit(dataset$Song.duration[i], ":"))
  # Convert minutes and seconds to numeric values
  minutes <- as.numeric(time.parts[1])
  seconds <- as.numeric(time.parts[2])
  # Calculate total 
  total.seconds <- (minutes * 60) + seconds
  print(total.seconds)
  dataset$Song.duration[i] <- total.seconds
}
#transforming Song duration in numeric variable
dataset$Song.duration <- as.numeric(dataset$Song.duration)

na.indices <- c(7,8, 11:19)
# na.indices
# colnames(dataset)[na.indices]

#checking the distribution of numeric variables that have NA values with shapiro test
apply(dataset[,na.indices], MARGIN = 2, FUN = function(x) shapiro.test(x))
#only danceability attribute has Normal distribution and in filling its missing values mean value will be used
#in other numeric attributes, that don't have Normal distribution, median value will be used

mean.danceability <- mean(dataset$danceability,na.rm = T) 
#filling NA values in danceability attribute with its mean value
dataset$danceability[is.na(dataset$danceability)] <- mean.danceability 
#checking for NA in danceability
# sum(is.na(dataset$danceability))

na.indices <- c(7,8,11,13:19)
for(i in na.indices ){
  print(colnames(dataset)[i])
  median.value <- median(dataset[,i], na.rm = T)
  print(median.value)
  # print(dataset[,i][is.na(dataset[,i])])
  dataset[,i][is.na(dataset[,i])] <- median.value
}
#checking for NA values
apply(dataset, MARGIN = 2, FUN = function(x) sum(is.na(x)))

#checking if character variables can be transformed into factor variable
length(unique(dataset$Record.Label))
dataset$Record.Label <- as.factor(dataset$Record.Label)
# levels(dataset$Record.Label)
# table(dataset$Album.type)
length(unique(dataset$Album.type))
dataset$Album.type <- as.factor(dataset$Album.type)

length(unique(dataset$Songwriter.s.))
#there are too many different values for Songwriter.s.attribute for it to be converted to factor
length(unique(dataset$Lead.vocal.s.))
# table(dataset$Lead.vocal.s.)
dataset$Lead.vocal.s. <- as.factor(dataset$Lead.vocal.s.)

table(dataset$British.charts)
dataset$British.charts <- as.factor(ifelse(test = dataset$Date != '-', yes = "Yes", no = "No"))

# Replace '-' with 'No' in columns 20 to 35
cols.to.replace <- 20:35
dataset[, cols.to.replace] <- lapply(dataset[, cols.to.replace], function(col) {
  gsub("-", "No", col)
})

get.ordinal <- function(position) {
  if (position == "No") {
    return(position)
  }
  # Remove leading zeros if they exist
  position <- gsub("^0+", "", position)
  
  last.two.digits <- as.numeric(substr(position, nchar(position) - 1, nchar(position)))
  
  if (is.na(last.two.digits)) {
    return(position)
  }
  
  if (length(last.two.digits) == 1) {
    if (last.two.digits == 1) {
      ordinal <- paste(position, "st", sep = "")
    } else if (last.two.digits == 2) {
      ordinal <- paste(position, "nd", sep = "")
    } else if (last.two.digits == 3) {
      ordinal <- paste(position, "rd", sep = "")
    } else {
      ordinal <- paste(position, "th", sep = "")
    }
  } else {
    if (last.two.digits >= 11 && last.two.digits <= 13) {
      ordinal <- paste(position, "th", sep = "")
    } else {
      last.digit <- as.numeric(substr(position, nchar(position), nchar(position)))
      if (last.digit == 1) {
        ordinal <- paste(position, "st", sep = "")
      } else if (last.digit == 2) {
        ordinal <- paste(position, "nd", sep = "")
      } else if (last.digit == 3) {
        ordinal <- paste(position, "rd", sep = "")
      } else {
        ordinal <- paste(position, "th", sep = "")
      }
    }
  }
  return(ordinal)
}
#transforming WksNo.1 variable
dataset$WksNo.1 <- NULL

names(dataset[,22:33])

dataset[12:15, c(1,22)]
#transforming attributes( 22 to 33 columns) into factor variable
for(j in 22:33) {
  variable <- dataset[,j]
  ordinal.positions.i <- sapply(variable, get.ordinal)
  # Get unique positions from the dataframe
  unique.positions.i <- unique(ordinal.positions.i)
  numeric.vector.i <- sort(as.numeric(gsub("[^0-9]", "", unique.positions.i)))
  numeric.vector.i <- sapply(numeric.vector.i, get.ordinal)
  numeric.vector.i <- c("No",numeric.vector.i)
  
  #creating factor variable
  variable.factor <- factor(ordinal.positions.i, levels = numeric.vector.i)
  #adding factor variable 
  dataset[,j] <- variable.factor
}

which(colnames(dataset) == 'UK Peak Pos')

#transforming Certification attribute
table(dataset$Certification)
dataset$Certification <- as.factor(gsub("\\[.*\\]","", dataset$Certification)) 
# levels(dataset$Certification)

#transforming British.charts attribute
table(dataset$British.charts)
dataset$British.charts <- as.factor(dataset$British.charts)

# transforming Album.name
sort(table(dataset$Album.name))
dataset$Album.name <- as.factor(dataset$Album.name)

all(complete.cases(dataset))

length(unique(dataset$Date))
# Replace '.' with ' ' in column names
colnames(dataset) <- gsub("\\.", " ", colnames(dataset))

write.csv(dataset, file = "stones_analysis.csv", row.names = FALSE)

plot(dataset$`US Rec World`)
unique(dataset$b)
dataset[dataset$AUS == '1st',1]

ggplot(data = dataset) +
  geom_bar(aes(x = US), fill = "#A505D0") +
  labs(title = "Broj pesama na US top listi", x = "Pozicije", y = "Broj Pojavljivanja")+
  scale_y_continuous(breaks = seq(0,200,by=10))

nrow(dataset[dataset$US == '1st',])

ggplot(data = dataset, mapping = aes(x = `British charts`)) +
  geom_bar(bins = 30) +
  scale_y_continuous(breaks = seq(0,300, by = 20))
  theme_minimal()
