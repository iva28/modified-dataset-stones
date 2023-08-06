#loading "The Rolling Stones" dataset
dataset <- read.csv('stones.csv',stringsAsFactors = F)

#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

#checking missing values in dataset
apply(dataset, MARGIN = 2,FUN = function(x) sum(is.na(x)))
#Track.number has 4 NA, Song duration has 4 NA, acousticness, danceability, energy,
#instrumentalness, liveness, loudness, speechiness, tempo and valence each have 16 NA values
apply(dataset, MARGIN = 2,FUN = function(x) sum(x == "", na.rm = T))
#there are not any "" values
apply(dataset, MARGIN = 2,FUN = function(x) sum(x == " ", na.rm = T))
#the are not any " " values

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

#checking the distribution of variables that have NA values with shapiro test
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

#transforming attributes( 22 to 34 columns) into factor variable
for(i in 22:33) {
  variable <- dataset[,i]
  ordinal.positions.i <- sapply(variable, get.ordinal)
  # Get unique positions from the dataframe
  unique.positions.i <- unique(ordinal.positions.i)
  numeric.vector.i <- sort(as.numeric(gsub("[^0-9]", "", unique.positions.i)))
  numeric.vector.i <- sapply(numeric.vector.i, get.ordinal)
  numeric.vector.i <- c("No",numeric.vector.i)
  
  #creating factor variable
  variable.factor <- factor(ordinal.positions.i, levels = numeric.vector.i)
  #adding factor variable 
  dataset[,i] <- variable.factor
}

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

