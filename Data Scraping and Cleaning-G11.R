#The data being generated below is a part of BTMA 636 (Data Analytics I) Mini Project done by 
#Suhas Lal in Fall 2020. This data is used as a part of Final Project in OPMA 631
#(Predictive Analytics in Business) in Winter 2021 by Suhas Lal
#as per the verbal confirmation of course instructor Dr. Alireza Sabouri Bagh Abbas.


#Loading the libraries. You have to ensure these packages are installed. If not, just use 
#install.packages('package name')
library('rvest')
library('ggplot2')
library('stringr')

#The following code scrapes the player database website of FIFA 21 game to retrieve the top 100 players 
#based on their real world performance. The player ID, Name, Age, Nationality, Overall Rating and the 
#Team are scraped from the webpage along with the ratings of their individual skills by accessing them 
#from the URL each player.

#Specifying the urls for the website to be scraped - fifaindex, where the top 100 players are spread 
#across 4 different URLs
url_page1 <- 'https://www.fifaindex.com/players/top/1/'
url_page2 <- 'https://www.fifaindex.com/players/top/2/'
url_page3 <- 'https://www.fifaindex.com/players/top/3/'
url_page4 <- 'https://www.fifaindex.com/players/top/4/'

#Defining a function to get the top 100 players across four different URLs.
getPlayerData <- function(url) {
  #Reading the HTML code from the website
  webpage <- read_html(url)
  
  #Scraping the webpage to retrieve the Player ID from the attribute within a row of the table called
  #data-playerid
  player_id <- webpage %>% 
    html_nodes('table.table-players') %>%
    html_nodes('tbody') %>%
    html_nodes('tr') %>%
    html_attr('data-playerid')
  
  #Scraping the webpage to retrieve the Nationality from the attribute within a row of the table called
  #title
  player_nationality <- webpage %>% 
    html_nodes('table.table-players') %>%
    html_nodes('tr') %>%
    html_nodes('[data-title = Nationality]') %>%
    html_nodes('a') %>%
    html_attr('title')
  
  for(i in 1:length(player_nationality)) {
    if(player_nationality[i] == 'England' | player_nationality[i] == 'Scotland' |
       player_nationality[i] == 'Wales' | player_nationality[i] == 'Ireland') {
      player_nationality[i] <- 'United Kingdom'
    }
    if(player_nationality[i] == 'Ivory Coast') {
      player_nationality[i] <- 'Cote d\'Ivoire'
    }
    if(player_nationality[i] == 'Korea Republic') {
      player_nationality[i] <- 'South Korea'
    }
  }

  #Scraping the webpage to retrieve the Name from the attribute within a row of the table called
  #title and removing the FIFA 21 suffix
  player_names <- webpage %>% 
    html_nodes('table.table-players') %>%
    html_nodes('tr') %>%
    html_nodes('[data-title = Name]') %>%
    html_nodes('a') %>%
    html_attr('title')
  player_names <- str_remove(player_names, 'FIFA 21')
  
  #Scraping the webpage to retrieve the Rating from the span with class name rating
  player_rating <- webpage %>% 
    html_nodes('table.table-players') %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_nodes('span.rating') %>%
    html_text()
  #Code retrieved from 
  #https://stackoverflow.com/questions/13461829/select-every-other-element-from-a-vector
  player_rating <- player_rating[c(TRUE, FALSE)]
  
  #Scraping the webpage to retrieve the Age from the row of table containing a node with attribute
  #data-title = Age
  player_age <- webpage %>% 
    html_nodes('table.table-players') %>%
    html_nodes('tr') %>%
    html_nodes('[data-title = Age]') %>%
    html_text()
  
  #Scraping the webpage to retrieve the Team from the attribute within a row of the table called
  #title and removing the FIFA 21 suffix
  player_team <- webpage %>% 
    html_nodes('table.table-players') %>%
    html_nodes('tr') %>%
    html_nodes('[data-title = Team]') %>%
    html_nodes('a') %>%
    html_attr('title')
  player_team <- str_remove(player_team, 'FIFA 21')
  
  #Creating a dataframe to store all the scraped data based on their characteristics
  player_table <- data.frame('Id' = player_id, 'Name' = player_names, 'Nationality' = player_nationality,
                             'Age' = as.numeric(player_age), 'Team' = player_team, 
                             'Overall Rating' = as.numeric(player_rating))
  
  return(player_table)
}

#Calling the function defined above to get 4 different data frames of the top 100 FIFA players
player_set1 <- getPlayerData(url_page1)
player_set2 <- getPlayerData(url_page2)
player_set3 <- getPlayerData(url_page3)
player_set4 <- getPlayerData(url_page4)

#Combining all the previous dataframes into a single one
player_data <- rbind(player_set1, player_set2, player_set3, player_set4)

#Defining empty vectors to store the individual ratings of the players
ball_control <- NULL   
dribbling <- NULL
marking <- NULL 
slide_tackle <- NULL
stand_tackle <- NULL
aggression <- NULL
reactions <- NULL
att_position <- NULL
interceptions <- NULL
vision <- NULL
composure <- NULL
crossing <- NULL
short_pass <- NULL
long_pass <- NULL
acceleration <- NULL
stamina <- NULL
strength <- NULL
balance <- NULL
sprint_speed <- NULL
agility <- NULL
jumping <- NULL
heading <- NULL
shot_power <- NULL
finishing <- NULL
long_shots <- NULL
curve <- NULL
fk_acc <- NULL
penalties <- NULL
volleys <- NULL
gk_positioning <- NULL
gk_diving <- NULL
gk_handling <- NULL
gk_kicking <- NULL
gk_reflexes <- NULL

for (i in 1:NROW(player_data)) {
  
  #Generating the URL for each player in the top 100 by adding the player ID to the end of the initial URL
  url <- paste('https://www.fifaindex.com/player/',player_data[i, 1], '/', sep = '')
  webpage <- read_html(url)
  
  print(paste('Retrieving player', i))
  
  #Scraping the webpage to generate a list of ratings whose order is apparent from the webpage
  player_stats <- webpage %>%
    html_nodes('div.row.grid') %>%
    html_nodes('div.item') %>%
    html_nodes('div.card') %>%
    html_nodes('div.card-body') %>%
    html_nodes('span.float-right') %>%
    html_text()
  
  #Storing the individual ratings in their respective vectors for each player
  ball_control <- c(ball_control, as.numeric(player_stats[1]))
  dribbling <- c(dribbling, as.numeric(player_stats[2]))
  marking <- c(marking, as.numeric(player_stats[3]))
  slide_tackle <- c(slide_tackle, as.numeric(player_stats[4]))
  stand_tackle <- c(stand_tackle, as.numeric(player_stats[5]))
  aggression <- c(aggression, as.numeric(player_stats[6]))
  reactions <- c(reactions, as.numeric(player_stats[7]))
  att_position <- c(att_position, as.numeric(player_stats[8]))
  interceptions <- c(interceptions, as.numeric(player_stats[9]))
  vision <- c(vision, as.numeric(player_stats[10]))
  composure <- c(composure, as.numeric(player_stats[11]))
  crossing <- c(crossing, as.numeric(player_stats[12]))
  short_pass <- c(short_pass, as.numeric(player_stats[13]))
  long_pass <- c(long_pass, as.numeric(player_stats[14]))
  acceleration <- c(acceleration, as.numeric(player_stats[15]))
  stamina <- c(stamina, as.numeric(player_stats[16]))
  strength <- c(strength, as.numeric(player_stats[17]))
  balance <- c(balance, as.numeric(player_stats[18]))
  sprint_speed <- c(sprint_speed, as.numeric(player_stats[19]))
  agility <- c(agility, as.numeric(player_stats[20]))
  jumping <- c(jumping, as.numeric(player_stats[21]))
  heading <- c(heading, as.numeric(player_stats[22]))
  shot_power <- c(shot_power, as.numeric(player_stats[23]))
  finishing <- c(finishing, as.numeric(player_stats[24]))
  long_shots <- c(long_shots, as.numeric(player_stats[25]))
  curve <- c(curve, as.numeric(player_stats[26]))
  fk_acc <- c(fk_acc, as.numeric(player_stats[27]))
  penalties <- c(penalties, as.numeric(player_stats[28]))
  volleys <- c(volleys, as.numeric(player_stats[29]))
  gk_positioning <- c(gk_positioning, as.numeric(player_stats[30]))
  gk_diving <- c(gk_diving, as.numeric(player_stats[31]))
  gk_handling <- c(gk_handling, as.numeric(player_stats[32]))
  gk_kicking <- c(gk_kicking, as.numeric(player_stats[33]))
  gk_reflexes <- c(gk_reflexes, as.numeric(player_stats[34]))
}

#A dataframe is created with all the individual ratings of the 100 players 
player_stats <- data.frame('Ball Control' = ball_control, 'Dribbling' = dribbling, 'Marking' = marking,
                           'Slide Tackle' = slide_tackle, 'Stand Tackle' = stand_tackle,
                           'Aggression' = aggression, 'Reactions' = reactions,
                           'Att Position' = att_position, 'Interceptions' = interceptions,
                           'Vision' = vision, 'Composure' = composure, 'Crossing' = crossing,
                           'Short Pass' = short_pass, 'Long Pass' = long_pass,
                           'Acceleration' = acceleration, 'Stamina' = stamina, 'Strength' = strength,
                           'Balance' = balance, 'Sprint Speed' = sprint_speed, 'Agility' = agility,
                           'Jumping' = jumping, 'Heading' = heading, 'Shot Power' = shot_power,
                           'Finishing' = finishing, 'Long Shots' = long_shots, 'Curve' = curve,
                           'FK Acc' = fk_acc, 'Penalties' = penalties, 'Volleys' = volleys,
                           'GK Positioning' = gk_positioning, 'GK Diving' = gk_diving,
                           'GK Handling' = gk_handling, 'GK Kicking' = gk_kicking,
                           'GK Reflexes' = gk_reflexes)

#A single dataframe is created combining the overview data of the players and their individual ratings
overall_data <- cbind(player_data, player_stats)
View(overall_data)

#Exporting the data to a csv file
write.csv(overall_data, 'C:/Courses/Semester 5/OPMA 631/Group Project 1/data_fifa.csv')