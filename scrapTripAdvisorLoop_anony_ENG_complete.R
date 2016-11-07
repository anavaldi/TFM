###########################################################################
#
# TRIP ADVISOR OPINIONS
#
# Description: Scrap TripAdvisor opinions and user information
# Inspired by:
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
# Author: Ana Valdivia
# Date: June 2016
###########################################################################

# Libraries
library(rvest)
library(beepr)

# change language
Sys.setlocale("LC_TIME", "English")

# Scrapping
TripAdvisorAlhambra <- data.frame()
totalpages <- 842 # upload with total pages to scrap on TripAdvisor webpage
last <- 1

for(k in last:totalpages){
  print(k)
  if(k == 1) {
    url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187441-d191078-Reviews-The_Alhambra-Granada_Province_of_Granada_Andalucia.html#REVIEWS")
  } else {
    url <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g187441-d191078-Reviews-or",k-1,"0-The_Alhambra-Granada_Province_of_Granada_Andalucia.html")
    
    
  }
  # User's information:
  
  # Name
  users <- url %>%
    read_html() %>%
    html_nodes(".member_info")
               # .memberOverlayLink")

  rev <- c()
  for(i in 1:length(users)){
    user.ch <- as.character(users[i])
    if(substr(user.ch, 27, 33) == "<div id"){
      rev <- append(rev, i, after=length(rev))
    }
    else if(substr(user.ch, 27, 44) == '<div class="avatar' & k > 1){
      rev <- append(rev, i, after=length(rev))
    }
  }
    
  users <- users[rev] # Only user's Reviews
  
  username <- users %>%
    html_node(".username") %>%
    html_text()
  username <- gsub("\n", "", username)
  
  # Location
  users <- url %>%
    read_html() %>%
    html_nodes(".member_info")

  rev <- c()
  for(i in 1:length(users)){
    user.ch <- as.character(users[i])
    if(substr(user.ch, 27, 33) == "<div id"){
      rev <- append(rev, i, after=length(rev))
    }
    else if(substr(user.ch, 27, 44) == '<div class="avatar'){
      rev <- append(rev, i, after=length(rev))
    }
  }
  users <- users[rev]
  
  location <- users %>%
    html_nodes(".location") %>%
    html_text()
  location <- gsub("\n", "", location)
  
  # Opinions
  users <- url %>%
    read_html() %>%
    html_nodes(".memberBadging .reviewerBadge")
  
  userop <- users %>%
    html_node(".badgeText") %>%
    html_text()
  
  # For anonymous users
  for(i in 1:length(username)){
    if(username[i] == "A TripAdvisor Member "){
      username[i] <- "A TripAdvisor Member"
      location <- append(location, NA, after=i-1)
      userop <- append(userop, NA, after=i-1)
    }
  }
  
  
  # About reviews 
    reviews <- url %>%
      read_html() %>%
      html_nodes("#REVIEWS .innerBubble")
  
    # ID reviewrs
    id <- reviews %>%
      html_node(".quote a") %>%
      html_attr("id")
    
    # 
    quote <- reviews %>%
      html_node(".quote span") %>%
      html_text()
    
    rating <- reviews %>%
      html_node(".rating .rating_s_fill") %>%
      html_attr("alt") %>%
      gsub(" of 5 bubble", "", .) %>%
      as.integer()
    
    # Date of the opinion
    date <- reviews %>%
      html_node(".rating .ratingDate") %>%
      html_attr("title") %>%
      as.Date("%d %b %Y")
    
    # Another format
    for(i in 1:length(date)){
      if(is.na(date[i])){
        date.aux <- reviews %>%
          html_node(".ratingDate")
        date[i] <- as.Date(substr(regmatches(as.character(date.aux[i]), 
                                             regexpr('Reviewed .+\n', as.character(date.aux[i]))), 10, nchar(regmatches(as.character(date.aux[i]), regexpr('Reviewed .+\n', as.character(date.aux[i]))))-1), format="%d %b %Y")
        }
      }
          
    #   PARTIAL REVIEWS:
    #     review <- reviews %>%
    #       html_node(".entry .partial_entry") %>%
    #       html_text()
    #     
    #     reviewnospace <- gsub("\n", "", review)
    
    # COMPLETE REVIEWS:
    reviewnospace <- as.character(c(1:length(id)))
    for(i in 1:length(id)){
      completeReviewURL <- paste0("https://www.tripadvisor.co.uk/ShowUserReviews-g187441-d191078-r",gsub("rn", "", id[i]),"-The_Alhambra-Granada_Province_of_Granada_Andalucia.html#REVIEWS")
      reviews_aux <- completeReviewURL %>%
              read_html() %>%
              html_nodes(".entry")
              
      
      reviewnospace[i] <- gsub(paste0('.*"review_',gsub("rn", "", id[i]),'\">\n|\n.*'), "", as.character(reviews_aux[1]))
      
    
      
    }
      
  # Page in TripAdvisor
  page <- rep(k, length(username))
  
  temp.TripAdvisorAlhambra <- data.frame(id, username, location, userop, quote, 
                              rating, date, reviewnospace, page, stringsAsFactors = FALSE) 
  
  TripAdvisorAlhambra <- rbind(TripAdvisorAlhambra, temp.TripAdvisorAlhambra)
}
beep(2)

# Remove <br/> symbol
TripAdvisorAlhambra <- TripAdvisorAlhambra[!(duplicated(TripAdvisorAlhambra)),]
TripAdvisorAlhambra$reviewnospace <- gsub("<br/>", "", TripAdvisorAlhambra$reviewnospace)
# Merge title and opinion
TripAdvisorAlhambra$titleopinion <- paste(TripAdvisorAlhambra$quote, TripAdvisorAlhambra$reviewnospace, sep=". ")

write.csv(TripAdvisorAlhambra, file="./data/TripAdvisorAlhambra_20160731_ENGLISH_complete.csv")
save(TripAdvisorAlhambra, file="./data/TripAdvisorAlhambra.Rdata")
