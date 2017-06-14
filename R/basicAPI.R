##########################################################################################
# Basic api for twitter data exploration
##########################################################################################




#' Function to get user Info for a given user twitter handle
#'
#' @param username the twitter handle of user whose followers need to be extracted.
#' @return userInfo object with information about user
#'
#' @export
getUserInfo <- function(username){

  if(Sys.Date() > "2017-12-31") stop("This package has expired please contact siddhesh dhuri (siddhesh.s.dhuri@gmail.com) for update")

  username <- stringr::str_trim(username)
  status <- FALSE
  while (status==FALSE) {
    rate <- getCurRateLimitInfo(c("users"))
    status <- as.numeric(rate[rate$resource == "/users/show/:id","remaining"]) > 50
    if (status) {
      cat('Extracting...\n')
      userInfo <- getUser(username)
    } else {
      cat("Waiting...\n")
      Sys.sleep(600)
    }
  }

  return(userInfo)
}




#' Function to get followers for a given user twitter handle
#'
#' @param userInfo user object of the user for whom followers need to be extracted
#' @return vector of first degree followers for the user
#'
#' @export
getUserFollowers <- function(userInfo, n=100){

  if(class(userInfo)=="character") {
    userInfo <- getUserInfo(userInfo)
  }

  status <- FALSE
  while (status==FALSE) {
    rate <- getCurRateLimitInfo(c("users"))
    status <- as.numeric(rate[rate$resource == "/users/lookup","remaining"]) > 50
    if (status) {
      cat('Extracting...\n')
      firstdegree <- userInfo$getFollowers(n)
    } else {
      cat("Waiting...\n")
      Sys.sleep(600)
    }
  }

  #get screen names of all followers
  followers <- as.character(length(firstdegree))
  for (i in 1:length(firstdegree)) followers[i] <- firstdegree[[i]]$screenName

  return(followers)

}


#' Function to get tweets for a given user twitter handle or userInfo object
#'
#' @param username twitter handle or user object of user whose followers need to be extracted.
#' @return list of last n tweets by user
#'
#' @export
getUserTweets <- function(username, n = 5){

  tweets <- userTimeline(username,n)

  tweets <- lapply(tweets,function(x) x$text)

  tweets <- gsub("\\n"," ",tweets)

  return(tweets)

}

#' Function to get when was user last active
#'
#' @param userInfo user object of the user
#' @return time when user was last active
#'
#' @export
whenLastActive <- function(userInfo){

  if(class(userInfo)=="character") {
    userInfo <- getUserInfo(userInfo)
  }

  return(as.character(userInfo$getLastStatus()$created))

}


#' Function to get last tweet of user
#'
#' @param userInfo user object of the user for whose last tweet needs to be fetched
#' @return time when user was last active
#'
#' @export
getLastTweet <- function(userInfo){

  if(class(userInfo)=="character") {
    userInfo <- getUserInfo(userInfo)
  }

  return(userInfo$getLastStatus()$text)

}


#' Function to setup authentication
#'
#' @param path of file with authentication details
#'
#' @export
setupAuthentication <- function(filepath){

  if (!require("httr")) install.packages("httr") ; require("httr")
  if (!require("twitteR")) install.packages("twitteR") ; require("twitteR")

  auth.df <- read.csv(filepath, header = TRUE)

  consumer_key <- auth.df$consumer_key
  consumer_secret <- auth.df$consumer_secret
  access_token <- auth.df$access_token
  access_secret <- auth.df$access_secret

  setup_twitter_oauth(consumer_key= stringr::str_trim(consumer_key),
                      consumer_secret= stringr::str_trim(consumer_secret),
                      access_token= stringr::str_trim(access_token),
                      access_secret= stringr::str_trim(access_secret))


}


#' Function to get total number of followers for a user
#'
#' @param userInfo user object of the user for whom  count need to be extracted
#' @return count of followers of this user
#'
#' @export
getUserFollowerCount <- function(userInfo){

  if(class(userInfo)=="character") {
    userInfo <- getUserInfo(userInfo)
  }

  return(userInfo$followersCount)
}



#' Function to gather data for user handles and save extracted data into a csv file
#'
#' @param userFile file path of the text file containing user handles
#'
#' @export
getUserData <- function(userFile){

  usernames <- readLines(userFile)

  num.followers <-c(); follower.handles <- c(); last.time <- c(); last.tweet <- c(); last.ten.tweets <- c()

  for(username in usernames){
    # if userhandle is the entire url rather than just user handle then we split
    # and take the last item as the username
    usernamesplit <- unlist(strsplit(username,"/"))

    username <- usernamesplit[length(usernamesplit)]

    user <- twitterEx:::getUserInfo(username)

    num.followers <- c(num.followers,
                       twitterEx:::getUserFollowerCount(user))

    this.follower.handles <- paste(twitterEx::getUserFollowers(user),collapse = "/")
    follower.handles <- c(follower.handles, this.follower.handles)

    last.time <- c(last.time,
                   twitterEx:::whenLastActive(user))

    last.tweet <- c(last.tweet,
                    twitterEx:::getLastTweet(user))

    this.last.ten.tweets <- paste(twitterEx::getUserTweets(user,10),collapse="[DELIMITER]")
    last.ten.tweets <- c(last.ten.tweets,this.last.ten.tweets)


  }

  file.name <- paste("Userdata_",gsub(":","_",Sys.time()),".csv",sep='')

  write.csv(as.data.frame(cbind(usernames,
                                num.followers,
                                follower.handles,
                                last.tweet,
                                last.time,
                                last.ten.tweets
                                )),
            file.name, row.names = FALSE)

  print(paste("User Data save in file : ",file.name))


}



#' Function to get tweets as clean data.frame for a list of users
#'
#' @param usernames vector of one or more usernames
#' @return tweets.df a data.frame with tweets details
#'
#' @export
getUserTweetsDataFrame <- function(usernames){
  print(usernames)
  tweettext <- c()
  user <- c()
  tweetcreated <- c()
  tweetdate <- c()
  favCount <- c()
  retweetCount <- c()
  reach <- c()

  for(username in usernames){

    # Fetch user tweets information
    tweets <- twitteR::userTimeline(username, 100)

    tweettext=c(tweettext,
                sapply(tweets, function(x) x$getText()))
    # Encode text to UTF-8 encoding
    stri_enc_mark(tweettext)
    tweettext <- stri_encode(tweettext, "", "UTF-8")

    user <- c(user, rep(username,length(tweettext)))

    tweetcreated <- c(tweetcreated,
                      unlist(lapply(tweets, function(x) as.character(x$getCreated()))))

    tweetdate <- c(tweetdate,
                   as.Date(tweetcreated))

    favCount <- c(favCount,
                  as.numeric(unlist(sapply(tweets, function(x) x$getFavoriteCount()))))

    retweetCount <- c(retweetCount,
                      as.numeric(unlist(sapply(tweets, function(x) x$getRetweetCount()))))

    # assigning higher weight to retweet as it will proliferate trend
    reach <- c(reach, (favCount + 2*retweetCount))

  }

  options(stringsAsFactors = FALSE)

  tweets.df <- as.data.frame(cbind(tweet=tweettext,
                                   user = user,
                                   tweetcreated=tweetcreated,
                                   tweetdate = tweetdate,
                                   favCount = favCount,
                                   retweetCount = retweetCount,
                                   reach = reach))

  tweets.df$tweetdate <- as.Date(as.numeric(tweets.df$tweetdate), origin="1970-01-01")
  tweets.df$favCount <- as.numeric(tweets.df$favCount)
  tweets.df$retweetCount <- as.numeric(tweets.df$retweetCount)
  tweets.df$reach <- as.numeric(tweets.df$reach)

  return(tweets.df)

}

#' Function to get tweets as clean data.frame from a tweets object
#'
#' @param tweets collection object returned by twitteR::search
#' @return tweets.df a data.frame with tweets details
#'
#' @export
getTweetsDataFrame <- function(tweets){

  tweettext=sapply(tweets, function(x) x$getText())

  stri_enc_mark(tweettext)
  tweettext <- stri_encode(tweettext, "", "UTF-8")

  tweetid=sapply(tweets, function(x) x$getId())

#   tweetlat=unlist(sapply(tweets, function(x) as.numeric(as.character(x$getLatitude()))))
#
#   tweetlon=unlist(sapply(tweets, function(x) as.numeric(as.character(x$getLongitude()))))

  tweetcreated =unlist(lapply(tweets, function(x) as.character(x$getCreated())))

  tweetuser=unlist(sapply(tweets, function(x) x$getScreenName()))

  tweetlat = NA
  tweetlon = NA

  result = tryCatch({

    gc <- getUserLocation(tweetuser)

    tweetlat <- as.numeric(gc$lat)
    tweetlon <- as.numeric(gc$lon)

  }, error = function(e) {
      print(e)
  })



  favCount <- unlist(sapply(tweets, function(x) x$getFavoriteCount()))

  retweetCount <- unlist(sapply(tweets, function(x) x$getRetweetCount()))

  # assigning higher weight to retweet as it will proliferate trend
  reach <- favCount + 2*retweetCount

  tweets.df = data.frame(cbind(tweetid= tweetid, tweet=tweettext,tweetcreated=tweetcreated,
                                  lat=tweetlat,lon=tweetlon, user=tweetuser,
                                  favCount = favCount, retweetCount = retweetCount, reach=reach),
                         stringsAsFactors = FALSE)

  #' type cast columns as numeric
  tweets.df$favCount <- as.numeric(tweets.df$favCount)
  tweets.df$retweetCount <- as.numeric(tweets.df$retweetCount)
  tweets.df$reach <- as.numeric(tweets.df$reach)

  return(tweets.df)

}

#' Function to get tweets as clean data.frame from a tweets object
#'
#' @param user username of the twitter user
#' @return geocode location
#'
#' @export
getUserLocation <- function(users){

  userobjs <- unlist(sapply(users, function(x) getUser(x)))

  locations <- unlist(sapply(userobjs, function(x) x$location))

  geocodes <- geocode(locations)

}


#' Function to get user interaction links from tweets
#'
#' @param tweets.df data.frame of tweet messages and users
#' @return link.df data.frame of links between users
#'
#' @export
getUserLinksFromTweets <- function(tweets.df){

  options(stringsAsFactors = FALSE)

  source.col <- NULL
  target.col <- NULL
  for(i in 1:nrow(tweets.df)){

    source <- tweets.df$user[i]

    targets <- unlist(str_extract_all(tweets.df$tweet[i], "@[:alnum:]+"))

    for(target in targets){
      source.col <- c(source.col,source)
      target.col <- c(target.col,target)
    }

  }

  source.col <- paste0("@",source.col)

  links.df <- data.frame(source=source.col,
                         target=target.col)

}
