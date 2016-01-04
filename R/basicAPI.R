#'#########################################################################################
#' basic api for twitter data exploration
#'#########################################################################################




#' Function to get user Info for a given user twitter handle
#'
#' @param username the twitter handle of user whose followers need to be extracted.
#' @return userInfo object with information about user
#'
#' @export
getUserInfo <- function(username){

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
getUserFollowers <- function(userInfo){

  status <- FALSE
  while (status==FALSE) {
    rate <- getCurRateLimitInfo(c("users"))
    status <- as.numeric(rate[rate$resource == "/users/lookup","remaining"]) > 50
    if (status) {
      cat('Extracting...\n')
      firstdegree <- userInfo$getFollowers()
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

  return(tweets)

}

#' Function to get when was user last active
#'
#' @param userInfo user object of the user
#' @return time when user was last active
#'
#' @export
whenLastActive <- function(userInfo){

  return(as.character(userInfo$getLastStatus()$created))

}


#' Function to get last tweet of user
#'
#' @param userInfo user object of the user for whose last tweet needs to be fetched
#' @return time when user was last active
#'
#' @export
getLastTweet <- function(userInfo){

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

  return(userInfo$followersCount)
}
