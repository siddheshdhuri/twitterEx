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
    reach <- c(reach, (favCount + retweetCount))

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
getTweetsDataFrame <- function(tweets) {

  tweettext=sapply(tweets, function(x) x$getText())

  stri_enc_mark(tweettext)
  tweettext <- stri_encode(tweettext, "", "UTF-8")

  # replace & ampersand with & symbols
  tweettext = gsub("&amp;", "", tweettext)

  tweetid=sapply(tweets, function(x) x$getId())

  #   tweetlat=unlist(sapply(tweets, function(x) as.numeric(as.character(x$getLatitude()))))
  #
  #   tweetlon=unlist(sapply(tweets, function(x) as.numeric(as.character(x$getLongitude()))))

  tweetcreated =unlist(lapply(tweets, function(x) as.character(x$getCreated())))

  tweetuser=unlist(sapply(tweets, function(x) x$getScreenName()))

  favCount <- unlist(sapply(tweets, function(x) x$getFavoriteCount()))

  retweetCount <- unlist(sapply(tweets, function(x) x$getRetweetCount()))

  source <- unlist(sapply(tweets, function(x) x$getStatusSource()))
  # remove surrounding html tags
  source <- gsub(pattern = "<.+\">|</a>",replacement = "",source)

  # assigning higher weight to retweet as it will proliferate trend
  reach <- favCount + retweetCount

  tweets.df = data.frame(cbind(tweetid= tweetid, tweet=tweettext,tweetcreated=tweetcreated, source = source,
                               favCount = favCount, retweetCount = retweetCount, reach=reach,
                               user=tweetuser),
                         stringsAsFactors = FALSE)

  # type cast columns as numeric
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

  user <- unlist(sapply(userobjs, function(x) x$screenName))
  name <- unlist(sapply(userobjs, function(x) x$name))
  # Encode text to UTF-8 encoding
  stri_enc_mark(name)
  name <- stri_encode(name, "", "UTF-8")

  followersCount <- unlist(sapply(userobjs, function(x) x$followersCount))
  tweetsCount <- unlist(sapply(userobjs, function(x) x$statusesCount))
  location <- unlist(sapply(userobjs, function(x) x$location))
  # Encode text to UTF-8 encoding
  stri_enc_mark(location)
  location <- stri_encode(location, "", "UTF-8")

  #locations <- unlist(sapply(userobjs, function(x) x$location))

  # remove non-alphabetic characters
  location <- gsub(pattern = "[^[:alpha:]|^[:space:]]", replacement = "", x = location, ignore.case = TRUE)

  user.df <- data.frame(cbind(user= user, name=name,followersCount=followersCount, tweetsCount = tweetsCount,
                              location = location),
                        stringsAsFactors = FALSE)

  #user.df <- user.df %>% ggmap::mutate_geocode(location)

  geocodes <- tryCatch({
    ggmap::geocode(user.df$location)

  }, error = function(e) {
    print(e)
  })

  user.df$lon <- geocodes$lon
  user.df$lat <- geocodes$lat

  return(user.df)

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

#' Function to get user interaction links from tweets
#'
#' @param tweets.df data.frame of tweet messages and users
#' @param exlclude_hashtags default FALSE, whether to include hashtags as nodes in network
#' @return link.df data.frame of links between users
#'
#' @export
getHastagUserLinks <- function (tweets.df, exlclude_hashtags=FALSE) {
  options(stringsAsFactors = FALSE)
  source.col <- NULL
  target.col <- NULL

  if(exlclude_hashtags){

    for (i in 1:nrow(tweets.df)) {

      tweet_user <- paste0('@',tweets.df$user[i])

      tagged_users <- unlist(str_extract_all(tweets.df$tweet[i],
                                             "@[:alnum:]+"))

      if(length(tagged_users) > 0){
        # for each tagged user link tweet user directly with mentioned user
        for(tagged_user in tagged_users){
          source.col <- c(source.col, tweet_user)
          target.col <- c(target.col, tagged_user)
        }
      }

    }

  }
  else {

    for (i in 1:nrow(tweets.df)) {

      tweet_user <- paste0('@',tweets.df$user[i])

      tagged_users <- unlist(str_extract_all(tweets.df$tweet[i],
                                             "@[:alnum:]+"))

      hashtags <- unlist(str_extract_all(tweets.df$tweet[i],
                                         "#[:alnum:]+"))
      if (length(hashtags) > 0 ){
        # for each hashtag link tweet user to hashtag
        for(hashtag in hashtags) {
          source.col <- c(source.col, tweet_user)
          target.col <- c(target.col, hashtag)
          # for each tagged user link hash tag with mentioned user
          for(tagged_user in tagged_users){
            source.col <- c(source.col, hashtag)
            target.col <- c(target.col, tagged_user)
          }
        }
      }else if(length(tagged_users) > 0){
        # for each tagged user link tweet user directly with mentioned user
        for(tagged_user in tagged_users){
          source.col <- c(source.col, tweet_user)
          target.col <- c(target.col, tagged_user)
        }
      }

    }

  }

  links.df <- data.frame(source = source.col, target = target.col)
}


#' Function to get nodes data.frame for network from tweets and links dataframes
#'
#' @param tweets.df data.frame of tweet messages and users
#' @param links.df data.frame of links between users
#'
#' @return network.nodes.df dataframe of node in the network
#'
#' @export
getNetworkNodesDF <- function(tweets.df, links.df){
  # get nodes df from links.df
  nodes <- as.data.frame(table(c(c(paste0("@",tweets.df$user), links.df$target))))
  num.nodes <- nrow(nodes)

  # nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
  # nodes$color.border <- "black"
  # nodes$color.highlight.background <- "orange"
  # nodes$color.highlight.border <- "darkred"
  options(stringsAsFactors = FALSE)
  network.nodes.df <- data.frame(name=nodes$Var1,
                                 size=nodes$Freq,
                                 shape=ifelse(sapply(nodes$Var1, function(i) grepl('^@', i)), 'dot', 'text'),
                                 shadow = TRUE,
                                 #title=nodes$Var1, #' causes page to refresh
                                 label = nodes$Var1,
                                 borderWidth = 1,
                                 group=ifelse(sapply(nodes$Var1, function(i) grepl('^@', i)), 'user', 'hashtag'),
                                 id=0:(num.nodes-1)
  )
  network.nodes.df <- network.nodes.df %>% dplyr::arrange(desc(size))
  return(network.nodes.df)
}

#' Function to get network links with node ids from nodes dataframe and links dataframe
#'
#' @param links.df data.frame of links between users
#' @param network.nodes.df data.frame of nodes with ids
#'
#' @return network.links.df dataframe of links to plot network
#'
#' @export
getNetworkLinksDF <- function(links.df, network.nodes.df){

  # get links by node id from
  links <- merge(links.df, network.nodes.df, by.x = "source", by.y = "name")

  links <- merge(links, network.nodes.df, by.x="target", by.y="name")

  # change column names
  colnames(links) <- c("target","source","target.size","target.group","target.id","source.size","source.group","source.id")

  # links.df for creating D3Network chart
  # network.links.df <- data.frame(source=links$source.id,
  #                                target=links$target.id,
  #                                value=1)

  # links.df for creating visNetwork chart
  network.links.df <- data.frame(from=links$source.id,
                                 to=links$target.id,
                                 value=1)

  return(network.links.df)
}


#' Function to get interaction network from tweets datafrae
#'
#' @param tweets.df data.frame of tweet messages and users
#' @param exlclude_hashtags default FALSE, whether to include hashtags as nodes in network
#'
#' @return networkd.df dataframe of interaction network
#'
#' @export
getTweetsInteractionNetwork <- function(tweets.df, exlclude_hashtags=FALSE){
  # get links from tweets
  links.df <- getHastagUserLinks(tweets.df)
  # get nodes df from links
  network.nodes.df <- getNetworkNodesDF(tweets.df,links.df)
  # create network where nodes are ids (this is required for VisNetwork)
  network.links.df <- getNetworkLinksDF(links.df,network.nodes.df[,c("name","size","group","id")])
  network.links.df$value <- NULL

  require(visNetwork)
  network.df <- visNetwork(network.nodes.df, network.links.df) %>%
                visGroups(groupname = "user", shape = "icon",
                          icon = list(code = "f0c0", size = 75)) %>%
                addFontAwesome() %>%
                visIgraphLayout() %>%
                visInteraction(navigationButtons = TRUE, hover = TRUE) %>%
                visOptions(highlightNearest = list(enabled =TRUE, degree = 2)) %>%
                visEvents(select = "function(nodes) {
                            Shiny.onInputChange('current_node_id', nodes.nodes);
                            ;}")

  return(network.df)

}

