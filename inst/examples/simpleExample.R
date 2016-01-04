if (!require("twitterEx")) install.packages("twitterEx_0.1.tar.gz", repos=NULL, type="source") ; require("twitterEx")

twitterEx:::setupAuthentication(file.path("authentication.csv"))

usernames <- readLines("user_handles.txt")

num.followers <-c(); last.time <- c(); last.tweet <- c()

for(username in usernames){

  user <- twitterEx:::getUserInfo(username)

  num.followers <- c(num.followers,
                     twitterEx:::getUserFollowerCount(user))

  last.time <- c(last.time,
                 twitterEx:::whenLastActive(user))

  last.tweet <- c(last.tweet,
                  twitterEx:::getLastTweet(user))

}

write.csv(as.data.frame(cbind(usernames,
                              num.followers,
                              last.tweet,
                              last.time)),
          "output.csv", row.names = FALSE)



