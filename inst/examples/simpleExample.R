if (!require("twitteR")) install.packages("twitteR") ; require("twitteR")
if (!require("httr")) install.packages("httr") ; require("httr")

if (!require("twitterEx")) install.packages("twitterEx_0.1.tar.gz", repos=NULL, type="source") ; require("twitterEx")

twitterEx::setupAuthentication(file.path("authentication.csv"))

twitterEx::getUserData("user_handles.txt")


