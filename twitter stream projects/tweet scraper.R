library(streamR)
# In addition, we need the "ROAuth"-package to establish an 
# authentification.
library(ROAuth)
# The following four lines assign the right values to the variables that
# are needed for the API call.
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
# The string within the quotation marks has to be replaced with the actual
# consumerKey and consumerSecret.
consumerKey <- "44i8RWOgHmw3fyK5s56yzkwTU"
consumerSecret <- "PxJoGDyz8uixMGRW1iwvhIhSj30j1jcUXjEF8rc5CC5xHsC6Tc"
# The next two lines establish a connection to the Twitter API.
# The system will print a URL which should be copied in a browser to receive a PIN number.
# This PIN has to be entered in the R-console.
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, 
                             consumerSecret = consumerSecret, 
                             requestURL = requestURL, 
                             accessURL = accessURL, 
                             authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# Once the connection is established we can save it so that we do not have
# repeat this process.
save(my_oauth, file = "my_oauth.Rdata")


load("my_oauth.Rdata")


file = "tweets.json"
track = NULL
follow = NULL
loc = c(-125, 30, -114, 42)
lang = NULL
minutes = 0.5
time = 60*minutes
tweets = NULL
filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             timeout = time, 
             tweets = tweets, 
             oauth = my_oauth,
             verbose = TRUE)


tweets.df <- parseTweets(file)
# Now we can inspect the table and save it.
View(tweets.df)
save(file="tweetsDF.RDATA", tweets.df)
