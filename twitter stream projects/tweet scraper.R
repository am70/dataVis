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
minutes = 5
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



library(stringr)
tweets.df$hashtags <- str_extract(tweets.df$text, "#[:alnum:]+")

tweets.df$hashtags <- as.factor(tweets.df$hashtags)
summary(tweets.df$hashtags)

tweets.df$full_name <- as.factor(tweets.df$full_name)
summary(tweets.df$full_name)


  library(plyr)
sel <- count(tweets.df$full_name)$x[count(tweets.df$full_name)$freq>100]
sel <- tweets.df$full_name %in% sel
test.df <- tweets.df[sel,]

test.df$full_name <- droplevels(test.df$full_name)

fit <- lm(followers_count ~ full_name, data=test.df)
summary(fit)

  
fit2 <- lm(followers_count ~ full_name + friends_count, data=test.df)
summary(fit2)





# Two additional packages are needed:
library(ggplot2)
library(grid)
library(maps)
# Create an object containing the boundaries of California as 
# longuitude and lattitude.
map.data <- map_data("state", region=c("california"))
# We only need the long and lat values from the data. 
# These are put in a new object.
points <- data.frame(x = as.numeric(tweets.df$place_lon), 
                     y = as.numeric(tweets.df$place_lat))
# This line is needed for the second plot, when hashtags are added.
points$hashtags <- tweets.df$hashtags
# The next lines are just used to remove points that are not specified or 
# are incidental too far a way from California.
points[!is.na(tweets.df$lon), "x"] <- as.numeric(tweets.df$lon)[!is.na(tweets.df$lon)]
points[!is.na(tweets.df$lat), "y"] <- as.numeric(tweets.df$lat)[!is.na(tweets.df$lat)]
points <- points[(points$y > 25 & points$y < 42), ]
points <- points[points$x < -114,]
# The following code creates the graphic.
mapPlot <- ggplot(map.data) + # ggplot is the basic plotting function used.
  # The following lines define the map-areas.
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white", 
           color = "grey20", 
           size = 0.25) +  
  expand_limits(x = map.data$long, 
                y = map.data$lat) + 
  # The following parameters could be altered to insert axes, title, etc.
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  # The next line plots points for each tweet. Size, transparency (alpha) 
  # and color could be altered.
  geom_point(data = points, 
             aes(x = x, y = y), 
             size = 2, 
             alpha = 1/20, 
             color = "steelblue")

mapPlot # This command plots the object.
