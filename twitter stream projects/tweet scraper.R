library(streamR)
library(ggplot2)
library(grid)
library(maps)
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
track = "flu vaccination"
follow = NULL
loc = c(-5, 49, -0.49, 60)
lang = NULL
minutes = 0.1
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



# Now we can inspect the table and save it.



tweetPlot<-function(toMatch){
  buff<-0
  while(buff<10000){
    buff=buff+1
    print(buff)
  
  load("my_oauth.Rdata")
  
  filterStream(file.name="tweets_rstats.json",language="en",
               locations = c(-175, -59, 175, 83), timeout = 30, oauth=my_oauth)
  
  tweets.df <- parseTweets("tweets_rstats.json")
  
  
  tweets.df2<-tweets.df[grep(paste(toMatch,collapse="|"),tweets.df$text),]
  tweets.df2x<-rbind(tweets.df2,tweets.df2x)
  
print(nrow(tweets.df2x))
  
  write.table(NULL,"C:\\Users\\Aaron\\Documents\\tweets_rstats.json")
  write.table(tweets.df2x,"C:\\Users\\Aaron\\Documents\\tweetResults.json")
  
  map.data <- map_data('world')
  
  # We only need the long and lat values from the data. 
  # These are put in a new object.
  points <- data.frame(x = as.numeric(tweets.df2x$place_lon), 
                       y = as.numeric(tweets.df2x$place_lat))
  # This line is needed for the second plot, when hashtags are added.
  points$hashtags <- tweets.df$hashtags
  # The next lines are just used to remove points that are not specified or 
  # are incidental too far a way from California.
  points[!is.na(tweets.df2x$lon), "x"] <- as.numeric(tweets.df2x$lon)[!is.na(tweets.df2x$lon)]
  points[!is.na(tweets.df2x$lat), "y"] <- as.numeric(tweets.df2x$lat)[!is.na(tweets.df2x$lat)]
  #points <- points[(points$y > 25 & points$y < 42), ]
  #points <- points[points$x < -114,]
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
               alpha=0.2,
               color = "red")
  
  
  print(mapPlot)
  
  png(filename="C:\\Users\\Aaron\\Documents\\mapres.png",width = 1920, height = 1080)
  plot(mapPlot)
  dev.off()
  
  
tweets.df3x<-subset(tweets.df2x,country=="United Kingdom")
  
  map.data <- map_data('world','uk')
  
  # We only need the long and lat values from the data. 
  # These are put in a new object.
  points <- data.frame(x = as.numeric(tweets.df3x$place_lon), 
                       y = as.numeric(tweets.df3x$place_lat))
  # This line is needed for the second plot, when hashtags are added.
  points$hashtags <- tweets.df$hashtags
  # The next lines are just used to remove points that are not specified or 
  # are incidental too far a way from California.
  points[!is.na(tweets.df3x$lon), "x"] <- as.numeric(tweets.df3x$lon)[!is.na(tweets.df3x$lon)]
  points[!is.na(tweets.df3x$lat), "y"] <- as.numeric(tweets.df3x$lat)[!is.na(tweets.df3x$lat)]
  #points <- points[(points$y > 25 & points$y < 42), ]
  #points <- points[points$x < -114,]
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
               size = 3, 
               alpha=0.5,
               color = "red")
  
  
  print(mapPlot)
  
  png(filename="C:\\Users\\Aaron\\Documents\\mapUK2.png",width = 1920, height = 1080)
  plot(mapPlot)
  dev.off()
  
  }
}


matchWords<-c("\\bfeeling sick\\b","\\bfeeling ill\\b","\\bi'm sick\\b","\\bi'm ill\\b","\\bthe flu\\b",
                     "\\bhave a cold\\b","\\bflu\\b","\\bfeel sick\\b","\\bfeel ill\\b","\\bfeel unwell\\b","\\bhave a cold\\b",
                     "\\bhave a virus\\b",ignore.case=T)

tweets.df2x<-NULL
  tweetPlot(matchWords)

  
  
  
  
  
  
  
  ####timeline plots
  library(ggplot2)
  
  bd<-read.csv("C:\\dataVis\\google data\\multiTimelineBD.csv",sep=",")
  colnames(bd)<-c("date","count")

  bd$timeLine<-c(1:nrow(bd))
  
  ggplot() +
    geom_line(aes(x = as.numeric(timeLine),y = count,xend = timeLine),data=bd, col = "blue",lwd=1,alpha=0.5) +
    geom_point(aes(x = as.numeric(timeLine),y = count),data=bd,alpha=0.4) +
    ylab("Interest Over Time")+
    xlab("Year")+
    scale_x_continuous(breaks = c(9,62,114,168,218),labels = c("2012","2013","2014","2015","2016")) +
    theme_bw()


  bd<-read.csv("C:\\dataVis\\google data\\multiTimelineSpark.csv",sep=",")
  colnames(bd)<-c("date","count")
  
  bd$timeLine<-c(1:nrow(bd))
  
  ggplot() +
    geom_line(aes(x = as.numeric(timeLine),y = count,xend = timeLine),data=bd, col = "blue",lwd=1,alpha=0.5) +
    geom_point(aes(x = as.numeric(timeLine),y = count),data=bd,alpha=0.4) +
    ylab("Interest Over Time")+
    xlab("Year")+
    scale_x_continuous(breaks = c(9,62,114,168,218),labels = c("2012","2013","2014","2015","2016")) +
    theme_bw()
  
  
  
  
  ###########ZIKA -timeline plots
  
  zika<-read.csv("C:\\dataVis\\google data\\lyme.csv",sep=",",head=F)
  colnames(zika)<-c("date","themex","intensity")
  aggDat<-aggregate(zika$intensity~zika$theme,FUN=sum)
  colnames(aggDat)<-c("themex","intensity")
  #top<-tail(aggDat[order(aggDat$intensity),],2) 
  top<-aggDat[sample(aggDat$themex,50,replace=T),] 
  
  results<-merge(top,zika,by="themex")
  results<-results[order(results$date),]
  results$timeLine<-c(1:nrow(results))
  
  results$date2<-as.Date(as.character(results$date), "%Y%m%d")
  startDate<-as.Date("20130101","%Y%m%d")
  results$date2<-results$date2-startDate
  
  
  ggplot() +
    geom_point(aes(x = (themex),y = as.numeric(date2)),data=results, col = "blue",alpha=0.1,size=sqrt(1+results$intensity.y)) +
   theme(axis.text.x=element_text(angle = -90, hjust = 0))+
    scale_y_continuous(breaks = c(1,373,731,1096),labels = c("2013","2014","2015","2016"))
    
  
  
  ###plot world and tweet data
  
  dat<-read.csv("C:\\Users\\Aaron\\Documents")