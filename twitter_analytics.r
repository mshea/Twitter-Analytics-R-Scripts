## This R script will take in the file tweet_activity_metrics.csv
## available from analytics.twitter.com and generate small multiple
## charts based on day of the week and hour of the day from both 
## engagements and impressions
##
## by Mike Shea on 29 August 2014

# Load twitter data

d <- read.csv("~/Downloads/tweet_activity_metrics.csv")

### Prep our data

# Create a date object in GMT
d$dateobj <- as.POSIXct(strptime(d$time, "%Y-%m-%d %H:%M +0000", tz="GMT"), "GMT")

# Create an hour column in EST
d$hour <- as.POSIXlt(d$dateobj, tz="America/New_York")$hour

# Build hour labels for our chart
hourlabels <- c("12a","4a","8a","12p","4p","8p")

# Create a column for the day of the week (0 = Sunday)
d$dayofweek <- as.POSIXlt.date(strptime(d$time, "%Y-%m-%d %H:%M +0000"))$wday

# Build a list of day labels for the chart
daysofweek <- c("Sun","Mon","Tue","Wed","Thr","Fri","Sat")

## Function to take in a data frame of scores and dates and come up
## with a max score for the whole week so we know what our max number should be
maxDailyScores <- function(d) {
	# d is a data frame with a numeric score in column 1 and a date in column 2
	# the date format is "YYYY-MM-DD HH:MM +0000".
	maxscores <- c()
	for (i in 0:6) {
		dw <- subset(d, d$dayofweek == i)
		scorebyhour <- aggregate(scores ~ hour, dw, sum)
		maxscores <- c(maxscores, scorebyhour$score)
	}
	return(max(maxscores))
}

generateChart <- function(d, title) {
	# Generate seven rows of hourly data, one row per week.
	# d has the columns "score", "dayofweek", and "hour".
	# title is a string of text to display at the top
	maxscores <- maxDailyScores(d)
	par(mfrow=c(7,1), mar=c(0,3,0,0), lwd=1, oma=c(3,.1,3,.2))
	for (i in 0:6) {
		dw <- subset(d, d$dayofweek == i)
		scorebyhour <- aggregate(scores ~ hour, dw, sum)

		topthreescores <- scorebyhour[order(-scorebyhour$scores),][c(1:1),]

		## Go through each data set. If there are empty hours, fill them with 0
		for (j in 0:23) {
			if(!any(scorebyhour$hour==j)) {
				scorebyhour <- rbind (scorebyhour, c(j,0))
			}
		}
		scorebyhour <- scorebyhour[with(scorebyhour, order(hour)),]
		plot(c(0:23), frame=F, font.main = 1, main="", type="n", ylim=c(0,maxDailyScores(d)*1.5), yaxt='n', xaxt="n", xlab="", ylab="", col='#333333')
		#lines(scorebyhour$hour, rep(0,times=length(scorebyhour$hour)), col="#333333")
		lines(scorebyhour, col="#333333")
		axis(2, at=1, labels=daysofweek[i+1], tick=F, padj=0)
		text(topthreescores, labels=topthreescores$score, cex=.7, pos=3)
	}
	axis(side=1, at=seq(from=0, to=22, by=4), lwd="0", lwd.ticks="0", col="#cccccc", labels=hourlabels)
	mtext(title, outer = TRUE, cex = 1)
}

d$scores <- d$engagements
svg(filename="~/Desktop/twitter_engagement_by_day_and_hour.svg", pointsize=16, height=6, width=6)
generateChart(d, "Engagements by Hour and Weekday")
dev.off()
png(filename="~/Desktop/twitter_engagement_by_day_and_hour.png", height=1600, width=1600, pointsize=50)
generateChart(d, "Engagements by Hour and Weekday")
dev.off()

d$scores <- d$impressions
svg(filename="~/Desktop/twitter_impressions_by_day_and_hour.svg", pointsize=16, height=6, width=6)
generateChart(d, "Impressions by Hour and Weekday")
dev.off()
png(filename="~/Desktop/twitter_impressions_by_day_and_hour.png", height=1600, width=1600, pointsize=50)
generateChart(d, "Impressions by Hour and Weekday")
dev.off()

