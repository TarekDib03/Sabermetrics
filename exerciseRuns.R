# link to scrape the data from: http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2014

# Scrape the data set using data-miner in google chrome. Save it as a .csv and then read it in
dat <- read.csv("baseball.csv", stringsAsFactors = F)

# What team had the second highest run environment (basic park factor) as of 2014?
# First order by Basic in descending order
dat1 <- dat[order(dat$Basic, decreasing = T), ]
mean(dat1$Basic)
head(dat1$Team,2)[2]

# Which team's park inflated doubles by the most as of 2014?
# Order by 2B in descending order
dat2 <- dat[order(dat$X2B, decreasing = T),]
head(dat2$Team, 1)


