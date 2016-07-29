fielding <- read.csv("Fielding_06-10.csv", stringsAsFactors = F)

sub <- subset(fielding, Pos != "C")
nrow(sub)

# Create a data frame with no missin values
attach(fielding)
sum(!is.na(UZR))

fitlm <- lm(DRS ~ UZR + TZL, data = sub)
summary(fitlm)

mydb <- dbConnect(MySQL(), user='root', password='637621td',
                  dbname='khanAcademy')

dat <- dbGetQuery(mydb, "select * from countries_by_population")
test <- as.numeric(dat[4])

rs <- dbSendQuery(mydb, 'select * from countries_by_population')
data <- dbFetch(rs)


dbDisconnect(mydb)
college <- dbConnect(MySQL(), user='root', password='637621td',
                     dbname='College_Amissions')
dat <- dbGetQuery(college, "select * from Serves")
