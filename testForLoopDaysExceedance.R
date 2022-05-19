days <- read_csv("Test_DaysExceedanceData.csv")
lapply(1:nrow(days), function(i) {
  unlist(lapply(1:nrow(df), 
                funDaysExceed("species", "lifestage", "South", 22, 24))) 
}
)

hi <- funDaysExceed("species", "lifestage", "South", 22, 24)

data.frame(a = names_list, b = points)
test <- data.frame(
  name = names_list,
  points = I(points))

test$points
days$seasons <- days$I(seasons)
days$seasons <- as.list(days$seasons)
days$seasons
str(days)

dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
dat
testFunc <- function(a, b) a + b
apply(dat[,c('x','z')], 1, function(x) testFunc(x[1],x[2]))

dat$x[1]
dat$x[2]

testFunc <- function(a, b, c) {filter(a + b
apply(days[,c('tolerance', 'optimum')], 1, function(x) testFunc(x['optimum'],x['tolerance']))

days <- as.list(days)
functionCat(region) {
  filter(region =="Spring")
}
newmap <- map(days, functionCat, decreasing = TRUE)

days <- read_csv("Test_DaysExceedanceData.csv")
days2 <- days[c(1,4),]
days$seasons <- as.list(days$seasons)

do.call( function(optimum, tolerance,...) testFunc(optimum, tolerance), days)

do.call( function(species, lifestage, region, optimum, tolerance,...) funDaysExceed(species, lifestage, "South", optimum, tolerance), days2[1])
