install.packages("ggplot2")
install.packages("gridExtra")

library(ggplot2)
library(gridExtra)
# http://had.co.nz/ggplot2


colnames(diamonds)
# qplot examples -------------------------------------------------------------



qplot(diamonds$cut, diamonds$carat)
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, colour=clarity)
qplot(carat, price, data = diamonds, geom=c("point", "smooth"), method=lm)

qplot(carat, data = diamonds, geom="histogram")
qplot(carat, data = diamonds, geom="histogram", binwidth = 1)
qplot(carat, data = diamonds, geom="histogram", binwidth = 0.1)
qplot(carat, data = diamonds, geom="histogram", binwidth = 0.01)


Same output!
qplot(clarity, data=diamonds, fill=cut, geom="bar")
Test <- ggplot(diamonds, aes(clarity, fill=cut)) 
Test + geom_bar()

# Scatterplot

qplot(wt, mpg, data=mtcars)
qplot(wt, mpg, data=mtcars, color=qsec)
qplot(wt, mpg, data=mtcars, color=qsec, size=3)
# Wyrzucamy z legendy dziwną trójkę.
qplot(wt, mpg, data=mtcars, colour=qsec, size=I(3))
# Tu zamiast kolorów nanosimy intensywność ?
qplot(wt, mpg, data=mtcars, alpha=qsec)



# using ggplot() -------------------------------------------------------------
d <- ggplot(diamonds, aes(x=carat, y=price))
d + geom_point()
d + geom_point(aes(colour = carat))
# d + geom_point(aes(colour = carat)) + scale_colour_brewer()

ggplot(diamonds) + geom_histogram(aes(x=price))

# Separation of statistcs and geometric elements -----------------------------

p <- ggplot(diamonds, aes(x=price))

p + geom_histogram()
p + stat_bin(geom="area")
p + stat_bin(geom="point")
p + stat_bin(geom="line")

p + geom_histogram(aes(fill = clarity))
p + geom_histogram(aes(y = ..density..))

# Setting vs mapping ---------------------------------------------------------
p <- ggplot(diamonds, aes(x=carat,y=price))

# What will this do?
p + geom_point(aes(colour = "green"))
p + geom_point(colour = "green")
p + geom_point(colour = colour)


#### Further tests.

head(mtcars)
qplot(wt, mpg, data=mtcars, colour=cyl)
levels(mtcars$cyl)
qplot(wt, mpg, data=mtcars, colour=factor(cyl))

# use different aesthetic mappings
qplot(wt, mpg, data=mtcars, shape=factor(cyl))
qplot(wt, mpg, data=mtcars, size=qsec)


# combine mappings (hint: hollow points, geom-concept, legend combinationqplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb))
# combine mappings (hint: hollow points, geom-concept, legend combqplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb))
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb))
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=I(1))
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=I(1))
qplot(wt, mpg, data=mtcars, size=qsec, shape=factor(cyl), geom="point")
qplot(wt, mpg, data=mtcars, size=factor(cyl), geom="point")

# bar-plot
P <- qplot(factor(cyl), data=mtcars, geom="bar")
# flip plot by 90°
Q <- qplot(factor(cyl), data=mtcars, geom="bar") + coord_flip()

# To działa na ggplot zamiast zmiany na par.
sidebysideplot <- grid.arrange(P, Q, ncol=2)

library(gridExtra)
# difference between fill/color bars
R <- qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))
W <- qplot(factor(cyl), data=mtcars, geom="bar", colour=factor(cyl))

sidebysideplot <- grid.arrange(P, Q, R, W, ncol=4)
# fill by variable

# To pokazuje możliwość dodania kolorów do jednej z dwóch cech na plocie, albo dodania kolorów charakteryzujących trzecią cechę.
Z <- qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))
sidebysideplot <- grid.arrange(R, Z, ncol=2)


# use different display of bars (stacked, dodged, identity)
# To można użyć ładnie dla pokazania wykształcenia i innych zmiennych jakościowych.
head(diamonds)
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="stack")
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="dodge")
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="fill")
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="identity")

# Trochę bez sensu łączyć linią te punkty - oś x nie jest uporządkowana.
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut, position="identity")
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut, position="stack")


# using pre-calculated tables or weights (hint: usage of ddply in package plyr)
library(plyr)

# To najładniejsze contingency tables. jakie do tej pory widziałem.

table(diamonds$cut)
t.table <- ddply(diamonds, c("clarity", "cut"), "nrow")
head(t.table)
t.table
qplot(cut, nrow, data=t.table, geom="bar", stat="identity")
qplot(cut, nrow, data=t.table, geom="bar", stat="identity", fill=clarity)

qplot(cut, data=diamonds, geom="bar", weight=carat)
qplot(cut, data=diamonds, geom="bar", weight=carat, ylab="carat")

### excursion ddply (split data.frame in subframes and apply functions)
ddply(diamonds, "cut", "nrow")
ddply(diamonds, c("cut", "clarity"), "nrow")
ddply(diamonds, "cut", mean)
ddply(diamonds, "cut", summarise, meanDepth = mean(depth))
ddply(diamonds, "cut", summarise, lower = quantile(depth, 0.25, na.rm=TRUE), median = median(depth, na.rm=TRUE),
upper = quantile(depth, 0.75, na.rm=TRUE))

# Fajne jest tu to, że można od razu obliczyć wartości funkcji na danych podzbiorach.
t.function <- function(x,y){ 
	z = sum(x) / sum(x+y)
	return(z)
}
ddply(diamonds, "cut", summarise, custom = t.function(depth, price))
ddply(diamonds, "cut", summarise, custom = sum(depth) / sum(depth + price))

# Działa nawet na bardziej skomplikowanych zapytaniach.
ddply(diamonds, c("cut", "clarity"), summarise, custom = sum(depth) / sum(depth + price))

### back to ggplot

# histogram
qplot(carat, data=diamonds, geom="histogram")
# change binwidth
qplot(carat, data=diamonds, geom="histogram", binwidth=0.1)
qplot(carat, data=diamonds, geom="histogram", binwidth=0.01)

# use geom to combine plots (hint: order of layers) Trzeba sprawdzić, co robi smooth..
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"))
qplot(wt, mpg, data=mtcars, geom=c("smooth", "point"))
qplot(wt, mpg, data=mtcars, color=factor(cyl), geom=c("point", "smooth"))
     

# tweeking the smooth plot ("loess"-method: polynomial surface using local fitting)
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"))
# removing standard error
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), se=FALSE)

