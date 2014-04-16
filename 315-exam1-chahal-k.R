attach(lab.exam.1a)

#Q1: To compare number of movies per year - HISTOGRAM
#Other options: stripchart; histogram could give wrong answer if not properly breaked.
hist(year, 
		 main="Histogram: Movies per Year from 1990 to 2005", 
		 xlim=c(1990, 2005), 
		 ylim=c(0, 70), 
		 xlab="Year")

#OR

stripchart(year, 
					 main="Strip Chart: Movies Released from 1990 to 2005",
					 method="jitter",
					 xlim=c(1990, 2005),
					 xlab="Year")

#Q2: To compare most popular rating - BARPLOT
#Other options: piechart, spinechart
barplot(table(mpaa), 
				main="Bar Plot: MPAA Ratings", 
				ylim=c(0, 400), 
				ylab="Number of Movies", 
				xlab="MPAA Rating", 
				col=c("green", "yellow", "red", "grey"))

#Q3: To compare most popular genre - SPINE CHART
#Other options: barplot, piechart
barplot(height=c(1,1,1,1), 
				width=table(Genre), 
				main="Spine Plot: Movie Genres", 
				xlab="Genre", space=0, 
				names.arg=c("Action", "ActionComedy", "ActionDrama", "Comedy"), 
				axes=F, 
				col=c(rgb(0, 0, 0), rgb(0.25, 0, 0), rgb(0.5, 0, 0), rgb(0.75, 0, 0)))

#Q4: To compare votes within each genre - DENSITY PLOTS
#Other options: could also do 4 separate histograms
#Distribution: very right skewed. Comedy has the most votes and Action has the least.
v.d = votes[Genre=="Drama"]
v.rd = votes[Genre=="RomanticDrama"]
v.rc = votes[Genre=="RomanticComedy"]

boxplot(v.d, v.rd, v.rc,
				main="Boxplot: Votes per Genre", 
				names=c("Drama", "RomanticDrama", "RomanticComedy"), 
				xlab="Genre", 
				ylab="Votes Cast", 
				ylim=c(0,300))

plot(density(v.d), 
		 xlim=c(0,150000), 
		 ylim=c(0,0.00009), 
		 col=1, 
		 lwd=2, 
		 main="Density Plot: Votes Cast by Genre")
lines(density(v.rd), 
			
			col=2, 
			lwd=2)
lines(density(v.rc), 
			
			col=3, 
			lwd=2)

legend(85000, 0.000085, 
			 lwd=2, 
			 col=c(1, 2, 3), 
			 c("Drama", "RomanticDrama", "RomanticComedy"))

#Q5: To compare two categorical variables - STACKED/SIDE-BY-SIDE BAR PLOT
#Other options: Perhaps 4 spine charts, side-by-side? *Mosaicplot, for 2 categorical.
#Association plot? Doesn't allow relative proportions.
#Action, ActionDrama and Comedy have the most R ratings. Comedy has most PG ratings.
#Doesn't seem to be a definite pattern.
r.action = mpaa[Genre=="Action"]
r.acomedy = mpaa[Genre=="ActionComedy"]
r.adrama = mpaa[Genre=="ActionDrama"]
r.comedy = mpaa[Genre=="Comedy"]

counts = table(lab.exam.1a$mpaa, lab.exam.1a$Genre)

barplot(counts, 
				main="Bar Plot: Genre vs. MPAA Rating", 
				ylim=c(0,250), xlab="Genre", 
				ylab="Frequency", 
				col=c("green", "blue", "red", "grey"), 
				legend=rownames(counts))

#OR

mosaicplot(counts, 
					 shade=T, 
					 main="Mosaic Plot: MPAA Ratings by Genre", 
					 xlab="Genre", 
					 ylab="MPAA Rating")

#Q6: To compare one categorical and one quantitative. - BOXPLOT
#Other options: Three stripcharts, but not enough data to do that.
l.bigbudget = length[BudgetRange=="BigBudget"]
l.modest = length[BudgetRange=="Modest"]
l.tiny = length[BudgetRange=="Tiny"]

boxplot(l.bigbudget, l.modest, l.tiny, 
				main="Boxplot: Length of Films by Budget", 
				names=c("BigBudget", "Modest", "Tiny"), 
				xlab="Budget", 
				ylab="Length (minutes)", 
				ylim=c(0,300))

#OR

par(mfrow=c(3,1))

stripchart(l.bigbudget)
stripchart(l.modest)
stripchart(l.tiny)

#Q7 To compare number of movies per genre over time - HISTOGRAM
#Other options: four stripcharts? Also not enough data points.
y.d = year[Genre=="Drama"]
y.rd = year[Genre=="RomanticDrama"]
y.rc = year[Genre=="RomanticComedy"]

par(mfrow=c(1,3))

stripchart(y.d, method="jitter")
stripchart(y.rd, method="jitter")
stripchart(y.rc, method="jitter")

hist(y.d, 
		 ylim=c(0,80), xlim=c(1990,2005), 
		 breaks=15, 
		 main="Histogram: Drama Movies per Year", 
		 xlab="Year", ylab="Number of Movies")
hist(y.rd, 
		 ylim=c(0,80), xlim=c(1990,2005), 
		 breaks=15, 
		 main="Histogram: RomanticDrama Movies per Year", 
		 xlab="Year", ylab="Number of Movies")
hist(y.rc, 
		 ylim=c(0,80), xlim=c(1990,2005), 
		 breaks=15, 
		 main="Histogram: RomanticComedy Movies per Year", 
		 xlab="Year", ylab="Number of Movies")


#Q8 Movie budget vs. Rating - DENSITY PLOT
#Other options: scatterplot? 3 stripcharts?
r.bigbudget = rating[BudgetRange=="BigBudget"]
r.modest = rating[BudgetRange=="Modest"]
r.tiny = rating[BudgetRange=="Tiny"]

par(mfrow=c(1,1))

plot(density(r.bigbudget), 
		 main="Density Plot: User Ratings by Budget")
lines(density(r.modest), col=2)
lines(density(r.tiny), col=3)

legend(7.25, 0.38, 
			 lwd=1, 
			 col=c(1, 2, 3), 
			 c("BigBudget", "Modest", "Tiny"))



#discuss advantages, disadvantages and parameter choices
#adequate graph
#answer question
#legend
#don't squish axes

#SCATTERPLOT with fit line
fit = loess(v1~v2)
plot(v2, v1, 
		 main="Length of Bridges Built in Pittsburgh vs. Year of Erection", 
		 xlab="Year of Erection")
lines(v2, 
			fit$fitted)

#AVERAGE SHIFTED HISTOGRAM
source("ashcode.R")
ashcode1D(Erected,8,10,plot.hist=FALSE)
#8 is the number of shifted histograms and 10 is the number of bins. Results in a
#smooth histogram. For univariate data. 