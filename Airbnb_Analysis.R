

#New data set as Airbnb

Airbnb <- read.csv("Airbnb.csv")

#summary
summary(Airbnb)


library(dplyr)
glimpse(Airbnb)

#first 6 and last 6 observations
head(Airbnb)
tail(Airbnb)

#columns
names(Airbnb)

#checking if any NA values
any(is.na(Airbnb))

#Viewing NA values columns
Airbnb[!complete.cases(Airbnb),]

#Omiting all NA values and new data set without NA values
Airbnb_cleaned <- na.omit(Airbnb)

#rechecking NA 
any(is.na(Airbnb_cleaned))


names(Airbnb_cleaned)

#rows
count(Airbnb_cleaned)

#summary of new dataset
summary(Airbnb_cleaned)

View(Airbnb_cleaned)



library(dplyr)
glimpse(Airbnb_cleaned)

#selecting and grouping
Airbnb_cleaned %>%
  group_by(room_type,minstay)


#new column actual_rate = price divide with minstay
actual_rate<-(Airbnb_cleaned$price/Airbnb_cleaned$minstay) 
#rounding values
actual_rate <- round(actual_rate)
#column binding to dataset
Airbnb_cleaned <-cbind(actual_rate,Airbnb_cleaned)

View(Airbnb_cleaned)

#mean of actual rate
actual_rate_mean <-mean(Airbnb_cleaned$actual_rate)

#dividing rooms rates as less than mean and more than mean
Rooms_withless_rates <- Airbnb_cleaned[Airbnb_cleaned$actual_rate <= actual_rate_mean,]
View(Rooms_withless_rates)

Rooms_withmore_rates <- Airbnb_cleaned[Airbnb_cleaned$actual_rate > actual_rate_mean,]
View(Rooms_withmore_rates)


cheap_room_satisfaction <-mean(Rooms_withless_rates$overall_satisfaction)
luxury_room_satisfaction <- mean(Rooms_withmore_rates$overall_satisfaction)

#boxplot of mean if cheap and luxury room overall_satisfaction


boxplot(Rooms_withless_rates$overall_satisfaction,Rooms_withmore_rates$overall_satisfaction,main="Boxplot of Overall Satisfaction",xlab="Cheap rooms and Highrate rooms",ylab="Overall Satisfaction",col="red")


#levels of borough
factor(Airbnb_cleaned$borough)

Borough<- table(Airbnb_cleaned$borough)
prop.table(Borough)*100


#pie chart of borough
library(pie3D)

pie2<- sort(table(Airbnb_cleaned$borough),decreasing = TRUE)
borough_counts <- c(10127,7158,1325,167,61 )
labels<- c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
percent<- round(borough_counts/sum(borough_counts)*100)

labels <- paste(labels, percent) # add percents to labels 
labels <- paste(labels,"%",sep="") # add % to labels 
pie3D(pie2,main="Borough Cateory",labels=labels,explode = 0.045)

Neighborhood <- table(Airbnb_cleaned$neighborhood)
prop.table(Neighborhood)

#table for room_type
table(Airbnb_cleaned$room_type)

#room satisfaction histogram
hist(Airbnb$overall_satisfaction,col='red',ylim=c(0,12000),xlab="Overall Satisfaction",ylab="Frequency",main="Overall satisfaction")

#room reviews histogram
hist(Airbnb_cleaned$reviews,col="yellow",xlim = c(0,150),ylim=c(0,15000),breaks = 15,ylab="Frequency",xlab="Reviews",main="Histogram of Reviews")

#room satisfaction by room_type
boxplot(overall_satisfaction ~ room_type,data= Airbnb_cleaned,col="yellow",xlab="Room type",ylab="Overall satisfaction",main="Satisfaction according to room type")

#room satisfaction by borough
boxplot(overall_satisfaction ~ borough,data=Airbnb_cleaned,col="red",xlab="Borough",ylab="Overall Satisfaction",main="Satisfaction according to borough")

#plot price and bedrooms scatterplot
plot(Airbnb_cleaned$price, Airbnb_cleaned$bedrooms, main="Scatterplot for Price and Bedrooms", 
     xlab="Price ", ylab="Bedrooms", pch=19)

#plot price and reviews scatterplot

plot(Airbnb_cleaned$price, Airbnb_cleaned$reviews, main="Scatterplot for Price and Reviews", 
     xlab="Price ", ylab="Reviews", pch=19)

#plot price and accommodates scatterplot
plot(Airbnb_cleaned$price, Airbnb_cleaned$accommodates, main="Scatterplot for Price and Accommodates", 
     xlab="Price ", ylab="Accommodates", pch=19)


#aggregate and preparing barchart of overall_satisfaction ,borough,room_type by grouping room_type

library(lattice)
seg.agg <- aggregate(overall_satisfaction ~ borough +room_type,data=Airbnb_cleaned,mean)
barchart(overall_satisfaction ~ borough, data=seg.agg, 
         groups=room_type, auto.key=TRUE,
         par.settings = simpleTheme(col=c("red", "green","blue")))

#aggregate and preparing barchart of reviews ,borough,room_type by grouping room_type
seg.agg2 <- aggregate(reviews ~ borough +room_type,data=Airbnb_cleaned,mean)
barchart(reviews ~ borough, data=seg.agg2,ylim=c(0,25), 
         groups=room_type, auto.key=TRUE,
         par.settings = simpleTheme(col=c("red", "green","blue")))


#aggregate and preparing barchart of actual_rate,borough,room_type by grouping actual_rate
se.agg3 <- aggregate(actual_rate ~ borough + room_type,data=Airbnb_cleaned,mean)
barchart(actual_rate ~borough,data=se.agg3,
         group=room_type,auto.key=TRUE,par.settings=simpleTheme(col=c("red","green","blue")))

#scatterplot of price and reviews

plot(Airbnb_cleaned$price,Airbnb_cleaned$reviews, main="Scatterplot of Price and Reviews",
     xlab="price", ylab="reviews ", pch=1,xlim=c(0,15000),ylim=c(0,250))


#qplot of overall_satisfaction and price by borough
library(ggplot2)

qplot( overall_satisfaction,price, data = Airbnb_cleaned, colour = borough)

#Corrgram of all obs
library(corrgram)
corrgram(Airbnb_cleaned, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrogram of Airbnb")

#corrplot of actual_rate,overall_satisfaction,reviews,bedrooms,minstay,accommodates,price

Airbnb_cleaned
library(corrplot)    
corrplot(corr=cor(Airbnb_cleaned[ , c(1,7:12)], use="complete.obs"), 
         method ="ellipse")

#corrplot mixed of  actual_rate,overall_satisfaction,reviews,bedrooms,minstay,accommodates,price

library(gplots)      
par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(Airbnb_cleaned[ , c(1,7:12)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))

#variance  of overall_satisfaction and price
var(Airbnb_cleaned$overall_satisfaction,Airbnb_cleaned$price, na.rm = TRUE)

#covariance of all_satisfaction,price,accommodates,reviews,bedrooms,minstay
cov(Airbnb_cleaned$overall_satisfaction,Airbnb_cleaned$price+Airbnb_cleaned$accommodates+Airbnb_cleaned$reviews+Airbnb_cleaned$bedrooms+Airbnb_cleaned$minstay, use = "complete.obs", method = "pearson")



#Multiple linear regression

fit <- lm(Airbnb_cleaned$overall_satisfaction ~ Airbnb_cleaned$borough )
summary(fit)

fit2 <- lm(Airbnb_cleaned$overall_satisfaction ~ Airbnb_cleaned$room_type)
summary(fit2)


fit3 <- lm(overall_satisfaction ~ reviews+bedrooms+accommodates+actual_rate,data=Airbnb_cleaned)
summary(fit3)

fit4 <-lm(formula = price ~ room_type + neighborhood + accommodates + bedrooms 
          + minstay, data = Airbnb_cleaned)
    
summary(fit4)

#fucntions for fit
coefficients(fit) 
confint(fit, level=0.95) 
fitted(fit) 
residuals(fit) 

#fucntions for fit2
coefficients(fit2) 
confint(fit2, level=0.95) 
fitted(fit2) 
residuals(fit2) 


#t-test
t.test(Airbnb_cleaned$overall_satisfaction,Airbnb_cleaned$actual_rate) 


# paired t-test
t.test(Airbnb_cleaned$overall_satisfaction,Airbnb_cleaned$bedrooms,paired=TRUE) 

t.test(Airbnb_cleaned$overall_satisfaction,Airbnb_cleaned$actual_rate,paired = TRUE) 


res <- t.test(Airbnb_cleaned$overall_satisfaction, mu = 25)




# printing the mean
res$estimate

# printing the confidence interval
res$conf.int



########################################Insights################################

  lm(formula = price ~ room_type + neighborhood + accommodates + bedrooms 
          + minstay, data = Airbnb_cleaned)

#The table above presents the effects for the predictors. 
# 1 ) The second column shows the parameter estimates: a negative estimate means that there is 
# a negative impact to the price. The opposite can be said for a positive estimate. 
# 2 )The final column indicates the significance of the pattern: the more stars, the more significant
#the effect ( for p < 0.05). 
# 3) When testing for significance we use a simple two-sided t-test.
# When testing we test for the variables parameter estimate to be  Since we have a large test size, 
# our distribution will be approximately normal distributed. 
# 4 ) The critical value for a normal distribution on a 95% confidence interval is 1.96.
# 5) Surprisingly the occupancy of apartments has a positive significant effect on the price.
#The more people who can live in the apartment the more expensive it should be, as the visitors will
# probably split the costs. Also, the accommodation variable could be positive correlated with the size
#of the apartment.which would also lead to higher prices.

#6 )The final two variables does not show any significant influence on the price
