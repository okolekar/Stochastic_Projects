##### Stochastic Methods for Material Science Programming Project 1 #####
### by Omkar Kolekar 

###############################################################################################################################################################
## Task 1 ##
#location A and locationB -> Vectors of values Cd content (in mg/g fresh wgt)
locationA = c(76.8,72.3,74.0,73.2,46.1,76.5,61.9,62.4,65.9,62.4)
locationB = c(64.4,60.0,59.4,61.2,52.0,58.1,62.0,57.8,57.2)
par(mar=c(2,2,2,2)) # Without this command the box plot was not working.
#It showed the error Figure margin too large.
#You can turn it off if this causes a problem in your code

#Answer 1.a) Draw Box plot
names = c("location A", "location B") #names vector for box plot X axis value 
boxplot(locationA,locationB,main="Cd contamination of trout in a river",ylab="mg/g fresh weight",names=names) 
#Observations from the boxplot are as follows
# 1) At locationA we see median is slightly closer to Q3 indicating a slightly left skewed distribution
# 2) At locationB we see outliners and quite possible a perfect normal distribution as median appears to be at center

#Answer 1.b) Testing for Normality Of DATA
shapiro.test(locationA)
shapiro.test(locationB)
#In the above shapiro test the P value of both the test is above alpha/2 = 0.025.
# Pvalue of locationA = 0.095
# Pvalue of locationB = 0.7679
#Hence we do not reject the assumption that Cd contents measured at both locations can be regarded as realizations of a normally distributed random variable.

#Answer 1.c)  F test to compare two variances
var.test(locationA,locationB)
#Since P value = 0.01022 is below our alpha = 0.025 we reject the assumption that the variances are equal

#Answer 1.d)  Welch Two Sample t-test
t.test(locationA,locationB,var.equal = FALSE)
#mean at locationA = 67.15000
#mean at locationB = 59.12222
#From the above, we clearly say that the expected value of Cd content at A is far greater that that of the B.


###############################################################################################################################################################
## TASK 2 ##
#before and after -> Vectors of values The measured temperatures before and 3 hours after taking a drug can be found for ten
#different patients in the following table:
before = c(38.4,39.6,39.4,40.1,39.2,38.5,39.3,39.1,38.4,39.5)
after =c(37.6,37.9,39.1,39.4,38.6,38.9,38.7,38.7,38.9,38.7)

#Answer 1.a) sample situation
#Since the data here after and before taking the drug, was taken from an individual its a paired sample situation
#The 1st before and after value corresponds to 1st individual.. the second corresponds to second and so on
#Hence paired sample

#Answer 1.b) scatter plot of the data
par(mar=c(5,5,5,5))
plot(before,after,xlab="Temp before drug", ylab=" Temp after drug")
# The scatter plot shows that there could be a linear relation between the Temperature before and after taking the drug

#Answer 1.c) Correlation Estimation between the temperature values before and after taking the drug.
cor(before,after)
# A correlation value of 0.348 which is less than 1 but more than 0 shows that there is a weak positive linear relation between two data set.

#Answer 1.d) Test for level α = 0.05 if the correlation is significant
cor.test(before,after)
#Pearson's product-moment correlation best used when results have already been plotted on a scatter graph
#and there is an indication of a linear relationship between the two factors.
#As here it can be said that there is a weak linear relation between the two data set, 
#hence we have used the Pearson's Test.
#And a correlation value of 0.3485987 indicates not a very significant correlation


##########################################################################################################################################################
## TASK 3 ##

#Answer 1.a) Loading the data in coaldata.
coaldata = scan("coal_data.txt")
n = length(coaldata) #the number of observations taken into consideration
L = max ( coaldata  ) - min ( coaldata  )
b = seq ( min ( coaldata  ), max ( coaldata  ), L/14)
hist ( coaldata  , breaks = b,freq=0)
lines ( density ( coaldata ),lwd =3)
#The histogram shows an exponential distribution. As this distribution is based on time intervals between events.

#Answer 1.b) Confidence interval for the mean value of days between two disasters for confidence
#level of 95%.

# estimation of the mean for coaldata
mu_coaldata = mean(coaldata)

# estimation of the variance
var_coaldata = var(coaldata)

# estimation of the standard deviation of the coaldata
sd_coaldata = sd(coaldata)

# Calculating half width of confidence interval using quantile
# of the t-distribution by qt
alpha = 0.05 #confidence level
del = sd_coaldata/sqrt(n)*qt(1-0.5*alpha,n-1)
#half width of the data is equal to 44.87

#lower and the upper limit of the confidence level
LL = mu_coaldata - del
UL = mu_coaldata + del
#Upper limit of the confidence level = 258.29
#Lower limit of the confidence level = 168.55
