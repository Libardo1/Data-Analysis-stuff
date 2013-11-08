load("/Users/karenyang/Desktop/Assignment1Folder/figure/loansData.rda")

dim(loansData) #2500 observations by 14 variables
str(loansData) #The number of observations is 2500 and the number of variables is 14
sapply(loansData[1, ], class)
summary(loansData)
class(loansData)
sum(is.na(loansData)) #7 are NA
table(is.na(loansData)) #34993 is FALSE and 7 is TRUE
data <- na.omit(loansData)
str(data) #the 7 NAs were in 2 rows only. The number of observations is 2498
table(is.na(data)) #34972 is FALSE
head(data)

#Fix Interest rate to be numeric  Interest.Rate
levels(data$Interest.Rate) 
RateChar <- as.character(data$Interest.Rate)
RateGsub <- gsub("%", "", RateChar)
Interest.Rate <- (as.numeric(RateGsub))/100
quantile(Interest.Rate) #%The min interest rate is 0.54 while the max is 0.25, median = 0.13
table(Interest.Rate)
hist(Interest.Rate, freq = FALSE, col = "blue")
Interest.Rate.Density <- density(Interest.Rate)
lines(Interest.Rate.Density, lwd = 3)
data$Interest.Rate <- Interest.Rate

#Fix Debt to Income Ratio to be numeric DI.Ratio
Debt.Income.RatioChar <- as.character(data$Debt.To.Income.Ratio)
DIRatioGsub <- gsub("%", "", Debt.Income.RatioChar)
DI.Ratio <- (as.numeric(DIRatioGsub))/100
quantile(DI.Ratio)
hist(DI.Ratio, freq = FALSE, col = "pink")
DI.Ratio.Density <- density(DI.Ratio)
lines(DI.Ratio.Density, lwd = 2)
data$Debt.To.Income.Ratio <- DI.Ratio

#Fix Loan.Length variable to remove "months"
LoanLengthChar <- as.character(data$Loan.Length)
LoanLengthGSub <- gsub("months", "", LoanLengthChar)
Loan.Length <- as.numeric(LoanLengthGSub)
Loan.LengthFactor <- as.factor(Loan.Length)
levels(data$Loan.Length) #Levels are 0 months, 36 months, and 60 months
#Fix Loan.Length by removing "" factor level 1
data$Loan.Length <- factor(data$Loan.Length)
levels(data$Loan.Length)
table(Loan.Length) #1950 loans are for 36 months and 548 loans are for 60 months
barplot(table(Loan.LengthFactor), col = "purple")
data$Loan.Length <- Loan.LengthFactor

quantile(data$Amount.Requested) #Smallest loan requested is $1K and the largest is $35K, median is $10K
hist (data$Amount.Requested)
hist(data$Amount.Requested, freq = FALSE, col = "orange")
AmountDensity <- density(data$Amount.Requested)
lines(AmountDensity, col = "red", lwd = 3)

quantile(data$Amount.Funded.By.Investors) #Same as amount requested
hist(data$Amount.Funded.By.Investors, freq = FALSE, col = "orange")
Amount.Funded.By.Investors.Density <- density(data$Amount.Funded.By.Investors)
lines(Amount.Funded.By.Investors.Density, lwd = 3)
with(data, cor(data$Amount.Requested, data$Amount.Funded.By.Investors)) # Correlation is 0.969


str(data$Loan.Purpose) #Factor with 15 levels, 1st factor level is ""
#Fix "" for factor level 1
data$Loan.Purpose <- factor(data$Loan.Purpose)
levels(data$Loan.Purpose)
table(data$Loan.Purpose) #Debt consolidation has the highest frequency and the second highest is credit card

nlevels(data$Home.Ownership)
levels(data$Home.Ownership)
#Fix 1st level of ""
data$Home.Ownership <- factor(data$Home.Ownership)
levels(data$Home.Ownership)
table(data$Home.Ownership) #1148 have mortgages while 1145 rent, roughly equivalent, only 200 own

class(data$Monthly.Income)
quantile(data$Monthly.Income) #mean monthly income is $5K
summary(data$Monthly.Income)
hist(data$Monthly.Income, freq = FALSE, col = "brown")
Monthly.Income.Density <- density(data$Monthly.Income)
lines(Monthly.Income.Density, lwd = 3)
hist(log(data$Monthly.Income + 1), freq = FALSE, col = "blue")
logMonthlyIncome <-log(data$Monthly.Income + 1)
data$Monthly.Income <- logMonthlyIncome

levels(data$FICO.Range)
#Fix problem with "" for 1st factor level
data$FICO.Range <- factor(data$FICO.Range)
nlevels(data$FICO.Range)
str(data$FICO.Range) #a factor variable with 38 levels
data$FICO.Range <- factor(data$FICO.Range)
table(data$FICO.Range)
data$FICO.Range <- as.numeric(data$FICO.Range)



class(data$Open.CREDIT.Lines)
quantile(data$Open.CREDIT.Lines) #mean number of open lines is 9
hist(data$Open.CREDIT.Lines, freq = FALSE, col = "green") 
Open.Credit.Density <- density(data$Open.CREDIT.Lines)
lines(Open.Credit.Density, lwd = 3)

class(data$Revolving.CREDIT.Balance)
quantile(data$Revolving.CREDIT.Balance) #median balance is $10,962.00
hist(data$Revolving.CREDIT.Balance, freq = FALSE, col = "purple")
RevolvingDensity <- density(data$Revolving.CREDIT.Balance)
lines(RevolvingDensity, lwd = 3, col = "red")

class(data$Inquiries.in.the.Last.6.Months)
table(data$Inquiries.in.the.Last.6.Months) #1250 had no inquires, 657 had only 1 inquiry
quantile(data$Inquiries.in.the.Last.6.Months)
hist(data$Inquiries.in.the.Last.6.Months, freq = FALSE, col = "green")
InquiriesDensity <- density(data$Inquiries.in.the.Last.6.Months)
lines(InquiriesDensity, lwd = 3)


class(data$Employment.Length)
str(data$Employment.Length) 
levels(data$Employment.Length)
#Fix the problem of "" in 1st factor level
data$Employment.Length <- factor(data$Employment.Length)
levels(data$Employment.Length)
table(data$Employment.Length) #Factor level 3 is 10+ years of employment = 653 individuals



cor(data$Amount.Requested, data$Interest.Rate) #0.33
cor(data$Amount.Funded.By.Investors, data$Interest.Rate) #0.34
cor(data$Loan.Length, data$Interest.Rate) #Not available
cor(data$Loan.Purpose, data$Interest.Rate) #not available
cor(data$Debt.To.Income.Ratio, data$Interest.Rate) #0.17
cor(data$State, data$Interest.Rate)#not available
cor(data$Home.Ownership, data$Interest.Rate) #not available
cor(data$Monthly.Income, data$Interest.Rate) #0.04
cor(data$FICO.Range, data$Interest.Rate) #-0.71
cor(data$Open.CREDIT.Lines, data$Interest.Rate) #0.09
cor(data$Revolving.CREDIT.Balance, data$Interest.Rate) #0.06
cor(data$Inquiries.In.The.Last.6.Months, data$Interest.Rate) #error
cor(data$Employment.Length, data$Interest.Rate) #error

#Interest Rate & FICO score (BASE MODEL)
par(mar(2,2,1,1))
plot(data$FICO.Range, data$Interest.Rate, ylab ="Interest Rate (%)", xlab ="FICO Score Range ( with levels 1-38 representing ranges 640-644 to 845-850)", cex.lab = 0.75, cex.axis = 0.5, pch = 19, col = "orange")
cor(data$Interest.Rate, data$FICO.Range)
lm1 <- lm(data$Interest.Rate ~ data$FICO.Range)
lines(data$FICO.Range, lm1$fitted, col = "black", lwd = 2)
summary(lm1)
title("A Negative Relationship Between Interest Rate and FICO Score Range")
legend("topright", legend = "Correlation = -0.71")
mtext("Figure 1. Plot of Interest Rate and FICO score range with regression line. Data source is the Lending Club.", side = 1, line = 4, cex = 0.75)


plot(data$Monthly.Income, data$Interest.Rate, pch= 19, col = "purple")
lm2 <- lm(data$Interest.Rate ~ data$FICO.Range + (data$Monthly.Income))
summary(lm2)


plot(data$Amount.Requested, data$Interest.Rate, pch = 19, col = "orange")
lm3 <-lm(data$Interest.Rate ~ data$FICO.Range + data$Amount.Requested)
summary(lm3)

plot(data$Amount.Funded.By.Investors, data$Interest.Rate, pch = 19, col = "orange")
lm4 <- lm(data$Interest.Rate ~ data$FICO.Range + data$Amount.Funded.By.Investors)
summary(lm4)

plot(data$Debt.To.Income.Ratio, data$Interest.Rate, pch = 19, col = "orange")
lm5 <-lm(data$Interest.Rate ~ data$FICO.Range + data$Debt.To.Income.Ratio)
summary(lm5) #not statistically significant

plot(data$Open.CREDIT.Lines, data$Interest.Rate, pch = 19, col = "orange")
lm6 <- lm(data$Interest.Rate ~ data$FICO.Range + data$Open.CREDIT.Lines)
summary(lm6)  #at statistical significance at p-value 0.05

plot(data$Revolving.CREDIT.Balance, data$Interest.Rate, pch = 19, col = "orange")
lm7 <- lm(data$Interest.Rate ~ data$FICO.Range + data$Revolving.CREDIT.Balance)
summary(lm7) #statistically significant

table(data$Employment.Length)
plot(data$Employment.Length, data$Interest.Rate, pch = 19, col = "orange")
lm8 <- lm(data$Interest.Rate ~ data$FICO.Range + relevel(data$Employment.Length, ref = "< 1 year"))
summary(lm8) #statistically significant


plot(data$Loan.Purpose, data$Interest.Rate, pch = 19, col = "orange")
lm9 <- lm(data$Interest.Rate ~data$FICO.Range + data$Loan.Purpose)
summary(lm9)

plot(data$Loan.Length, data$Interest.Rate, pch = 19, col = "orange")
lm10 <- lm(data$Interest.Rate ~ data$FICO.Range + data$Loan.Length)
summary(lm10)

plot(data$Home.Ownership, data$Interest.Rate, pch = 19, col = "orange")
lm11 <- lm(data$Interest.Rate ~ data$FICO.Range + data$Home.Ownership)
summary(lm11)

lm17 <-lm(data$Interest.Rate ~ data$FICO.Range  + data$Monthly.Income + data$Debt.To.Income.Ratio + relevel(data$Employment.Length, ref = "< 1 year"))
summary(lm17)
