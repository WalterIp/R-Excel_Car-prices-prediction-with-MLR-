###Step 0: Data preparation
raw_data.df <- read.csv("CarPrice_Assignment.csv")
View(raw_data.df)
str(raw_data.df) 


car1.df <- read.csv("Regression_Data.csv")
#dropping empty rows and columns
car2.df <- car1.df[1:205,]
car.df <- car2.df[,1:41]

View(car.df)
str(car.df)
names(car.df)


###Step 1: Choose the predictors (avoid multicollinearity)

#First step: correlation coefficient
install.packages("corrplot")
library(corrplot)
cor(car.df[1:41], car.df$price..Y.)
M <- cor(car.df[1:41], car.df[1:41])
N <- cor(car.df[1:10], car.df[1:10])

corrplot(M, method="color")

#Second step: ggplot to verify
library(ggplot2)
ggplot(data = car.df) + geom_point(aes(x = enginesize, y = price..Y.)) + 
  xlab("Engine Size") +
  ylab("Price")

ggplot(data = car.df)+ geom_boxplot(aes(x = factor(aspiration..1.std..0.turbo.), 
                                        y = price..Y.)) + 
  xlab("Aspiration (0:turbo, 1:std)") +
  ylab("Price")

ggplot(data = car.df) + geom_bar(aes(x = factor(enginelocation..1.front..0.back.), y = price..Y.), 
                                        stat = "summary", fun = "mean") +
  xlab("Engine Location (0:back, 1:front)") +
  ylab("Price")

ggplot(data = car.df) + geom_bar(aes(x = factor(five..1.yes..0.no.), y = price..Y.), 
                                 stat = "summary", fun = "mean") +
  xlab("5 Cylinders (0:no, 1:yes)") +
  ylab("Price")

ggplot(data = car.df)+ geom_boxplot(aes(x = factor(mpfi..1.yes..0.no.), 
                                        y = price..Y.)) + 
  xlab("MPFI fuel system (0:no, 1:yes)") +
  ylab("Price")

ggplot(data = car.df)+ geom_boxplot(aes(x = factor(X2..1.yes..0.no.), 
                                        y = price..Y.)) + 
  xlab("2 degree symboling (0:no, 1:yes)") +
  ylab("Price")
#not strong, so we eliminate it

#Third step: select the variables
selected.var <- c(1,7,16,24,31,33)

selected.car.df <- car.df[,selected.var]
View(selected.car.df)
names(car.df)


###Step 2: Partition data

set.seed(888)
train.index <- sample(1:nrow(selected.car.df), nrow(selected.car.df)*0.66)
train.df <- selected.car.df[train.index, ]
valid.df <- selected.car.df[-train.index, ]


###Step 3: Fit a MLR model

car.model <- lm(price..Y. ~., data = train.df)
summary(car.model)

###Step 4: Interpret the coefficients
#looking at the summary


###Step 5: Predict with the model
car.pred.price <- predict(car.model, newdata = valid.df)
car.pred.price
View(valid.df)


###Step 6: Evaluate the model
install.packages('forecast')
library(forecast)
accuracy(car.pred.price, valid.df$price..Y.)
accuracy(car.model$fitted.values, train.df$price..Y.)


