
  #Import cc.csv file(Converted from BankCreditCard)
  
  # Import libraries
  
library(dplyr)
library(car)
library(psych)
library(ggplot2)
library(PerformanceAnalytics)
library(VIM)
library(InformationValue)


#No of observations and no of variables

dim(cc)



#Summary of All rows and columns

summary(cc)

#Convert categorical variables in factor
bcc=cc
ac=cc
bcc[, c(3,4,5,7,8,9,10,11,12,27)] <- sapply(bcc[, c(3,4,5,7,8,9,10,11,12, 27)], as.factor)

####Defaulting status Academic_Qualification_wise(Pie Chart)#########
p <- ggplot(data=bcc, aes(x=factor(1), stat="bin", fill=Academic_Qualification)) +
  geom_bar(position="fill") 
p <- p + ggtitle("Defaulting status Academic_Qualification_wise") + xlab("") +
  ylab("Non Defaulters                 Defaulters") # Adds titles
p <- p + facet_grid(facets=. ~ Default_Payment) 
p <- p + coord_polar(theta="y") 
p
str(acc)

####Defaulting status Academic_Qualification_wise(Bar Chart)#########
i <- ggplot(bcc, aes(x=Gender))
i + geom_bar(aes(fill = Academic_Qualification), width = 0.7) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="") + 
  facet_wrap(~Default_Payment)


# Male Default pattern agewise 
bcc %>%
  filter( Gender== 1) ->pcc
ah <- ggplot(pcc, aes(x = Age_Years))

ah + geom_bar(aes(fill=Default_Payment), width = .9) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5)) + 
  labs(title="Male Default pattern agewise", 
       subtitle="")


# FeMale Default pattern agewise
bcc %>%
  filter( Gender== 2) ->pcc1
ai <- ggplot(pcc1, aes(x = Age_Years))

ai + geom_bar(aes(fill=Default_Payment), width = .9) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5)) + 
  labs(title="FeMale Default pattern agewise", 
       subtitle="")

#######Box plot of comparision of age credit amount with age
par(mfrow= c(1,1))
bcc %>%
  filter( Credit_Amount< 700000) ->pcc2

boxplot(Credit_Amount~Age_Years, data= pcc2, col= heat.colors(4), main = "Comparision of Credit_Amount with Age")


#Box plot of comparision of age credit amount with age(For defaulters only)
bcc %>%
  filter( Credit_Amount< 700000 & Default_Payment == 1) ->pcc3

boxplot(Credit_Amount~Age_Years, data= pcc3, col= heat.colors(4), main = " Defaulters Comparision with Credit_Amount & Age ")


#Scatter plot age vs creditamount
bcc %>%
  filter( Credit_Amount< 700000 ) ->pcc2
scatterplot(Age_Years ~ Credit_Amount| Marital, data = pcc2 )

library(ggplot2)
ggplot(pcc2, aes(x = Age_Years, y = Credit_Amount)) +
  labs(title = 'Age and Credit amount Variation with Marital status',
       x = 'Age_Years', 
       y = 'Credit_Amount') + geom_point(aes(color= Marital)) +
  geom_density_2d()

#Scatter plot age vs creditamount (Only defaulter) 
bcc %>%
  filter( Credit_Amount< 700000 & Default_Payment==1) ->pcc3
ggplot(pcc3, aes(x = Age_Years, y = Credit_Amount)) +
  labs(title = 'Age and Credit amount Variation with Marital (Defaulter)',
       x = 'Age_Years', 
       y = 'Credit_Amount') + geom_point(aes(color= Marital)) +
  geom_density_2d()


#sample Test and train
set.seed(100)
acc=cc
sam<-sample(x=1:nrow(acc),size=0.8*nrow(acc))
train<-acc[sam,]
test=acc[-sam,]
dim(train)
dim(test)
trn=train
tst=test
```

# Create model1
model1 <- glm(Default_Payment~ Age_Years + Credit_Amount +  Repayment_Status_Jan +
                Repayment_Status_June + Previous_Payment_Jan ,
              data = trn, family= binomial(link= "logit") )

summary(model1)


predicted= predict(model1, tst, type="response")
head(predicted)

optcutoff= optimalCutoff(tst$Default_Payment, predicted)[1]

optcutoff

misClassError(tst$Default_Payment, predicted, threshold = optcutoff)


table(tst$Default_Payment)



plotROC(tst$Default_Payment, predicted)


sensitivity(tst$Default_Payment, predicted)



specificity(tst$Default_Payment, predicted)

confusionMatrix(tst$Default_Payment, predicted)

# Model 2
model2 <- glm(Default_Payment~  
                Repayment_Status_Jan + Age_Years +
                Repayment_Status_March +
                Repayment_Status_June +
                Total_payment, data = trn, family= binomial(link= "logit") )

summary(model2)
predicted2= predict(model2, tst, type="response")
optcutoff2= optimalCutoff(tst$Default_Payment, predicted2)[1]
optcutoff2
misClassError(tst$Default_Payment, predicted2, threshold = optcutoff2)



table(tst$Default_Payment)

plotROC(tst$Default_Payment, predicted2)

sensitivity(tst$Default_Payment, predicted2)
specificity(tst$Default_Payment, predicted2)
confusionMatrix(tst$Default_Payment, predicted2)

