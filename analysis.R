df <- read.csv('/home/muhammad/AdvStats/Detailed.csv')

#Alpha value is 0.05 for all Questions

#Question1

#Null hypothesis = purchase amount of the customers increases over the period of time.
#Alternative hypothesis = purchase amount of the customers decreases over the period of time.

#Week3 Sales are used as a reference point and compared with Week1 Sales if they are the same then there was no increase
#in purchase amount but if Week1 has greater sales than Week3 then there was an increase in purchase amount.

t.test(df$W1_Total_Sales, df$W3_Total_Sales-df$W2_Total_Sales,alternative = 'less')

#As the p-value shows 1 and the mean of x is greater than y that means the purchase amount increases overtime.
#Fail to reject Null hypothesis

#Question2

#Null hypothesis = rich people are prone to churn as compared to poor customers.
#Alternative hypothesis = rich people are not prone to churn as compared to poor customers.

ordereddf <- df[order(df$Total_Revenue,decreasing = TRUE),] #Ordering values based on highest Revenue.

richdf <- ordereddf[1:66666,]
moderatedf <- ordereddf[66667:133333,]
poordf <- ordereddf[133334:200000,]                     #Splitting df into 3 equal parts to represent rich,moderate and poor

Explabel <- c(.50, .50)

label1_tab <- table(richdf$Label)
label2_tab <- table(poordf$Label)

chisq.test(label1_tab)
chisq.test(label2_tab)

#As X squared value is greater for poor customers then null hypothesis is rejected.

#Question3

#Null hypothesis = churned customers visit the stores occasionally.
#Alternative hypothesis = churned customers don't visit the stores occasionally.

df$Label <-ifelse(df$Label =="Churned",1,0) #Converts Label column in df to 1 or 0, 1 represents Churned

churnedcus <- df[df$Label == 1,] #All Churned Customers
notchurnedcus <- df[df$Label == 0,] #All Not Churned Customers

t.test(churnedcus$Total_Visit_Days, notchurnedcus$Total_Visit_Days,alternative = 'less')

#As difference in mean value is not significant
#Fail to accept Null hypothesis.

#Question4

#Null hypothesis = Overall spending of churned customers is significantly lesser than the non-churned customers.
#Alternative hypothesis = Overall spending of churned customers is significantly greater than the non-churned customers.

t.test(churnedcus$Total_Revenue, notchurnedcus$Total_Revenue,alternative = 'greater')

#Overall spending of not churned customers is greater.
#Reject Null hypothesis.

#Question5

train_rows <- sample(1:nrow(df), 0.9*nrow(df))  #Creates row index of Training Set (90% of Total Data)

train_data <- df[train_rows, ] #Creates Training Dataset
test_data  <- df[-train_rows, ] #Creates Testing Dataset

lmodel <- lm(Label ~ Days_Since_Last_Visit, data=train_data) #Creates Linear Model trained by train_data
labelpred <- predict(lmodel, test_data) #Creates Predicted Labels from test_data

actuals_preds <- data.frame(cbind(actuals=test_data$Label, predicteds=labelpred)) #Combines Predicted Lables and Actual Lables

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))*100 #Calculates Accuracy of Linear Model
