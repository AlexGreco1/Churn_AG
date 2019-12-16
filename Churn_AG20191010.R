#---
#title: "Churn"
#author: "Alejandro Greco"
#date: "10/10/2019"
#---
  
# 1 - Introduction


#This project has the objective to make a recommendation system to reduce the effort of loyalty actions beeing more efficient by increasing the precision of the actions based on churn data of a telecommunication company provided by kaggle.
#The objective is to obtain the lower possible Root Mean Squared Error (RMSE), lower than 0.40, using the data provided divided into a training set and validation set. The original data content is about 7 thousand registers with 21 variables but after the cleaning process removing the null values is about 6.5 thousand and 2 new variables were added to transform continues values of money to categorical. This data was divided into 2 data set, one with 90% of the data named "churn_data" with about 5.9 thousand observations and other named "validation" with about 0.6 thousand registers both with 23 columns with not null values.

# 2 - Methods

#In this project was evaluated 5 models to make the best recommendation possible with the lower value in loss function Root mean squared error (RMSE):
  
#     1 - Naive: A model using only the average churn; 
#     2 - Contracts: A model using model 1 adding Contracts type of is month to month, one year or two years.  because is impacting the churn;
#     3 - Total Charges: A model using model 2 adding Total Charges because some users could spend extra of the monthly charges and we could guess that a user that spend more on the company's products could reflect their preferences, loyalty and reduce the probability of churn;
#     4 - Monthly Charges: A model using model 3 adding the monthly charges of users. This is a reflection of the users' preferences, loyalty, and the market because is the amount of money the users are willing to pay for the company's services;
#     5 - Tenure: A model using model 4 adding the tenure of the user. I considered this variable super important because could be used as a trigger to take loyalty actions. 

#Using Root mean squared error (RMSE) will would compare the efficiency of the models.

## 2.1 - Data

#The data is from kaggle, please log in with your account and download it with this link:
# https://www.kaggle.com/blastchar/telco-customer-churn/download .
#This file must be unzipped and move it into your working directory that could be found with "getwd()".

################################
# Create churn
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# you must download the file from https://www.kaggle.com/blastchar/telco-customer-churn/download unzipped and move it into the working directory getwd() .

churn <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

## 2.2 - Requirements 

#Installation of these packages are required for this project:

# Packages

if(!require(bazar)) install.packages("bazar", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(rapportools)) install.packages("rapportools", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

## 2.3 - Data cleaning

#In the table below you can observe the head of the data set.

head(churn)

#First, it is suggested to clean the data before creating the training and validation set to save time and because is more efficient. The changes required are:

#   1 - The objective variable is Churn, and it will be transformed into numeric. Yes for 1 and No made churn for 0.
#   2 - All the "no ..." will be transformed to "no" because for this analysis if the person does not have phone service that means that it does not have multiple lines. The same occurred with all the other variables bellow.

#Two columns will be added to categorize the monetary values using Scott's breaks.

#The objective variable is Churn, and it will be transformed into numeric.

churn$Churn <- as.character(churn$Churn)
churn$Churn[churn$Churn == "Yes"] <- 1
churn$Churn[churn$Churn == "No"] <- 0
churn$Churn <- as.numeric(churn$Churn)

# This is to correct some text data but first, we need to change the columns to act as characters.

churn$MultipleLines <- as.character(churn$MultipleLines)
churn$OnlineSecurity <- as.character(churn$OnlineSecurity)
churn$OnlineBackup <- as.character(churn$OnlineBackup)
churn$DeviceProtection <- as.character(churn$DeviceProtection)
churn$TechSupport <- as.character(churn$TechSupport)
churn$StreamingTV <- as.character(churn$StreamingTV)

#All the "no ..." will be transformed into "no" because for this analysis if the person does not have phone service that means that does not have multiple lines. The same occurred with all the other variables.

churn$MultipleLines[churn$MultipleLines == "No phone service"] <- "No"
churn$OnlineSecurity[churn$OnlineSecurity == "No internet service"] <- "No"
churn$OnlineBackup[churn$OnlineBackup == "No internet service"] <- "No"
churn$DeviceProtection[churn$DeviceProtection == "No internet service"] <- "No"
churn$TechSupport[churn$TechSupport == "No internet service"] <- "No"
churn$StreamingTV[churn$StreamingTV == "No internet service"] <- "No"

#There are two columns that will be added based on continues values (monetary) into categorical using Scott break to calculate intervals because they are required for the analysis. 

#To categorized total charges, the breaks used are 19 as you would observe in the next table.
summary(
        hist(churn$TotalCharges, breaks = "Scott", col='Orange', main="Users and Total Charges", xlab = "Total Charges")
        )

#This is to create class size

lower_total<-min(churn$TotalCharges)
upper_total<-max(churn$TotalCharges)

classsize<-(upper_total-lower_total)/19

#This is the sequences using the class size
break_seq<-seq(lower_total, upper_total, classsize)

#Finally this adds a new variable with the category to the data set.

churn$TotalChargesCategory <- cut(churn$TotalCharges, breaks = break_seq , labels=1:19)

#The other variable to classify is Monthly Charges. In this case, we need 22 Scott breaks as you could observe in the table below.

summary(
        hist(churn$MonthlyCharges, breaks = "Scott", col='Orange', main="Users and Monthly Charges", xlab = "Total Monthly")
        )

#This is to create class size

lower_monthly<-min(churn$MonthlyCharges)
upper_monthly<-max(churn$MonthlyCharges)

classsize_monthly <- (upper_monthly-lower_monthly)/22

#This is the sequences using the class size

break_seq_monthly <- seq(lower_monthly, upper_monthly, classsize_monthly)

#Finally this adds a new variable with the category to the data set.

churn$MonthlyChargesCategory <- cut(churn$MonthlyCharges, breaks = break_seq_monthly , labels=1:22)

#This is to remove na or null values.

churn <- churn[complete.cases(churn), ]

# This is to split the data into churn_data and validation set 90% and 10% respectively.

set.seed(1)
test_index <- createDataPartition(y = churn$Churn, times = 1, p = 0.1, list = FALSE)
churn_data <- churn[-test_index,]
validation <- churn[test_index,]

rm( test_index)

## 2.4 - Data exploration and visualization

#This data has 7,043 rows originally and after remove the null values was 6,563 that was divided it in 90% (5,906 registers) for training the model and 10% (657 registers) to validate it. In the training set there are 2,913 female and 2,993 male; partner No 3,059 and Yes 2,847; dependents No 4,132 and Yes 1,774; tenure minimum 1, maximum 72, mean 32.23, and median 29; Contract Month-to-month 3,262 ,  One year 1,250 and Two year 1,394; MonthlyCharges minimum 18.70, median 70.40, mean 64.68 and maximum 118.75; TotalCharges minimum 18.85 Median 1,389.72 Mean 2,270.84 and maximum 8,684.80; and churn with a mean of 26.58%. You can observe this information in the table below.

original_users <- count(churn)
original_users

after_clean_useres <- count(churn_data)
after_clean_useres

users_in_validateion <- count(validation)
users_in_validateion

summary(churn_data)

head(churn_data)
which(is.na(churn_data$TotalCharges))

#In the data, each row represents a customer, each column contains customer’s attributes includes information about:
  
  
#   1 - Customers who left within the last month – the column is called Churn.
#   2 - Services that each customer has signed up for – phone, multiple lines, internet, online security, online backup, device protection, tech support, and streaming TV and movies.
#   3 - Customer account information – how long they have been a customer, contract, payment method, paperless billing, monthly charges, and total charges.
#   4 - Demographic info about customers – gender, age range, and if they have partners and dependents.

#The columns information form the original data:
  
#   1	- customerID	:	Customer ID; 
#   2	- gender	:	Whether the customer is a male or a female; 
#   3	- SeniorCitizen	:	Whether the customer is a senior citizen or not (1, 0); 
#   4	- Partner	:	Whether the customer has a partner or not (Yes, No); 
#   5	- Dependents	:	Whether the customer has dependents or not (Yes, No); 
#   6	- tenure	:	Number of months the customer has stayed with the company; 
#   7	- PhoneService	:	Whether the customer has a phone service or not (Yes, No); 
#   8	- MultipleLines	:	Whether the customer has multiple lines or not (Yes, No, No phone service); 
#   9	- InternetService	:	Customer’s internet service provider (DSL, Fiber optic, No); 
#   10	- OnlineSecurity	:	Whether the customer has online security or not (Yes, No, No internet service); 
#   11	- OnlineBackup	:	Whether the customer has online backup or not (Yes, No, No internet service); 
#   12	- DeviceProtection	:	Whether the customer has device protection or not (Yes, No, No internet service); 
#   13	- TechSupport	:	Whether the customer has tech support or not (Yes, No, No internet service); 
#   14	- StreamingTV	:	Whether the customer has streaming TV or not (Yes, No, No internet service); 
#   15	- StreamingMovies	:	Whether the customer has streaming movies or not (Yes, No, No internet service); 
#   16	- Contract	:	The contract term of the customer (Month-to-month, One year, Two years); 
#   17	- PaperlessBilling	:	Whether the customer has paperless billing or not (Yes, No); 
#   18	- PaymentMethod	:	The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic)); 
#   19	- MonthlyCharges	:	The amount charged to the customer monthly; 
#   20	- TotalCharges	:	The total amount charged to the customer; 
#   21	- Churn	:	Whether the customer churned or not (Yes or No); 

### 2.4.1 - Demographic

#Exploring the data we could observe that some variables have an impact on the churn. For example, looks like gender, senior citizen, partner, dependents have influence in the churn. In this table, reflect that a female senior with no partner and no dependents has an average churn of about 50% vs no senior female with partner and dependents that are about 15%.


churn_gender <- churn_data %>% 
              group_by(gender,SeniorCitizen,Partner,Dependents) %>% 
              summarise(churn=sum(Churn), 
                        avg_churn=mean(Churn),
                        sd_churn=sd(Churn)
              )

churn_gender



### 2.4.2 - Tenure


#In the first month, the average churn is above 61%, but when the tenure is in the tenth month, the average churn is close to 39%. In these graphics illustrated the tendency behavior affected by the tenure. This information is represented in the table below.
#Overall, churn tended to decrease against increase the tenure as you could observe in the graphic below.

# Here is the churn by tenure.
churn_tenure <- churn_data %>% 
                group_by(tenure) %>% 
                summarise(count_customers=n(customerID),
                          churn=sum(Churn), 
                          avg_churn=mean(Churn),
                          sd_churn=sd(Churn),
                          avg_monthly=mean(MonthlyCharges),
                          min_monthly=min(MonthlyCharges),
                          max_monthly=max(MonthlyCharges),
                )

churn_tenure

#Scatter plot exploring tenure and number of users
sp_user_tenure_temp <- qplot(churn_tenure$tenure, 
                             churn_tenure$count_customers,    
                             data = churn_tenure) + 
                        geom_smooth() + 
                        labs(x="Tenure", y="Users")  

sp_user_tenure <- sp_user_tenure_temp + 
                  theme_minimal() +
                  ggtitle("Users distributions in tenure") + 
                  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                        plot.title=element_text(hjust=0.5))

sp_user_tenure

#scatter plot exploring tenure and avg churn
sp_avg_churn_tenure_temp <- qplot(churn_tenure$tenure, 
                                  churn_tenure$avg_churn,    
                                  data = churn_tenure) + 
                            geom_smooth() + 
                            labs(x="Tenure", y="Avg users churn")  

sp_avg_churn_tenure <- sp_avg_churn_tenure_temp + 
                        theme_minimal() +
                        ggtitle("Avg churn in tenure") + 
                        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                              plot.title=element_text(hjust=0.5))

sp_avg_churn_tenure


### 2.4.3 - Charges


#The products and services prices, annually or monthly customer spending have a strong influence in the churn.
#In these graphics and tables bellow, you could observe customers with their tenure and on average how much they spend monthly and annually. These variables affect churn because in almost every tenure interval, the users that spent more, are more susceptible to make churn. In the graphic below illustrated the correlation between spending and churn. 


#This table shows the total charges category and churn
churn_total_charges <- churn_data %>% 
                        group_by(TotalChargesCategory) %>% 
                        summarise(count_customers=n(customerID),
                                  churn=sum(Churn), 
                                  avg_chrun=mean(Churn),
                                  sd_chrun=sd(Churn),
                                  avg_totalCharges=mean(TotalCharges),
                                  min_totalCharges=min(TotalCharges),
                                  max_totalCharges=max(TotalCharges),
                        )

churn_total_charges

#This table shows the tenure and churn 
churn_tenure_charges <- churn_data %>% 
                        group_by(tenure,Churn) %>% 
                        summarise(count_customers=n(customerID),
                                  avg_monthly=mean(MonthlyCharges),
                                  min_monthly=min(MonthlyCharges),
                                  max_monthly=max(MonthlyCharges),
                                  avg_total=mean(TotalCharges),
                                  min_total=min(TotalCharges),
                                  max_total=max(TotalCharges),
                        )

churn_tenure_charges

#In these graphics bellow, you can observe customers with their tenure and on average how much they spend monthly and annually. These variables affect churn because in almost every tenure interval, the users that spent more, are more susceptible to make churn. In the graphic below illustrated the correlation between spending, monthly and total, with the churn.

churn_tenure_Monthlypayment <- ggplot(churn_tenure_charges, 
                                      aes(fill=as.factor(churn_tenure_charges$Churn), 
                                          y=churn_tenure_charges$avg_monthly, 
                                          x=churn_tenure_charges$tenure)) + 
                                geom_bar(position="dodge", stat="identity") +
                                theme_minimal() +
                                ggtitle("Churn in avg payment in tenure") + 
                                labs(x="Tenure", y="Avg monthly payment", fill="Churn") +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                      plot.title=element_text(hjust=0.5))


churn_tenure_Monthlypayment

churn_tenure_TotalPayment <- ggplot(churn_tenure_charges, 
                                    aes(fill=as.factor(churn_tenure_charges$Churn), 
                                        y=churn_tenure_charges$avg_total, 
                                        x=churn_tenure_charges$tenure)) + 
                              geom_bar(position="dodge", stat="identity") +
                              theme_minimal() +
                              ggtitle("Churn in avg payment in tenure") + 
                              labs(x="Tenure", y="Avg total payment", fill="Churn") +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                                    plot.title=element_text(hjust=0.5))

churn_tenure_TotalPayment




### 2.4.4 - Linear regressions

#Using linear regression is possible to obtain the more relevant variables, between the 21 that are in the data, to make the best prediction. The more relevant variables that have a significant impact on the churn are:


lr_churn<-lm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines +
               InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV +
               StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges
             , data=churn_data)
summary(lr_churn)


#There are several variables that have significance into the churn. The top are:
  
#   1 - Contract,
#   2 - TotalCharges,   
#   3 - OnlineSecurity, 
#   4 - TechSupport,      
#   5 - PaperlessBilling,
#   6 - PaymentMethod,
#   7 - PhoneService,    
#   8 - MonthlyCharges,      
#   9 - tenure,              
#   10 - OnlineBackup,     
#   11 - SeniorCitizen

#If we select this top, the interception p-value improves from 5.96e-08 to 3.21e-12. But, this selection could be better with fewer variables. The payment method and senior citizen has no significance compared with the other selected variables.


lr_churn<-lm(Churn ~ Contract+
               TotalCharges + 
               OnlineSecurity + 
               TechSupport + 
               PaperlessBilling + 
               PaymentMethod + 
               PhoneService + 
               MonthlyCharges + 
               tenure + 
               OnlineBackup + 
               SeniorCitizen
             , data=churn_data)
summary(lr_churn)

#Removing payment method and SeniorCitizen, we have the strongest linear regression.

lr_churn<-lm(Churn ~ Contract+
               TotalCharges + 
               OnlineSecurity + 
               TechSupport + 
               PaperlessBilling  + 
               PhoneService + 
               MonthlyCharges + 
               tenure + 
               OnlineBackup 
             , data=churn_data)
summary(lr_churn)

#These are the strongest significant variables in a linear regression:
  
  
#   1 - Contract,
#   2 - TotalCharges, 
#   3 - MonthlyCharges,
#   4 - TechSupport,  
#   5 - OnlineSecurity, 
#   6 - PhoneService,
#   7 - PaperlessBilling,
#   8 - OnlineBackup 

#But, based on the objective of taking loyalty action over the user that could cancel the company services, and avoid overfitting, I selected the variables contract, total charges, monthly charges and tenure to be evaluated them.  I considered these variables as customer-centric to evaluate customer behavior to predict their churn. TechSupport, OnlineSecurity, PhoneService, PaperlessBilling and OnlineBackup are added value to the company's products but are not intrinsic related to uses of the services. Although, it is suggested to be used them as tools for loyalty actions. This linear regression obtained 2.5e-09 for the p-value interception that is a really good one and not overfitted.


lr_churn <- lm(Churn ~ Contract+
                     TotalCharges +  
                     MonthlyCharges + 
                     tenure 
             , data=churn_data)
summary(lr_churn)


## 2.5 - Insights gained

#This is a small data but is very interesting because this approach could be used for any company's services. Considering this data as a picture in a certain time, immersed into an open complex system (CLIOS Process), where some users made churn to another company and others no yet for the moment. It is uncertain if they will keep been customers for 1, 2 or 60 months more because it depends on internal and external factors. This means they are making churn to other companies could be because the technology is constantly evolving as the same as the market growth in the offer and the demands are variating but the only constant is the communication needs will always exist as an intrinsic aspect of human beings and universal rights; or could be for opportunities that better fit their needs based on cost, quality and quantity services; or other factors like the prestige of a mark or because the person just wants a change without any other reason.
#The demographics suggested that some users are more characterize with the company than others but in my opinion, are not actionable. I can not recommend not to sell to senior citizens females with no partner and not dependents because more of the half will make churn. Not only because is illegal but more important, unethical. Everyone has the right to communicate and is a customer so is not possible to be selective.

## 2.6 - Modeling approach

#This study considered 5 different approaches:
  
  
#   1 - The simple average of the rating;
#   2 - The previous model adding contracts effect;
#   3 - The previous model adding total charges effect;
#   4 - The previous model adding monthly charges effect;
#   5 - The previous model adding tenure effect.

#Every step is increasing the number of variables considered, increase the complexity and reduce the error in our predictions of churn. All of them were evaluated using RMSE and the best option was the 5Th as you could observe in the results bellow.

RMSE <- function(true_ratings, predicted_ratings)
      { sqrt(mean((true_ratings - predicted_ratings)^2))}

### 2.6.1 - Method 1, churn average

#First, considering that the average in churn could predict future churn and the recommendation is using just the churn.

mu_hat <- mean(churn_data$Churn)
mu_hat

rmse_1_mu <- RMSE(churn_data$Churn, mu_hat)
rmse_1_mu

#Add the rusult to the result table
methods_rmse_results <- data.frame(model="General churn average only", RMSE=rmse_1_mu)

methods_rmse_results

### 2.6.2 - Method 2, contracts effect

#Secondly, based on the previous method, increase complexity by adding contracts to reduce the error and improve the recommendation using churn and contracts types effect.

mu <- mean(churn_data$Churn)
mu

contract_mu <- churn_data %>%
              group_by(Contract) %>% 
              summarise(b_c = mean(Churn - mu))
contract_mu

predicted_churn_2 <- mu + validation %>% left_join(contract_mu, by='Contract') %>% pull(b_c)

model_2_rmse <- RMSE(predicted_churn_2, validation$Churn)
model_2_rmse

#Add the rusult to the result table
methods_rmse_results <- methods_rmse_results %>% add_row(model="Contract effect", RMSE=model_2_rmse)

methods_rmse_results

### 2.6.3 - Method 3, total charges effect

#Thirdly, based on the previous method, increase complexity by adding total charges to reduce the error and improve the recommendation using churn, contracts types and total charges effect.

totalchargescategory_mu <- churn_data %>% 
                            left_join(contract_mu, by='Contract') %>% 
                            group_by(TotalChargesCategory) %>%
                            summarise(b_t = mean(Churn - mu - b_c))

predicted_Churns_3 <- validation %>% 
                      left_join(contract_mu, by='Contract') %>% 
                      left_join(totalchargescategory_mu, by='TotalChargesCategory') %>% 
                      mutate(pred = mu + b_c + b_t) %>% 
                      pull(pred)


model_3_rmse <- RMSE(predicted_Churns_3, validation$Churn)
model_3_rmse


#Add the rusult to the result table
methods_rmse_results <- methods_rmse_results %>% add_row(model="Total charges category effect", RMSE=model_3_rmse)
methods_rmse_results

### 2.6.4 - Method 4, monthly charges effect

#Fourthly, based on the previous method, increase complexity by adding monthly charges to reduce the error and improve the recommendation using churn, contracts types, total and monthly charges effect.

monthly_mu <- churn_data %>% 
              left_join(contract_mu, by='Contract') %>%
              left_join(totalchargescategory_mu, by='TotalChargesCategory') %>% 
              group_by(MonthlyChargesCategory) %>%
              summarise(b_mo = mean(Churn - mu - b_c - b_t))
monthly_mu

predicted_Churns_4 <- validation %>% 
                      left_join(contract_mu, by='Contract') %>% 
                      left_join(totalchargescategory_mu, by='TotalChargesCategory') %>% 
                      left_join(monthly_mu, by='MonthlyChargesCategory') %>% 
                      mutate(pred = mu + b_c + b_t + b_mo) %>% 
                      pull(pred)

model_4_rmse <- RMSE(predicted_Churns_4, validation$Churn)
model_4_rmse

#Add the rusult to the result table

methods_rmse_results <- methods_rmse_results %>%  add_row(model="Monthly charges category effect", RMSE=model_4_rmse)
methods_rmse_results

### 2.6.5 - Method 5, tenure effect

#Finally, based on the previous method, increase complexity by adding tenure to reduce the error and improve the recommendation using churn, contracts types, total and monthly charges  and tenure effect.

tenure_mu <- churn_data %>% 
            left_join(contract_mu, by='Contract') %>%
            left_join(totalchargescategory_mu, by='TotalChargesCategory') %>% 
            left_join(monthly_mu, by='MonthlyChargesCategory') %>% 
            group_by(tenure) %>%
            summarise(b_te = mean(Churn - mu - b_c - b_t - b_mo))
tenure_mu

predicted_Churns_5 <- validation %>% 
                      left_join(contract_mu, by='Contract') %>% 
                      left_join(totalchargescategory_mu, by='TotalChargesCategory') %>% 
                      left_join(monthly_mu, by='MonthlyChargesCategory') %>% 
                      left_join(tenure_mu, by='tenure') %>% 
                      mutate(pred = mu + b_c + b_t  + b_mo + b_te) %>% 
                      pull(pred)

model_5_rmse <- RMSE(predicted_Churns_5, validation$Churn)
model_5_rmse

#Add the rusult to the result table

methods_rmse_results <- methods_rmse_results %>% add_row(model="Tenure effect", RMSE=model_5_rmse)
methods_rmse_results

# 3 - Results 

#In conclusion, the best model is the 5th with an excellent value of 0.3818975. This adds more complexity to the model giving better predictions but it is no infinity. Notice that every time a new dimension is added to the model improves the RMSE but in this study, after the 5Th the improvement was very low because it is harder to predict better behavior only considering one company of the market without knowing the quality of the services, prices of the products, experience of use of the users, problem resolutions, between others variables in the industry.

methods_rmse_results

# 4 - Conclusion 


## 4.1 - Summary of the report

#In this project, we could evaluate more than 6.5 thousand users from a telco company with the objective to take loyalty actions to retain users that could make churn. We could verify that the customers of this company have a wide variate in gender, ages, partner and dependents. Independent of the demographics of the users we could make a recommendation with an RMSE of 0.3818975 considering the averages of churn, contracts type, total charges, monthly charges and tenure, which is an excellent result based only on commercial results index.

## 4.2 - Limitations 

#This data is very small with only about 7 thousand users with commercial result indicators without knowing the universe of the populations (total clients) and we don't have performance indicators to evaluate the services neither the performance and result of the customer support. In this data there are some characteristics associated with the products like multiple lines or phone services but there are not the characteristic of services, for example, are unlimited in minutes and/or in data? how could these products compete with the market based on price, characteristic and quality? are users from one country? all these questions remain unanswered that could help to improve the model prediction.

## 4.3 - Future work

#This study riched great results but is still under-developed its full potential. The above analysis is also suitable for other services companies but keeping the focus on telecom companies, here we want to extend the discussion and make a few further recommendations. The performance indicators of services, customer support, characteristics of the product and the market competition products not only could improve this prediction of churn to make loyalty actions it also could help to developed better products to sell. If the company has centered on customer satisfaction, the churn could be a great opportunity to know better the reasons for churn with a poll looking to gap to improve commercially and technically the processes and the products offered.