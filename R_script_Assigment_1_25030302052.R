#DATA UNDERSTANDING & CLEANING
# Load required libraries
library(dplyr)

# Import dataset (change path to your file)
Loan_Default = read.csv("C:/Users/sumir/Downloads/Loan_Default.csv", header = TRUE,stringsAsFactors = FALSE)
Loan_Default
# View first few rows
head(Loan_Default)

# Dimensions
dim(Loan_Default)

# Column names
names(Loan_Default)

# Structure
str(Loan_Default)

# Summary
summary(Loan_Default)


#A.numerical variable to median

# checking missing values after clening 
colSums(is.na(Loan_Default))

Loan_Default$rate_of_interest[is.na(Loan_Default$rate_of_interest)] = 
  median(Loan_Default$rate_of_interest, na.rm = TRUE)

Loan_Default$income[is.na(Loan_Default$income)] = 
  median(Loan_Default$income, na.rm = TRUE)

Loan_Default$property_value[is.na(Loan_Default$property_value)] =
  median(Loan_Default$property_value, na.rm = TRUE)
summary(Loan_Default$income)
summary(Loan_Default$rate_of_interest)
summary(Loan_Default$property_value)
#checking missing values after clening 
colSums(is.na(Loan_Default))

#B.Categorical varibale to Mode
# checking missing values before clening 
colSums(is.na(Loan_Default))
# Create Mode Function
mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Apply Cleaning
Loan_Default$Gender[is.na(Loan_Default$Gender)] =
  mode_func(Loan_Default$Gender)

Loan_Default$loan_limit[is.na(Loan_Default$loan_limit)] = 
  mode_func(Loan_Default$loan_limit)

Loan_Default$Region[is.na(Loan_Default$Region)] =
  mode_func(Loan_Default$Region)
#checking missing values after clening 
colSums(is.na(Loan_Default))
table(Loan_Default$Gender)
table(Loan_Default$loan_limit)

# FEATURE ENGINEERING Creating Variable
library(dplyr)

Loan_Default = Loan_Default %>%
  mutate(
    loan_income_ratio = loan_amount / income,
    EMI = loan_amount / term,
    Risk_Category = ifelse(Credit_Score > 750, "Low Risk",
                           ifelse(Credit_Score > 600, "Medium Risk", "High Risk")),
    
    High_Loan = ifelse(loan_amount > median(loan_amount, na.rm = TRUE), 1, 0)
  )
head(Loan_Default[, c("loan_amount", "income", "loan_income_ratio", "EMI", "Risk_Category")])
table(Loan_Default$Risk_Category)

#OUTLIER TREATMENT
#Before Treatment
summary(Loan_Default$loan_amount)
summary(Loan_Default$income)
#Function
cap_outliers = function(x) {
  qnt = quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  caps = quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
  
  x[x < qnt[1]] = caps[1]
  x[x > qnt[2]] = caps[2]
  
  return(x)
}

Loan_Default$loan_amount = cap_outliers(Loan_Default$loan_amount)
Loan_Default$income = cap_outliers(Loan_Default$income)
Loan_Default$property_value = cap_outliers(Loan_Default$property_value)

#After treatment
summary(Loan_Default$loan_amount)
summary(Loan_Default$income)

# Checking the datA
str(Loan_Default)
summary(Loan_Default)
colSums(is.na(Loan_Default))

#Data Cleaning & Preparation
#Missing values in numerical variables such as income, property value, and interest rate were handled using median imputation
#Categorical variables such as Gender, loan_limit, and Region were cleaned using mode imputation
#Feature engineering was performed to create:
#Loan-to-Income Ratio
#EMI
#Risk Category
#Outliers were treated using 5th–95th percentile capping method
#Post-cleaning validation confirmed no missing values remain

##EXPLORATORY DATA ANALYSIS (EDA)
library(ggplot2)
library(dplyr)
#DISTRIBUTION OF LOAN AMOUNT
ggplot(Loan_Default, aes(x = loan_amount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Loan Amount") +
  xlab("Loan Amount") +
  ylab("Frequency")
#INTERPRETATION
#Most loans are concentrated in lower ranges, indicating that the majority of 
#borrowers take moderate loan amounts, while a small proportion takes high-value loans.

#LOAN AMOUNT BY RISK CATEGORY
ggplot(Loan_Default, aes(x = Risk_Category, y = loan_amount, fill = Risk_Category)) +
  geom_boxplot() +
  ggtitle("Loan Amount by Risk Category")
#INTERPRETATION
#High-risk customers tend to have more variability in loan amounts,
#indicating inconsistent borrowing patterns.

#REGION-WISE LOAN DISTRIBUTION
Loan_Default %>%
  group_by(Region) %>%
  summarise(avg_loan = mean(loan_amount, na.rm = TRUE)) %>%
  arrange(desc(avg_loan))
ggplot(Loan_Default, aes(x = Region, y = loan_amount, fill = Region)) +
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("Average Loan Amount by Region")
#INTERPRETATION
#Northern region shows the highest average loan amount, 
#indicating stronger borrowing capacity or higher property values.

#CREDIT SCORE VS LOAN AMOUNT
ggplot(Loan_Default, aes(x = Credit_Score, y = loan_amount)) +
  geom_jitter(alpha = 0.3, width = 5, height = 5000) +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Credit Score vs Loan Amount (Improved)")
#INTERPRETATION
#The scatter plot shows a weak positive relationship between credit score and 
#loan amount. Jittering was applied to reduce overplotting and improve visualization clarity.

#APPROVAL STATUS DISTRIBUTION
table(Loan_Default$Status)
ggplot(Loan_Default, aes(x = as.factor(Status), fill = as.factor(Status))) +
  geom_bar() +
  ggtitle("Loan Approval Status") +
  xlab("Status (0 = Rejected, 1 = Approved)")
#A higher number of rejected loans indicates strict lending policies or high-risk applicants.

#LOAN-INCOME RELATIONSHIP
ggplot(Loan_Default, aes(x = income, y = loan_amount)) +
  geom_point(alpha = 0.3) +
  ggtitle("Income vs Loan Amount")
#Higher income individuals tend to take larger loans, confirming income as a key lending factor.

##KPI Summery
Loan_Default %>%
  summarise(
    Avg_Loan = mean(loan_amount),
    Avg_Income = mean(income),
    Avg_Credit_Score = mean(Credit_Score),
    Approval_Rate = mean(Status)
  )
#The dataset shows a moderate approval rate, with average borrowers having mid-level income and credit scores.

#EDA REPORT
#Exploratory Data Analysis
#Loan distribution shows a right-skewed pattern, indicating most borrowers take moderate loans
#Credit score positively influences loan amount
#Regional analysis shows North region has highest average loans
#Loan approval rate suggests moderate lending strictness
#Income and loan amount show a positive relationship, confirming financial capacity as a key factor

#***WHAT-IF ANALYSIS (SCENARIO BASED)

#SCENARIO 1: INCOME INCREASE BY 10%
#creating senario
Loan_Default$income_new = Loan_Default$income * 1.10

Loan_Default$loan_income_ratio_new <- 
  Loan_Default$loan_amount / Loan_Default$income_new
head(Loan_Default[, c("income", "income_new", 
                      "loan_income_ratio", "loan_income_ratio_new")])

#Comparing the brfore vs after
# we had to make 0 to na and recalulate the ratio and recalculate the senario
#because the old ratio was infinite
sum(Loan_Default$income == 0, na.rm = TRUE)
Loan_Default$income[Loan_Default$income == 0] <- NA
Loan_Default$loan_income_ratio <- 
  Loan_Default$loan_amount / Loan_Default$income
Loan_Default$income_new <- Loan_Default$income * 1.10

Loan_Default$loan_income_ratio_new <- 
  Loan_Default$loan_amount / Loan_Default$income_new
data.frame(
  Metric = c("Old Ratio", "New Ratio"),
  Value = c(
    mean(Loan_Default$loan_income_ratio, na.rm = TRUE),
    mean(Loan_Default$loan_income_ratio_new, na.rm = TRUE)
  )
)
#The loan-to-income ratio decreased from 60.58 to 55.07 after a 10% increase in
#income. This indicates that borrowers become more financially stable, reducing their repayment burden and lowering the probability of default.

#SCENARIO 2: INTEREST RATE INCREASE BY 1%
Loan_Default$rate_new=Loan_Default$rate_of_interest+1
Loan_Default$EMI_new = Loan_Default$loan_amount / Loan_Default$term * 
  (1 + Loan_Default$rate_new/100)
head(Loan_Default[, c("rate_of_interest", "rate_new", "EMI", "EMI_new")])
#comparing
data.frame(
  Metric = c("Old EMI", "New EMI"),
  Value = c(
    mean(Loan_Default$EMI, na.rm = TRUE),
    mean(Loan_Default$EMI_new, na.rm = TRUE)
  )
)
#The increase in interest rate resulted in a rise in EMI from 1011.82 to 1094.82.
#This indicates that borrowers face a higher repayment burden when interest rates increase.

#Scenario 3 CREDIT SCORE IMPROVES
Loan_Default$Credit_Score_new = Loan_Default$Credit_Score + 50

Loan_Default$Risk_Category_new = ifelse(Loan_Default$Credit_Score_new > 750, "Low Risk",
                                         ifelse(Loan_Default$Credit_Score_new > 600, 
                                                "Medium Risk", "High Risk"))
head(Loan_Default[, c("Credit_Score", "Credit_Score_new", 
                      "Risk_Category", "Risk_Category_new")])
#comparing
table(Old = Loan_Default$Risk_Category,
      New = Loan_Default$Risk_Category_new)
#The improvement in credit score resulted in a significant shift of borrowers from 
#higher risk categories to lower ones. A large number of high-risk borrowers moved
#to medium risk, and many medium-risk borrowers transitioned to low risk. 
#This indicates that credit score is a critical determinant of borrower risk classification.

#Visulation
ggplot(Loan_Default, aes(x = loan_income_ratio_new)) +
  geom_histogram(fill = "green", bins = 30) +
  ggtitle("Loan Income Ratio After Income Increase")
)

##Report of What-if-Analysis (3 Senarios)
#Scenario 1 A 10% increase in income reduces the loan-to-income ratio, improving borrower financial stability and lowering default risk.
#Scenario 2 showed that interest rate increase raises EMI burden
#Scenario 3 Improving credit scores shifts borrowers to lower risk categories, enhancing loan quality and reducing overall portfolio risk.


#**SO-WHAT ANALYSIS
#*TOP 3 INSIGHTS
#*INSIGHT 1: INCOME IS A KEY DRIVER OF LOAN RISK
#*Evidence:
#*Loan-income ratio reduced from 60.58 ??? 55.07 after income increase
#*So What?
#*Income plays a critical role in determining a borrower’s repayment capacity. Higher income significantly reduces financial stress and improves loan sustainability.
#**Risk / Opportunity
#*Risk: Low-income borrowers are more likely to default
#*Opportunity: Banks can target high-income segments for safer lending

#**INSIGHT 2: INTEREST RATE DIRECTLY IMPACTS REPAYMENT BURDEN
#*Evidence:
#*EMI increased from 1011.82 ??? 1094.82 (~8.2% increase)
#*So What?
#*Interest rate changes have a direct and immediate impact on borrower affordability, making loans more expensive and harder to repay.
#*Risk / Opportunity
#*Risk: Higher interest rates may increase defaults
#*Opportunity: Flexible interest schemes can improve customer retention

#**INSIGHT 3: CREDIT SCORE STRONGLY DETERMINES RISK PROFILE
#*Evidence:
#*Large shift from:
#*High ??? Medium Risk
#*Medium ??? Low Risk
#*So What?
#*Credit score is one of the strongest indicators of borrower risk, and improving it significantly enhances loan quality.
#*Risk / Opportunity
#*Risk: Poor credit score borrowers increase portfolio risk
#*Opportunity: Credit score improvement programs can reduce defaults

#**WHICH VARIABLES HAVE STRONGEST IMPACT?
#*The most impactful variables in this analysis are income, interest rate, 
#*and credit score, as they directly influence repayment capacity, affordability, 
#*and borrower risk classification.

#**OVERALL BUSINESS MEANING
#*The analysis highlights that borrower financial strength (income), 
#*loan cost (interest rate), and creditworthiness (credit score) are the key
#* drivers of loan performance and risk management.

#**FINAL REPORT ON So-What Analysis
#*The analysis reveals three key insights. First, income significantly affects loan sustainability, as higher income
#*reduces the loan burden and default risk. Second, interest rates directly impact EMI, making loans less
#*affordable when rates increase. Third, credit score plays a crucial role in determining borrower risk, with
#*improvements leading to better risk classification.
#*These insights highlight that income, interest rate, and credit score are the most critical variables influencing
#*lending decisions. From a business perspective, these factors present both risks and opportunities. Low-
#*income borrowers and high interest rates increase default risk, while improving credit scores and 
#*targeting financially stable customers can enhance portfolio quality.


#**VISUALIZATION STORYTELLING 
#*This section is about:
#*Showing key insights visually
#*Making it look like a mini dashboard
#*Explaining which graph matters most

#install.packages("patchwork")   # run once
library(ggplot2)
library(dplyr)
library(patchwork)

#1.Loan Distribution KPI
p1 = ggplot(Loan_Default, aes(x = loan_amount)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  ggtitle("Distribution of Loan Amount")
#**What it shows: it show the distribution of loan amounts across borrowers.
#*Insight:The distribution is right-skewed, indicating that most borrowers 
#*take smaller to moderate loans, while only a few take very large loans.
#*Business Meaning:The bank primarily serves moderate loan customers, suggesting 
#*a focus on middle-income segments rather than high-value lending.
#*OUTPUT:Histogram, Right-skewed
#*WHY IMPORTANT: Shows overall loan behavior and concentration

#2.APPROVAL RATE
p2= ggplot(Loan_Default, aes(x = as.factor(Status), fill = as.factor(Status))) +
  geom_bar() +
  ggtitle("Loan Approval Status") +
  xlab("0 = Rejected, 1 = Approved")
#**What it shows:it shows the number of approved vs rejected loan applications.
#*Insight:A significant portion of applications are rejected, indicating strict lending criteria.
#*Business Meaning: The bank follows a risk-averse strategy, which helps reduce defaults but 
#*may limit customer acquisition.
#*OUTPUT:Bar chart (Approved vs Rejected)
#*WHY IMPORTANT:Shows lending strictness and approval trends

#.3REGION-WISE LOAN ANALYSIS
p3 =ggplot(Loan_Default, aes(x = Region, y = loan_amount, fill = Region)) +
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("Average Loan Amount by Region")
#**What it shows: Average loan amount across different regions.
#*Insight:Certain regions (e.g., North) have higher average loan values.
#*Business Meaning:These regions represent high-value markets and should be
#* prioritized for expansion and targeted marketing.
#* OUTPUT:Bar chart of average loans per region
#* WHY IMPORTANT:Identifies high-value markets

#4.RISK DISTRIBUTION
p4 = ggplot(Loan_Default, aes(x = Risk_Category, fill = Risk_Category)) +
  geom_bar() +
  ggtitle("Risk Category Distribution")
#**Risk Category Distribution
#*What it shows: It shows Distribution of borrowers across Low, Medium, and High risk.
#*Insight: A large proportion of borrowers fall into medium and low-risk categories.
#*Business Meaning: The portfolio is relatively stable, but attention should be given
#* to high-risk segments to reduce potential defaults.
#* OUTPUT: Bar chart (Low / Medium / High risk)
#* WHY IMPORTANT:Shows portfolio risk composition

#5.CREDIT SCORE VS LOAN
p5 = ggplot(Loan_Default, aes(x = Credit_Score, y = loan_amount)) +
  geom_jitter(alpha = 0.3, width = 5, height = 5000) +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Credit Score vs Loan Amount")
#**What it shows:Relationship between credit score and loan amount.
#*Insight:A positive relationship exists, where higher credit score customers tend to receive larger loans.
#*Business Meaning:Credit score is a critical factor in lending decisions and can be used to optimize risk-based loan allocation.
#*OUTPUT: Scatter + trend line
#*WHY IMPORTANT:Shows relationship between creditworthiness and lending

#Creating a DashBoard comprising of all the 5 plots
(p1 | p2) /
  (p3 | p4) /
  p5


#KPI SUMMARY
Loan_Default %>%
  summarise(
    Avg_Loan = mean(loan_amount, na.rm = TRUE),
    Avg_Income = mean(income, na.rm = TRUE),
    Avg_Credit_Score = mean(Credit_Score, na.rm = TRUE),
    Approval_Rate = mean(Status, na.rm = TRUE)
  )

#**Final Report
#*Visualization Storytelling
#*The visualizations provide a comprehensive view of loan distribution, approval
#*trends, and borrower risk.The loan distribution chart highlights that most loans
#*are concentrated in lower ranges, while the approval chart shows the balance between
#*accepted and rejected applications.Regional analysis identifies key markets contributing
#*to higher loan values. The risk distribution chart provides insight into the overall
#*portfolio composition, while the scatter plot between credit score and loan
#* amount highlights the relationship between creditworthiness and lending decisions.

#**MOST IMPACTFUL VISUAL 
#*The most impactful visualization is the credit score vs loan amount plot, as it 
#*clearly demonstrates the relationship between borrower creditworthiness and lending 
#*decisions, helping managers assess risk more effectively.

#BUSINESS RECOMMENDATIONS

#**RECOMMENDATION 1: TARGET HIGH-INCOME BORROWERS
#*Insight:Loan-to-income ratio decreases significantly when income increases (60.58 ??? 55.07), indicating lower financial stress.
#*Action:The bank should prioritize lending to higher-income segments and introduce income-based eligibility criteria.
#*Expected Outcome:Lower default rates and improved loan portfolio stability.

#**RECOMMENDATION 2: INTRODUCE FLEXIBLE INTEREST RATE STRATEGIES
#*Insight: An increase in interest rates leads to a rise in EMI (1011.82 ??? 1094.82), increasing repayment burden.
#*Action:Offer flexible interest rate options such as floating rates or customized repayment plans.
#*Expected Outcome:Improved affordability, higher customer retention, and reduced risk of loan defaults.

#**RECOMMENDATION 3: IMPLEMENT CREDIT SCORE IMPROVEMENT PROGRAMS
#*Insight:Improvement in credit scores shifts borrowers from high and medium risk to lower risk categories.
#*Action:Provide financial literacy programs and credit score improvement tools for customers.
#*Expected Outcome:Better credit quality, increased loan approvals, and reduced overall portfolio risk.

#**FINAL REPORT
#*Business Recommendations:
#*Based on the analysis, three key recommendations are proposed. First, the bank should
#*focus on high-income borrowers, as they demonstrate lower financial risk and better 
#*repayment capacity. Second, flexible interest rate strategies should be implemented to 
#*manage the impact of rising interest rates on borrower affordability. Third, credit score 
#*improvement initiatives should be introduced to shift borrowers into lower risk categories 
#*and enhance overall loan portfolio quality. These actions will help improve profitability,
#*reduce risk, and support sustainable lending practices.