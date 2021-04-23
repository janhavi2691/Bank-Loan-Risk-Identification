# Binary Logistic Regression 
# diab = 90% , non diab =10% 
# y = b0+ b1x1+b2x2+b3x3+....bnxn
# heart = age + type of work + WLB + exr. + BMI 

# y[-inf to +inf ] = b0+ b1x1+b2x2+b3x3+....bnxn [-inf to +inf ] : LM 
# p[0 to 1] != b0+b1x1+b2x2+b3x3+....bnxn [-inf to +inf ] 
# p/1-p[>0 to +inf] !=b0+b1x1+b2x2+b3x3+....bnxn [-inf to +inf ] 
# odd 
# log(p/1-p)[-inf to +inf] = b0+b1x1+b2x2+b3x3+....bnxn [-inf to +inf ] 
# y = p = p/1-p = log(p/1-p) = LOGIT / LOGISTIC / LINK 

#p(head) = 0.5 
# odds in favour = p/1-p = p / q 
# odd against = 1-p / p = q/p 

# odds ratio 
#p(head) = 0.5 
# odd(head)= 0.5/0.5 = 1 [controlled ] 

# loaded coin : p(head)=0.7 , 1-p = 0.3 = 0.7/0.3 = 2.333 
# [experimental] 

# R1/R0 = 2.33/1= 2.33 approx 2 

# the likelihood of getting head in loaded coin is 2 times higher than 
# the likelihood of getting head in normal coin 


# Example 2 
#odd(E)=80/20= 4 
#odd(C)=55/45=1.2 
# odds ratio = 4/1.2 = 3.3 
# likelihood of people coming for voting increases thrice in exp group
# than from t

# than from the control group ; campign is sucessfull 
#Example 3
#odd(E)= 30/70= 0.42 
# odd(C)= 45/55 = 0.81 
#odds ratio = 0.42 /0.80 = 0.5 
# chances of getting ill due to epidemic reduces by half in the region 
# where the vaccination was performed .

 
#load data Bank loan 

bank_loan_data <- read.csv("BANK LOAN.csv",stringsAsFactors = T)
View(bank_loan_data)
dim(bank_loan_data)
#remove unwanted columns
bank_loan_data$SN <- NULL 

#check for factor variable but stored as numbers
str(bank_loan_data) # AGE and DEFAULTER
bank_loan_data$AGE <- as.factor(bank_loan_data$AGE)
bank_loan_data$DEFAULTER <- as.factor(bank_loan_data$DEFAULTER)
str(bank_loan_data)

#We dont need to check for outliers here. directly go for building logistic model
#glm = generalized linear model
#dependent/y var is DEFAULTER, rest all are indepenedent var
bank_model <- glm(DEFAULTER~.,data = bank_loan_data,family = "binomial")
bank_model
summary(bank_model)

#   Null deviance: 804.36  on 699 (no. of observations-1 =700 -1) degrees of freedom : model without using any indep. vars
# Residual deviance: 553.41  on 692 (no. of observations-1 - num of indep vars = 700 -1 -7) degrees of freedom  : model  using  indep. vars
# Ideally for a good model Residual deviance should always be less than Null deviance


#Use significant var (pvalue < 0.05) and remove insignificant vars
#and revise the model
bank_model_rev <- glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,data = bank_loan_data,family = "binomial")
summary(bank_model_rev)

#get the prob of defaulter as it is the classification deciding if the person is defaulter or not

prob_defaulter <- predict(bank_model_rev,bank_loan_data,type = "response") 
# type = "response" used for logistic regression getting probabilities(0-1) p = inverse logit= 1/1+e^-alpha
head(prob_defaulter) #probabilities

#To predict whether a person is defaulter or not, we need to keep some threshold
# Generally threshold = 0.5
# If prob > 0.5 then call observation as defaulter (say class = 1)
# IF prob < 0.5 then call observation as not defaulter (say class = 0)
pred_defaulter <- ifelse(prob_defaulter > 0.5 , '1' ,'0')
#add preditions to our dataset
bank_loan_data$pred_defaulter <- pred_defaulter
head(bank_loan_data)



