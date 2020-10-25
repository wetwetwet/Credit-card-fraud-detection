# Input Data
app.df <- read.csv("~/Doc/credit card fraud/application_record.csv",header=TRUE)
# ensure there is no NA
table(is.na(app.df)) 

# Inspect the structure of the data
str(app.df) 

# Inspect all the factors' name
table(app.df$NAME_INCOME_TYPE) 
table(app.df$NAME_EDUCATION_TYPE)
table(app.df$NAME_FAMILY_STATUS)
table(app.df$NAME_HOUSING_TYPE)
table(app.df$OCCUPATION_TYPE)

# Remove occupation type due to the Null value " "
app.df <- app.df[,-17]  

# Turn factors into numeric type in order to calculate correlation coefficient
app.df$CODE_GENDER <- factor(app.df$CODE_GENDER, levels = c("M", "F"), labels = c(1, 2))
app.df$CODE_GENDER <- as.numeric(as.character(app.df$CODE_GENDER))
table(app.df$CODE_GENDER) 

app.df$FLAG_OWN_CAR <- factor(app.df$FLAG_OWN_CAR, levels = c("N", "Y"), labels = c(0, 1))
app.df$FLAG_OWN_CAR <- as.numeric(as.character(app.df$FLAG_OWN_CAR))
table(app.df$FLAG_OWN_CAR)  
str(app.df)

table(app.df$NAME_INCOME_TYPE)
app.df$NAME_INCOME_TYPE <- factor(app.df$NAME_INCOME_TYPE, levels = c("Commercial associate", "Pensioner", "State servant", "Student", "Working"), labels = c(1, 2, 3, 4, 5))
app.df$NAME_INCOME_TYPE <- as.numeric(as.character(app.df$NAME_INCOME_TYPE))

table(app.df$NAME_EDUCATION_TYPE)
app.df$NAME_EDUCATION_TYPE <- factor(app.df$NAME_EDUCATION_TYPE, levels = c("Academic degree","Higher education","Incomplete higher","Lower secondary","Secondary / secondary special"), labels=c(1,2,3,4,5))
app.df$NAME_EDUCATION_TYPE <- as.numeric(as.character(app.df$NAME_EDUCATION_TYPE))

table(app.df$NAME_FAMILY_STATUS)
app.df$NAME_FAMILY_STATUS <- factor(app.df$NAME_FAMILY_STATUS, levels=c("Civil marriage","Married","Separated","Single / not married","Widow"), labels=c(1,2,3,4,5))
app.df$NAME_FAMILY_STATUS <- as.numeric(as.character(app.df$NAME_FAMILY_STATUS))

table(app.df$NAME_HOUSING_TYPE)
app.df$NAME_HOUSING_TYPE <- factor(app.df$NAME_HOUSING_TYPE, levels=c("Co-op apartment","House / apartment","Municipal apartment","Office apartment","Rented apartment","With parents"), labels = c(1,2,3,4,5,6))
app.df$NAME_HOUSING_TYPE <- as.numeric(app.df$NAME_HOUSING_TYPE)

app.df$FLAG_MOBIL <- as.numeric(app.df$FLAG_MOBIL)
table(app.df$FLAG_MOBIL)

app.df$FLAG_WORK_PHONE <- as.numeric(app.df$FLAG_WORK_PHONE)
table(app.df$FLAG_WORK_PHONE)

app.df$FLAG_PHONE <- as.numeric(app.df$FLAG_PHONE)
table(app.df$FLAG_PHONE)

app.df$FLAG_EMAIL <- as.numeric(app.df$FLAG_EMAIL)
table(app.df$FLAG_EMAIL)

table(app.df$FLAG_OWN_REALTY)
app.df$FLAG_OWN_REALTY <- factor(app.df$FLAG_OWN_REALTY, levels=c("N","Y"), labels=c(0,1))
app.df$FLAG_OWN_REALTY <- as.numeric(as.character(app.df$FLAG_OWN_REALTY))
table(app.df$FLAG_OWN_REALTY)

app.df$CNT_CHILDREN <- as.numeric(app.df$CNT_CHILDREN)
app.df$DAYS_BIRTH <- as.numeric(app.df$DAYS_BIRTH)
app.df$DAYS_EMPLOYED <- as.numeric(app.df$DAYS_EMPLOYED)
app.df$CNT_FAM_MEMBERS <- as.numeric(app.df$CNT_FAM_MEMBERS)

# Ensure all of the variables are numeric
str(app.df)
summary(app.df)

# Read credit_record file
credit.df <- read.csv("~/Doc/credit card fraud/credit_record.csv",header = TRUE)
#Inspect the structure of the data
s tr(credit.df)
# Ensure no NA
table(is.na(credit.df)) 

# Convert STATUS into numeric type, reclassify STATUS
# c(0,1,2,3,4,5,X,C) convert into c(-1,-2,-3,-4,-5,-6,0,1) respectively. The smaller the numbers are, the lower the credit ratings are.
summary(credit.df$STATUS)
credit.df$MONTHS_BALANCE <- as.numeric(credit.df$MONTHS_BALANCE)
credit.df$STATUS <- as.character(credit.df$STATUS)
credit.df$STATUS_new <- factor(credit.df$STATUS,labels=c(-1,-2,-3,-4,-5,-6,1,0))
credit.df$STATUS_new <- as.numeric(as.character(credit.df$STATUS_new))

# Aggregate the count of month use per customer, name the new dataframe "credit_mon"
credit_mon.df <- aggregate(credit.df[,1],by=list(ID=credit.df$ID),FUN=length)
# Aggregate all the credit ratings per customer, name the new dataframe "credit_sum"
credit_sum.df <- aggregate(credit.df[,4],by=list(ID=credit.df$ID),FUN=sum)
# Change col. name into "USE_MON", representing the counts of months that per user have used
names(credit_mon.df)[names(credit_mon.df)=="x"] <- "USE_MON" 

# combine the two dataframe by "ID"
credit2.df <- merge(credit_mon.df,credit_sum.df,by="ID")

# ensure there is not NA
table(is.na(credit2.df))

# Change col. name into "STATUS_sum"
names(credit2.df)[names(credit2.df)=="x"] <- "STATUS_sum"

# create new variable: average pay status:AVG_PAY
credit2.df$AVG_PAY <- round(credit2.df$STATUS_sum/credit2.df$USE_MON,2)

# combine credit data with application data, by "ID"
credit3.df <- merge(app.df,credit2.df,by="ID")
str(credit3.df)

# convert USE_MON into numeric
credit3.df$USE_MON <- as.numeric(credit3.df$USE_MON)
str(credit3.df)

# remove FLAG_MOBIL due to all the values are 1
credit4.df <- credit3.df[,-13]

# draw the correlation plot to check the relationship between all variables
install.packages('corrplot')

#check correlation coefficient
library(corrplot) 
cor(credit4.df)
corrplot.mixed(corr=cor(credit4.df, use="complete.obs"), 
               upper="ellipse" , tl.pos= "lt", tl.cex=0.5,number.cex = 0.5)

# remove Family Size because of Correlation Coefficient
credit4.df <- credit4.df[,-16]

# remove STATUS_sum because of Correlation Coefficient
credit4.df <- credit4.df[,-17]
corrplot.mixed(corr=cor(credit4.df, use="complete.obs"), 
               upper="ellipse" , tl.pos= "lt", tl.cex=0.5,number.cex = 0.5)
