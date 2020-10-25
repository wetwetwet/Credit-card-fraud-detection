app.df <- read.csv("~/Doc/credit card fraud/application_record.csv",header=TRUE)
table(is.na(app.df[,])) #ensure there is no NA


str(app.df) #檢視變數綱要
table(app.df$NAME_INCOME_TYPE) #檢視類別變數內容物
table(app.df$NAME_EDUCATION_TYPE)
table(app.df$NAME_FAMILY_STATUS)
table(app.df$NAME_HOUSING_TYPE)
table(app.df$OCCUPATION_TYPE)
app.df <- app.df[,-17]  #移除occupation type

# turn factors into numeric
app.df$CODE_GENDER <- factor(app.df$CODE_GENDER, levels = c("M","F"), labels = c(1,2))
app.df$CODE_GENDER <- as.numeric(as.character(app.df$CODE_GENDER))
table(app.df$CODE_GENDER) #M:1, F:2

app.df$FLAG_OWN_CAR <- factor(app.df$FLAG_OWN_CAR, levels = c("N","Y"), labels = c(0,1))
app.df$FLAG_OWN_CAR <- as.numeric(as.character(app.df$FLAG_OWN_CAR))
table(app.df$FLAG_OWN_CAR)  #N:0, Y:1
str(app.df)

table(app.df$NAME_INCOME_TYPE)
app.df$NAME_INCOME_TYPE <- factor(app.df$NAME_INCOME_TYPE, levels = c("Commercial associate","Pensioner","State servant","Student","Working"), labels = c(1,2,3,4,5))
app.df$NAME_INCOME_TYPE <- as.numeric(as.character(app.df$NAME_INCOME_TYPE))
table(app.df$NAME_INCOME_TYPE)

table(app.df$NAME_EDUCATION_TYPE)
app.df$NAME_EDUCATION_TYPE <- factor(app.df$NAME_EDUCATION_TYPE, levels = c("Academic degree","Higher education","Incomplete higher","Lower secondary","Secondary / secondary special"), labels=c(1,2,3,4,5))
app.df$NAME_EDUCATION_TYPE <- as.numeric(as.character(app.df$NAME_EDUCATION_TYPE))
table(app.df$NAME_EDUCATION_TYPE)

table(app.df$NAME_FAMILY_STATUS)
app.df$NAME_FAMILY_STATUS <- factor(app.df$NAME_FAMILY_STATUS, levels=c("Civil marriage","Married","Separated","Single / not married","Widow"), labels=c(1,2,3,4,5))
app.df$NAME_FAMILY_STATUS <- as.numeric(as.character(app.df$NAME_FAMILY_STATUS))
table(app.df$NAME_FAMILY_STATUS)

table(app.df$NAME_HOUSING_TYPE)
app.df$NAME_HOUSING_TYPE <- factor(app.df$NAME_HOUSING_TYPE, levels=c("Co-op apartment","House / apartment","Municipal apartment","Office apartment","Rented apartment","With parents"), labels = c(1,2,3,4,5,6))
app.df$NAME_HOUSING_TYPE <- as.numeric(app.df$NAME_HOUSING_TYPE)
table(app.df$NAME_HOUSING_TYPE)

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

#ensure all of the variables are numeric
str(app.df)
summary(app.df)

credit.df <- read.csv("~/Doc/credit card fraud/credit_record.csv",header = TRUE)
str(credit.df)
table(is.na(credit.df)) #ensure no NA

# convert STATUS into numeric type, reclassify STATUS
summary(credit.df$STATUS)
credit.df$MONTHS_BALANCE <- as.numeric(credit.df$MONTHS_BALANCE)
credit.df$STATUS <- as.character(credit.df$STATUS)
credit.df$STATUS_new <- factor(credit.df$STATUS,labels=c(-1,-2,-3,-4,-5,-6,1,0))
credit.df$STATUS_new <- as.numeric(as.character(credit.df$STATUS_new))

credit_mon.df <- aggregate(credit.df[,1],by=list(ID=credit.df$ID),FUN=length)
credit_sum.df <- aggregate(credit.df[,4],by=list(ID=credit.df$ID),FUN=sum)
names(credit_mon.df)[names(credit_mon.df)=="x"] <- "USE_MON" # change col. name

# combine two dataframe
credit2.df <- merge(credit_mon.df,credit_sum.df,by="ID")
table(is.na(credit2.df))
names(credit2.df)[names(credit2.df)=="x"] <- "STATUS_sum"
# create new variable: average pay status:AVG_PAY
credit2.df$AVG_PAY <- round(credit2.df$STATUS_sum/credit2.df$USE_MON,2)

# combine credit data with application data
credit3.df <- merge(app.df,credit2.df,by="ID")
str(credit3.df)

# convert USE_MON into numeric
credit3.df$USE_MON <- as.numeric(credit3.df$USE_MON)
str(credit3.df)

#remove FLAG_MOBIL(因為都是1)
credit4.df <- credit3.df[,-13]

cor(credit4.df)
quartz()  #graph 另開視窗

install.packages('corrplot')
library(corrplot) #check correlation coefficient
cor(credit4.df)
corrplot.mixed(corr=cor(credit4.df, use="complete.obs"), 
               upper="ellipse" , tl.pos= "lt", tl.cex=0.5,number.cex = 0.5)

# remove Family Size because of Correlation Coefficient
credit4.df <- credit4.df[,-16]
# remove STATUS_sum because of Correlation Coefficient
credit4.df <- credit4.df[,-17]
corrplot.mixed(corr=cor(credit4.df, use="complete.obs"), 
               upper="ellipse" , tl.pos= "lt", tl.cex=0.5,number.cex = 0.5)
