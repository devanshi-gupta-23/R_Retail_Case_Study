###################################################################
#################### Retail Case Study ############################
###################################################################


################# Setting the working directory ###################

setwd('C:/Users/user/Videos/Data Science 360 course/R/case study R/R - Retail Case study/R case study 1 (Retail)')


################# Importing the data ###############################


df_customer=read.csv('Customer.csv')     
df_prod=read.csv('prod_cat_info.csv')
df_transaction=read.csv('Transactions.csv')


View(df_customer)
View(df_prod)
View(df_transaction)

########### loading the .RData file #############

load('.Rdata')

########### loading the library #############
library(dplyr)
library(ggplot2)

################# (1) ####################

df=merge(x=df_transaction,y=df_customer,
         by.x='cust_id',
         by.y ='customer_Id',all = F)
Customer_Final=merge(x=df,y=df_prod,by.x = c('prod_cat_code','prod_subcat_code'),
          by.y = c('prod_cat_code','prod_sub_cat_code'),
          all = F
          )    # merging the data
View(Customer_Final)

##########################################
############ Data Prep ###################
##########################################

Customer_Final$Rate <- as.numeric(Customer_Final$Rate)
Customer_Final$Tax <-as.numeric(Customer_Final$Tax)
Customer_Final$total_amt <-as.numeric(Customer_Final$total_amt)
Customer_Final$DOB <-lubridate::dmy(Customer_Final$DOB)
Customer_Final$tran_date <-lubridate::dmy(Customer_Final$tran_date)

################# (2) ####################

# a) 
colnames(Customer_Final)  # it gives all the column names.
sapply(Customer_Final,FUN = class) # it gives the data type of all the columns.

# b)
View(   head(Customer_Final,10)  )  # it shows the top 10 rows
View(   tail(Customer_Final,10)  )  # it shows the bottom 10 rows.

# c)

### Method 1

df_summary <- fivenum(Customer_Final$total_amt,na.rm= T)
View(as.data.frame(df_summary)) 
df_summary

### Method 2

########## Created a function for five number summary ####################

five.num.summary <- function(x){
  min1=min(x,na.rm=T)
  Q1=quantile(x,prob=0.25,na.rm=T)
  median1=median(x,na.rm=T)
  Q3=quantile(x,prob=0.75,na.rm=T)
  max1=max(x,na.rm=T)
  result=c('Minimum'=min1,'Q1'=Q1,'Median'=median1,'Q3'=Q3,'Maximum'=max1)
  return (result)
}

five.num.summary(Customer_Final$total_amt)
View(as.data.frame(five.num.summary(Customer_Final$total_amt)))

# d)

View(table(Customer_Final$prod_cat))  # frequency table for prod_cat
View(table(Customer_Final$prod_subcat)) # frequency table for prod_subcat
View(table(Customer_Final$prod_cat_code)) # frequency table for prod_cat_code
View(table(Customer_Final$prod_subcat_code)) # frequency table for prod_subcat_code
View(table(Customer_Final$Gender))

################# (3) ####################

## Histogram
ggplot2::ggplot(data= Customer_Final) +                                
  aes(x = Rate) +                             
  geom_histogram(binwidth = 100, fill='pink', color='black') + theme_dark()

ggplot2::ggplot(data= Customer_Final) +                                
  aes(x = total_amt) +                             
  geom_histogram(binwidth = 1000, fill='skyblue', color='black')

ggplot2::ggplot(data= Customer_Final) +                                
  aes(x = Qty) +                             
  geom_histogram(binwidth = 1, fill='skyblue', color='black')

## Frequency bars

ggplot2::ggplot(data = Customer_Final) + aes(Store_type) + geom_bar(fill= 'skyblue', color = 'black')
ggplot2::ggplot(data = Customer_Final) + aes(prod_cat) + geom_bar(fill= 'skyblue', color = 'black')
ggplot2::ggplot(data = Customer_Final) + aes(prod_subcat) + geom_bar(fill= 'skyblue', color = 'black') + coord_flip()
ggplot2::ggplot(data = Customer_Final) + aes(Gender) + geom_bar(fill= 'skyblue', color = 'black') 

################# (4) ####################

#a) 
time.period <- as.numeric(max(Customer_Final$tran_date,na.rm=T) - min(Customer_Final$tran_date,na.rm=T))
time.period  # time period of the available transactions.
 
#b)
sum(Customer_Final$total_amt<0,na.rm=T) # count of transactions where the total amount of transaction was negative.

################# (5) ####################

filter.m <- Customer_Final[Customer_Final$Gender=='M',c('prod_cat','Gender')] # filtering the data where gender is male.
View(filter.m)
class(filter.m)
agg1 <- dplyr::group_by(filter.m,prod_cat)  #grouping the filtered data by prod_cat.
agg2 <- dplyr::summarise(agg1 ,for.male=dplyr::n())
View(agg1)
arr1 <- dplyr::arrange(agg2,desc(for.male)) # arranging the grouped data by desc order.
View(arr1) # viewing the data


filter.f <- Customer_Final[Customer_Final$Gender =='F',] # filtering the data where gender is female.
View(filter.f)
class(filter.f)
agg3 <- dplyr::group_by(filter.f,prod_cat)  #grouping the filtered data by prod_cat.
agg4 <- dplyr::summarise(agg3 ,for.female=dplyr::n())
View(agg4)
arr2 <- dplyr::arrange(agg4,desc(for.female)) # arranging the grouped data by desc order.
View(arr2)

# Books category is popular among females and books category is popular among males also.

################# (6) ####################


agg5 <- dplyr::group_by(Customer_Final,city_code)  #grouping the data by city_code.
agg6 <- dplyr::summarise(agg5 ,count1 =dplyr::n()) 
View(agg6)
arr3 <- dplyr::arrange(agg6,desc(count1)) # arranging the grouped data by desc order.


total.cust <- nrow(as.matrix(Customer_Final$cust_id))
total.cust

arr3 <- dplyr::mutate(arr3,'percentage'= round((count1/total.cust)*100,5)) # creating a new column for percentage,
View(head(arr3,1)) # city code 4 has the maximum customers with a 10.50622% of total customers.

################# (7) ####################

a1 <- dplyr::group_by(Customer_Final,Store_type)
a2 <-  dplyr::summarise(a1, Total_Qty = sum(Qty,na.rm = T),Total_Rate=sum(Rate,na.rm = T))

View(a2[a2$Total_Qty == max(a2$Total_Qty) & 
          a2$Total_Rate== max(a2$Total_Rate) ,"Store_type"]) # e-shop is the store type that sells the maximum products by Qty $ by Rate

################# (8) ####################

sum(Customer_Final[ Customer_Final$Store_type == 'Flagship store' & 
                      (Customer_Final$prod_cat == 'Electronics' | 
                         Customer_Final$prod_cat == 'Clothing' ),'total_amt'],na.rm = T)


################# (9) ####################

sum(Customer_Final[ Customer_Final$Gender == 'M' & 
                      Customer_Final$prod_cat == 'Electronics','total_amt'],na.rm = T)

################# (10) ####################

df.cust <- Customer_Final
df.cust <-  df.cust[df.cust$total_amt>=0,]

c1 <- dplyr::group_by(df.cust,cust_id)
c2 <- dplyr::summarise(c1,Count=n())
View(c2[c2$Count>10,])
nrow(c2[c2$Count>10,])

################# (11) ####################

#a)
for.age= max(Customer_Final$tran_date)
class(for.age)
Customer_Final <-  dplyr::mutate(Customer_Final,Age = round(as.numeric((for.age-(Customer_Final$DOB))/365),0) )
Customer_Final$Age

sum(Customer_Final[( Customer_Final$Age >=25 &  Customer_Final$Age <=35)  &
                      ( Customer_Final$prod_cat == 'Books' | 
                      Customer_Final$prod_cat == 'Electronics'),'total_amt'],na.rm = T) # This is the answer.

#b)
sum(Customer_Final[( Customer_Final$Age >=25 &  Customer_Final$Age <=35)  &
                     ( Customer_Final$tran_date >= '2014-01-01' & Customer_Final$tran_date <= '2014-03-01'),
                   'total_amt'],na.rm = T)

