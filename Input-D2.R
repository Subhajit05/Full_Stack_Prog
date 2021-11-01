## Basic manipulations with a dataset ####

analytics <- round(sample(40:100,35),2)
math <-  round(sample (40:100,35),2)
stat <- round(sample (40:100,35),2) 
sql <- round(sample(40:100,35),2)
retail <- sample(40:100,35)

marks <- cbind(analytics,math,stat,sql)
class(marks)
head(marks)

40.00000356
99.00000000001

marks <- as.data.frame(marks)

class(marks)
head(marks)




head(marks,1)
tail(marks)
class(marks)



avg_marks  <- rowMeans(marks) # adding row mean

marks1 <- cbind(marks,avg_marks)
head(marks1)
class(marks1)



# Creating Categorical Variables

gd <- c("F","C","B","A")

grade<- cut(avg_marks,breaks=c(0,49.5,60,70,100),labels = gd)

marks2 <- cbind(marks1,grade)

marks2

table(marks2$grade) # examining frequency distribution

grade_summary <-as.data.frame(table(marks2$grade))
colnames(grade_summary) <- c("Grade","No")

grade_summary <-cbind(table(marks2$grade))
grade_summary


# Use of proportion table

grade_summary1 <- prop.table(grade_summary,margin = 2) 

grade_summary1

grade_summary1 <- round(grade_summary1,2)
grade_summary1 



## A rough visualisation of the performance

pie(grade_summary1)



## ORDERING, RANKING , SORTING
## ordering the marks2 dataset on the basis of marks scored in analytics

marks2[order(marks2$analy),]




## decreasing ordering  of the marks2 dataset on the basis of marks of analytics

marks2[rev(order((marks2$analytics))),]




## If two variables are involved for ordering, then first variable is used to rank and second one to break the tie if any 

marks2[order(marks2$analy,marks2$sql),]

marks2[rev(order(marks2$analy,marks2$stat)),]

## Now if the sequencing of columns of the ordered data can be changed in the following manner

marks2[rev(order(marks2$analy,marks2$stat)),c(6,5,4,1,2,3)]


## Lets us add a column containing the sum of the marks

total_marks_scored <- rowSums(marks)
total_marks_scored
marks3 <- cbind((marks2[,c(1:4)]),total_marks_scored,marks2[,c(5,6)]) 
marks3
class(marks3)




## Note the values of total marks column is missing, the output only carries ranks
rank<- cbind(rank(marks3$total_marks_scored))
rank



## now we will be sorting the total marks 

sort <- cbind(sort(total_marks_scored))
marks4 <- cbind(marks3,rank, sort)
marks4

## Extracting data with LOGICAL CONDITIONS - selecting students who has scored more than mean marks in analytics

summary(marks2)



mean(marks2$analytics)


best_analytics <- marks2[analytics > mean(analytics),]

best_analytics

length(marks2[marks2$analytics > mean(marks2$analytics),])

dim(marks2[analytics > mean(analytics),])

marks2 > mean(marks2$analy)



## identifying students who have scored more than mean in BOTH stat and sql

marks2[analytics > mean(analytics) & sql > mean(sql),]



## examining presence students who above average in stat and math, but belowe average in analytics

marks2[analytics < mean(analytics) & stat > mean(stat) & mean(math),]



## identifying STUDENTS WHO ARE IN FOURTH QUARTILE IN ANALYTICS AND MATHEMATICS

marks2[analytics > 1.25*median(analytics) & math > 1.25*median(math),]


## CREATING A SUBSET OF A DATATSET , selecting students secured Grade A]

gradeA <- subset(marks2, grade=="A")

summary(gradeA)

## identifying Grade-A students with below average marks in SQL

gradeA.1 <- subset(marks2,grade == "A" & sql < mean(sql))

dim(gradeA.1)

s2 <- subset(marks2,grade == "A" | grade!="A" & sql > mean(sql))

aggregate(analytics ~ grade, mean, data = marks2)

table(marks2$grade)

tapply(marks2$analytics, marks2$grade, mean)




  
  
  
  



## Summerisation of DATA: Aggregate, Summary 
## Selecting a Part from a Dataset

setwd("E:/Outline/WD/D2")
dir()
retail <- read.csv("Retail Sales.csv")
reatil1 <- read.csv("E:/Outline/WD/D2/Retail Sales.csv")
names(retail)
class(retail)
class(retail$Retailer.country)


## Identifying Unique Names 
## Now we would like to know the names of countries in the Dataset

countries <- unique(retail$Retailer.country)
countries

## Then how many countries are there , through it is reflected in the previous result 

length(unique(retail$Retailer.country))

## Time period covered in the dataset
unique(retail$Year)

## again you may check out thhe class of retail$Year
## you may try out with product line or product type or any other variable
## Now let us select all information pertaining to planned revenue from various product lines
## aggregate is used to split the data in to various categories 
## the first argument could be one or a list of NUMERIC VARIABLE(S), second argument is a FACTOR, AND THIRD ARGUMENT IS THE MATHEMATICAL FUNC THAT IS TO BE APPLIED ON THE NUMERICAL VARIBLES 

c_sales <- aggregate(retail$Revenue ~ retail$Retailer.country,retail,sum)
c_sales

colnames(retail)
p.line_p.rev <- aggregate(retail$Planned.revenue ~ retail$Product.line,retail,sum)

p.line_p.rev

## same information , with year-wise break-up

year.wise_p.rev <- aggregate(retail$Planned.revenue~retail$Year, retail, sum)
year.wise_p.rev

## Now we would like to annual revenue generated by each product line

aggregate(aggregate(retail$Revenue~retail$Product.line+retail$Year,retail,sum))

## examining the diifference between planned and actual revenue

rev.diff <- aggregate(cbind(Planned.revenue,Revenue)~Year,retail,sum)
rev.diff ## remember to type the correct names in the same as it is displayed in the column names of the database
rev.diff

## now we would like to understand over the years how product lines have performed 

y.rev.diff <- aggregate(cbind(Planned.revenue,Revenue)~retail$Product.line+retail$Year, retail,sum)
y.rev.diff
r.diff <- round((y.rev.diff$Planned.revenue-y.rev.diff$Revenue)/y.rev.diff$Planned.revenue,2)
y.rev.diff <- cbind(y.rev.diff,r.diff)
y.rev.diff

## now we would like see how average price of product line changed over the years

avg.price<- aggregate(cbind(Unit.price,Unit.cost)~retail$Retailer.country+retail$Year, retail,mean)

## now let us look at most popular product of each country

popular.product<- aggregate(Revenue~Retailer.country+Year+Product.line,retail, sum)

## Now let us subset the database and capture all the data of CANADA

canada <- subset(retail,retail$Retailer.country=="Canada")

## Now my objective is to identify the best performing products in CANADA
## Best performance can be defined in many ways here we have taken the maximum revenue generated by a product line

best.product.canada <- aggregate(Revenue~Year+Product.line,canada,max)
best.product.canada

## Next, I would like to see the price performance of the best product over the years

camp.equip.canada <- subset(canada,Product.line == "Camping Equipment")
price.camp.equip <- aggregate(cbind(Unit.price,Unit.cost)~Year,camp.equip.canada,mean)
price.camp.equip
price.change <- diff(price.camp.equip$Unit.price)
price.change
cost.change <- diff(price.camp.equip$Unit.cost)
cost.change
price.cost.change <- cbind(price.change,cost.change)
price.cost.change
plot(price.cost.change)


## what could be the other ways to define best performing product line?

