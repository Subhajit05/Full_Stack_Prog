## Basic manipulations - Creating Dataframe, Categorical Variable, Sorting , Ranking , Ordering

## Exploring Dataframe, Matrix - Use of rbind, cbind & Arranging data (sort, rank, order etc.)

analy <- round(sample(40:100,15),2)
math <-  round(sample (40:100,15),2)
stat <- round(sample (40:100,15),2)
sql <- round(sample(40:100,15),2) 
marks <- cbind(analy,math,stat,sql)
marks
class(marks)
marks <- as.data.frame(marks)
marks

## Splitting dataset with [,]
## with in the Third Bracket , left to comma indicates Rows - Rigt to comma indicates Columns

# selecting columns 
marks$analy 
marks[,1]

marks[,1][1]
marks$analy[1:length(marks$analy)]


marks[,1][1:3]

marks$analy[1][1:3] ## why NAs??

# Selecting Rows 

marks[1,]
marks[1,][1:3] # 1 row and three cols

marks[c(1,4,7,8),][4] # 4th column 
marks[c(1,4,7,8),][1,3] # 3rd column 1 element
marks[c(1,4,7,8),][c(1,4),2] # 2nd column 1st and 4th element.
marks[c(1,4,7,8),][c(1,4),2][1] # 2nd column 1st element

# Conditonal selection of Rows

marks[marks$analy>60,] # 1 conditions
marks[marks$analy>80|marks$stat>70,] # 2 conditions
marks[marks$analy>80|marks$stat>70 & marks$sql>70,] # 3 condtions
marks[marks$analy>80 & marks$sql>70|marks$stat>70 & marks$sql>70,]

# Arranging rows and columns 

m1 <- marks[c(4:1),] ## position of the rows in reverse order - all columns seclected
m1

m2 <- marks[c(3,5,7,9),] # selecting rows arbitrarily - all colums selected
m2

# playing with colums

marks
marks[,c(3,2,1,4)] # all rows selected - position of the colums changed
marks[,c(3,2,1,4)][1,3] # first element of 3rd column
marks[,c(3,2,1,4)][1:3,c(1,3)] # first 3 elements of 1st and 3rd col.


# selecting a part of the dataframe

s1 <- subset(marks, subset = marks$analy>mean(marks$analy))
s1 # all the cols. of the main database is there and a logical condition has been tested

s2 <- subset(marks, subset = marks$analy>mean(marks$analy) & marks$sql>70)
s2

s3 <- subset(marks, subset = marks$analy>mean(marks$analy) & marks$sql>70, select = - math)
s3 # col of math has been dropped 


## Conversion - Continuous to Categorical variables

r_means <- rowMeans(marks)
marks1 <- cbind(marks,r_means)
marks1
gd <- c("F","C","B","A")
grade<- cut(r_means,breaks=c(0,49.5,60,70,100),labels = gd)
marks2 <- cbind(marks1,grade)
marks2

grade_summary <-as.data.frame(table(marks2$grade))
colnames(grade_summary) <- c("Grade","No")
grade_summary
grade_summary <-cbind(table(marks2$grade))
grade_summary1 <- prop.table(grade_summary,margin = 2)
grade_summary1 <- round(grade_summary1,2)
grade_summary1

## A rough visualisation of the performance
pie(grade_summary1,main="Analysis of students Performance")

## ORDERING, RANKING , SORTING
## ordering the marks2 dataset on the basis of marks scored in analytics

marks2[order(marks2$analy),]

## decreasing ordering  of the marks2 dataset on the basis of marks of analytics

marks2[rev(order((marks2$analy))),]

## If two variables are involved for ordering, then first variable is used to rank and second one to break the tie if any 

marks2[order(marks2$analy,marks2$sql),]
marks2[rev(order(marks2$analy,marks2$stat)),]

## Now if the sequencing of columns of the ordered data can be changed in the following manner

marks2[rev(order(marks2$analy,marks2$stat)),c(6,5,4,1,2,3)]

## Lets us add a column containing the sum of the marks

total_marks <- rowSums(marks)
total_marks
marks3 <- cbind((marks2[,c(1:4)]),total_marks,marks2[,c(5,6)]) 
marks3
class(marks3)

## Note the values of total marks column is missing, the output only carries ranks

rank<- rank(marks3$total_marks)
rank

## now we will be sorting the total marks 

sort <- sort(total_marks)
marks4 <- cbind(marks3,rank, sort)
marks4
min(total_marks)
max(total_marks)

## Extracting data with LOGICAL CONDITIONS - selecting students who has scored more than mean marks in analytics

summary(marks2)
marks2[analy>mean(analy),]
length(marks2>mean(marks2$analy))
dim(marks2[analy>mean(analy),])
marks> mean(marks2$analy)

## identifying students who have scored more than mean in BOTH stat and sql

marks2[analy>mean(analy) & sql>mean(sql),]

## examining presence students who above average in stat and math, but belowe average in analytics

marks2[analy<mean(analy) & stat>mean(stat) & mean(math),]

## identifying STUDENTS WHO ARE IN FOURTH QUARTILE IN ANALYTICS AND MATHEMATICS

marks2[analy > 1.25*median(analy) & math > 1.25*median(math),]

## CREATING A SUBSET OF A DATATSET , selecting students secured Grade A]

gradeA<-subset(marks2, grade=="A")

## identifying Grade-A students with below average marks in SQL

subset(marks2,grade=="A" & sql<mean(sql))
subset(marks2,grade=="A" | grade!="A" & sql>mean(sql))

## Summerisation of DATA: Aggregate, Summary 
## Selecting a Part from a Dataset

setwd("E:/Outline/WD/D2")
dir()
retail <- read.csv("Retail Sales.csv")
View(retail)
names(retail)
class(retail)
class(retail$Retailer.country)

## Identifying Unique Names 
## Now we would like to know the names of countries in the Dataset

countries <- unique(retail$Retailer.country)
countries