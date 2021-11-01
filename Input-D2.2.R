## Summerisation of DATA: Aggregate, Summary 
## Selecting a Part from a Dataset

setwd("E:/Outline/WD/D2")
dir()
retail <- read.csv("Retail Sales.csv")
str(retail)

View(retail)


## Identifying Unique Names 
## Now we would like to know the names of countries in the Dataset

countries <- unique(retail$Retailer.country)
countries

## Then how many countries are there , through it is reflected in the previous result 

length(unique(retail$Retailer.country))
nlevels(retail$Retailer.country)

## Time period covered in the dataset

unique(retail$Year)

## again you may check out the class of retail$Year
## you may try out with product line or product type or any other variable

## Now I would like to some basic information about country-wise sales report

t_sales <- aggregate(Planned.revenue~Retailer.country,retail,sum)
t_sales
t_sales1 <- t_sales[order(t_sales$Planned.revenue),]
t_sales1
t_sales2 <- t_sales[rev(order(t_sales$Planned.revenue)),]
t_sales2
t_sales3 <- aggregate(retail$Revenue~retail$Retailer.country, retail,sum)
class(t_sales3)
t_diff <- cbind(t_sales, t_sales3$`retail$Revenue`)
t_diff
class(t_diff)

r_sales <- rank(t_sales$Planned.revenue)
r_sales1 <- cbind(t_sales,r_sales)
r_sales1

## Countrywise & Yearwise total sales report

cy <- aggregate(Revenue~Year+Retailer.country,retail,sum)
cy 

# let us change the presentation format

cy1 <- aggregate(Revenue~Retailer.country+Year,retail,sum)
cy1

## Now let us select all infomation pertaining to Canada

canada <- subset(retail,retail$Retailer.country=="Canada")
canada

## let us examine the performance of each product line in Canada

p.line.canada <- aggregate(Revenue~Product.line,canada,sum)
p.line.canada

## 

p.line.canada[order(p.line.canada$Revenue, decreasing = TRUE),]


## year-wise performance of each product line

p.line.canada1 <- aggregate(Revenue~Product.line+Year,canada,sum)
p.line.canada1

## Now we would like to know which product type is doing good within the Personal Accessories

colnames(canada)

personal.accessories.canada <- subset(canada, canada$Product.line =="Personal Accessories")

head(personal.accessories.canada)
 
total.per.accessories <- aggregate(Revenue~Product.type,personal.accessories.canada,sum)
total.per.accessories[order(total.per.accessories$Revenue,decreasing = TRUE),]

## now it can be seen that within Personal Accessories, Eyeware is fetching maximum revenue
## now we would like to examine the performance of the product in detail

eyeware.canada <- subset(canada,Product.type=="Eyewear")
yearwise.eyewear <- aggregate(cbind(Planned.revenue,Revenue,Unit.price,Unit.cost,Quantity)~Product+Year,eyeware.canada,mean) 
yearwise.eyewear
yearwise.eyewear$profit <- yearwise.eyewear$Unit.price-yearwise.eyewear$Unit.cost 
yearwise.eyewear

## since the profit of the eyeware has been reduced significantly, we would like to examine the perfomance of other products 
## let us access the database personal.accessories contaning information on all personal products

watches.canada <- subset(canada, Product.type=="Watches")
yearwise.watches <- aggregate(cbind(Planned.revenue,Revenue,Unit.price,Unit.cost,Quantity)~Product.type+Year,eyeware.canada,mean)
yearwise.watches
yearwise.watches$profit <- yearwise.eyewear$Unit.price-yearwise.eyewear$Unit.cost 
yearwise.watches

## for the second most sold product we find that profit per unit has decreased
## now we would like examine profittablity of various product personal products

personal.accessories.canada$unit.profit <- (personal.accessories.canada$Unit.price - personal.accessories.canada$Unit.cost)/personal.accessories.canada$Unit.price
personal.accessories.canada[order(personal.accessories.canada$unit.profit, decreasing = TRUE),c(1,3,5,14)]
profitability.trend <- aggregate(unit.profit~Product+Year,personal.accessories.canada,mean)
profitability.trend
profitability.trend[order(c(profitability.trend$unit.profit,Product), decreasing = TRUE),]
