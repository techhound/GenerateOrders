setwd("~/RData/Random Data")

# Most customers will buy only one product - unless it's a Hershey bar :) That's why drawing a 1 has the highest chances
# 70% Chance of selecting a 1, followed by a 17% chance of selecting a 2, following by 10% chance, etc.
GetProductQuantity <- function() {
  x = c(1, 2, 3, 4)
  px = c(0.70, 0.17, 0.1, 0.03)
  #set.seed(123)
  sample(x, size = 1, replace = TRUE, prob = px)
}

GetCustomerQuantity <- function() {
  x = c(1, 2, 3, 4)
  px = c(0.40, 0.30, 0.20, 0.10)
  #set.seed(123)
  sample(x, size = 1, replace = TRUE, prob = px)
}

CreateTransaction <- function(dt, customerID, productMax, oNum) {
  orderNum = oNum
  transactionDate = dt
  customerID = customerID
  #set.seed(123)
  productID = sample(1:productMax, 1)
  quantity = GetProductQuantity()
  df <- data.frame(orderNum, transactionDate, customerID, productID, quantity)
  
  return (df)
}

GenerateCustomers <- function(dt, customerMax, productMax, oNum) {
  df <- data.frame(orderNum = NULL, transactionDate = NULL, customerID = NULL, productID = NULL, quantity = NULL)
  
  customersToGenerate <- GetCustomerQuantity() #simulates multiple customers per date

  for (cq in 1:customersToGenerate) {
    customerIDs <- sample(customerMax, customersToGenerate, FALSE)

    keepSameCustomer <- sample(c(TRUE,FALSE), prob = c(0.40, 0.60), 1)

    if (customersToGenerate > 1 && keepSameCustomer) { 
      customerIDs[2:customersToGenerate] = customerIDs[1] 
    }
    
    for (cust in 1:length(customerIDs)) { 
      dfTemp <- CreateTransaction(dt, customerIDs[cust], productMax, oNum)
      df <- rbind(df, dfTemp)
      
      # Need to account for the oNum incrementing outside this for loop. When cust == length(customerIDs), no increment should occur here.
      if(!keepSameCustomer & cust < length(customerIDs)) {
        oNum <- oNum + 1 
      }
    }
    
    oNum <- oNum + 1
  } 
  
  retVal <- list(oNum, df)
  return (retVal)
}

# Creates a series of orders for each transaction date (dateArr below). Generates the dates as a sequence based on the startingDate.
# Parameters:
#   startingDate - the first date in the series of transactions. Used in the creation of the date sequence (dateArr)
#   customerMax - the maximum value of the customerID in the customers table. See text for full explanation.
#   productMax - the maximum value of the productID in the products table. See text for full explanation.
#   numDate - the initial number of dates to create in the sequence. The number of actual dates (including repeats) will be significantly more.
# Return Value
#    List of order number and data frame. The order number is needed to maintain the sequence from one date to the next. The data frame contains the sequence of orders for that date.
GenerateOrders <- function(startingDate, customerMax, productMax, numDates) {
  df <- data.frame(orderNum=NULL, transactionDate=NULL, customerID=NULL, productID=NULL, quantity=NULL)
  
  oNum <- 1
  dateArr <- seq(as.Date(startingDate), by = "day", length.out = numDates)
  dateArr <- as.character(dateArr)

  for(dt in dateArr) {
    dfTemp <- GenerateCustomers(dt, customerMax, productMax, oNum)
    df <- rbind(df, dfTemp[[2]])
    oNum <- dfTemp[[1]] # this is needed as R does not support by reference variables
  }
  return (df)
}

orders <- GenerateOrders('2017-11-28', 1000, 15, 1000)
write.csv(orders, "orders.csv", row.names = FALSE)
