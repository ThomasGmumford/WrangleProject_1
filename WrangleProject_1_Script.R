#Load in the Libraries
library(dplyr)
library(tidyr)

#Clening company names to lowercase and all one of four unique names
refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Phillips"|refine_original$company == "phillips"|refine_original$company == "phllips"|refine_original$company == "phillps"|refine_original$company == "phillipS"|refine_original$company == "fillips"|refine_original$company == "phlips"] <-"philips" 

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Akzo"|refine_original$company == "AKZO"|refine_original$company == "akz0"|refine_original$company == "ak zo"] <-"akzo" 

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Van Houten"|refine_original$company == "van Houten"] <-"van houten"

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Unilever"|refine_original$company == "unilver"] <-"unilever"

#Seperate the product ID code from its number
refine_original$Product.code...number <- as.character(refine_original$Product.code...number)
x <- c(refine_original$Product.code...number)
refine_original["product_code"] <- NA
refine_original["product_number"] <-NA
refine_original$product_code <- c(substring(x, 1, 1))
refine_original$product_number <- c(substring(x, 3))

#Identify each product code
for (i in 1:nrow(refine_original))
  refine_original$product_category[i] <- switch(refine_original$product_code[i], p="Smartphone",
                                   v="TV", x="Laptop", q="Tablet")

#Seperate the number from street name to order it properly (# name)
refine_original <- separate(refine_original, address,into = c("aname","anum"),
                       sep=" ")
#paste number and name into same column
for (i in 1:nrow(refine_original))
  refine_original$address[i] <- paste(refine_original$anum[i], refine_original$aname[i], sep=" ")

#Create correct full address column
for (i in 1:nrow(refine_original))
  refine_original$Full_Address[i] <- paste(refine_original$address[i], refine_original$city[i],
                                           refine_original$country[i], sep = ", ")
#Create dummy variables for company (can also use caret)
refine_original = within(refine_original, {
  company_philips = ifelse(company == 'philips', "1", "0")
  company_akzo = ifelse(company == 'akzo', "1", "0")
  company_van_houten = ifelse(company == 'van houten', "1", "0")
  company_unilever = ifelse(company == 'unilever', "1", "0")
})

#Create dummy variables for category (can also use caret)
refine_original = within(refine_original, {
  product_smartphone = ifelse(product_category == 'Smartphone', "1", "0")
  product_tv = ifelse(product_category == 'TV', "1", "0")
  product_laptop = ifelse(product_category == 'Laptop', "1", "0")
  product_tablet = ifelse(product_category == 'Tablet', "1", "0")
})
refine_original$aname <- NULL
refine_original$anum <- NULL

write.csv(refine_original, file = "refine_clean.csv")
