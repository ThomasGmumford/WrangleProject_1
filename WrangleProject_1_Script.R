library(dplyr)
library(tidyr)

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Phillips"|refine_original$company == "phillips"|refine_original$company == "phllips"|refine_original$company == "phillps"|refine_original$company == "phillipS"|refine_original$company == "fillips"|refine_original$company == "phlips"] <-"philips" 

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Akzo"|refine_original$company == "AKZO"|refine_original$company == "akz0"|refine_original$company == "ak zo"] <-"akzo" 

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Van Houten"|refine_original$company == "van Houten"] <-"van houten"

refine_original$company <- as.character(refine_original$company)
refine_original$company[refine_original$company == "Unilever"|refine_original$company == "unilver"] <-"unilever"

refine_original %>%
  select(company,Product.code...number)%>%
  arrange(company)

refine_original$Product.code...number <- as.character(refine_original$Product.code...number)
x <- c(refine_original$Product.code...number)
refine_original["product_code"] <- NA
refine_original["product_number"] <-NA
refine_original$product_code <- c(substring(x, 1, 1))
refine_original$product_number <- c(substring(x, 3))

refine_original = within(refine_original, {
  product_category1 = ifelse(product_code == 'p', "Smartphone", "")
  product_category2 = ifelse(product_code == 'v', "TV", "")
  product_category3 = ifelse(product_code == 'x', "Laptop", "")
  product_category4 = ifelse(product_code == 'q', "Tablet", "")
})
unite(refine_original,"product_category",product_category1:product_category4,sep = "", remove = TRUE)

unite(refine_original,"full_address",address, city, country,sep = ", ", remove = TRUE)

refine_original = within(refine_original, {
  company_philips = ifelse(company == 'philips', "1", "0")
  company_akzo = ifelse(company == 'akzo', "1", "0")
  company_van_houten = ifelse(company == 'van houten', "1", "0")
  company_unilever = ifelse(company == 'unilever', "1", "0")
})

refine_original = within(refine_original, {
  product_smartphone = ifelse(product_category == 'Smartphone', "1", "0")
  product_tv = ifelse(product_category == 'TV', "1", "0")
  product_laptop = ifelse(product_category == 'Laptop', "1", "0")
  product_tablet = ifelse(product_category == 'Tablet', "1", "0")
})