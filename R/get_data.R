# Downloads the UCI "Default of Credit Card Clients" data (CC BY 4.0) as cleaned by the imodels-data project:
# same 30,000 rows, with sex/education/marriage one-hot encoded. Original: https://archive.ics.uci.edu/dataset/350
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/csinva/imodels-data/master/data_cleaned/credit_card_clean.csv"
download.file(url, "data/credit_card_clean.csv", mode = "wb")
stopifnot(nrow(read.csv("data/credit_card_clean.csv")) == 30000)
