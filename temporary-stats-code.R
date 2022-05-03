# stat analysis with predictions code
rm(list = ls())

library(lme4)
library(ggiraph)
library(ggiraphExtra)

# Read Dataset
data = read.csv("data/data.csv", na.strings = c("",".","NA"))
factors = c("Dataset", "Location", "County", "AgriLife.Region", "Region", "Irrigation", 
            "Hybrid", "Brand", "Maturity", "Previous.Crop")
numerics = c("DA", "PH", "EX", "MST", "bu.per.acre.Yield", "lbs.per.ac.Yield", "GY", "Lodging", "RowWidth", 
             "Location.Average.Yield", "plot.length", "rainfall", "Irrigation.Amount", 
             "Total.Moisture", "Population", "Days.from.Plant.to.Harvest")

integers = c("Number.of.Rows", "Year")
data[,factors] = lapply(data[,factors], as.factor)
data[,numerics] = lapply(data[,numerics], as.numeric)
data[,integers] = lapply(data[,integers], as.integer)

usda = data[which(data$Dataset == "USDA"),]
txar = data[which(data$Dataset == "TXAR"),]

traits = c("Year", "DA", "PH", "EX", "GY")

##### Irrigation #####
Irr.model = lm(lbs.per.ac.Yield ~ Year + Irrigation, txar)
ggPredict(Irr.model) + 
  labs(x = "Year", y = "Yield (lbs/ac)") +
  theme_minimal()

##### Total.Moisture #####

Total.Moisture.model = lm(lbs.per.ac.Yield ~ Year + Total.Moisture, txar)

ggPredict(Total.Moisture.model) + 
  labs(x = "Year", y = "Yield (lbs/ac)") +
  theme_minimal()


##### Lodging #####

ldg.model = lm(lbs.per.ac.Yield ~ Year + Lodging, txar)

ggPredict(ldg.model) + 
  labs(x = "Year", y = "Yield (lbs/ac)") +
  theme_minimal()



colnames(txar)

##### Lodging #####

ldg.model = lm(lbs.per.ac.Yield ~ Year + Lodging, txar)

ggPredict(ldg.model) + 
  labs(x = "Year", y = "Yield (lbs/ac)") +
  theme_minimal()



