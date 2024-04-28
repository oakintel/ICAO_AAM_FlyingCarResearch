
# Read the data
data <- read.csv("surveytesttest.csv", skip = 3, header = FALSE, na.strings = c("", " "), stringsAsFactors = FALSE)

# View the modified data
head(data)

# Read the data
colnames(data) <- c("Familiarity", "WillingnessToTry", "Interest", "FlyingExperience", "AirshowExperience","PerceivedReliability", "EnvironmentalConcern", "WillingnessToPay", "ImportanceOfTimeSavings","ImportanceOfTrafficAvoidance", "ImportanceOfConvenience", "ImportanceOfEnvBenefits","ComfortWithFlying", "MainConcern", "DemonstrationInterest", "AAMUseCases", "OhioTesting","AgeGroup", "IncomeLevel", "AreaType")

# View the modified data
head(data)
#view(data)
#write.csv(data, "modified_data.csv", row.names = FALSE)
```



