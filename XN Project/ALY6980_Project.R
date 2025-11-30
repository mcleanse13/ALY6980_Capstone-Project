# Install necessary packages and libraries
install.packages("readxl")
library(readxl)
install.packages(c("dplyr", "psych", "skimr", "Hmisc"))
library(dplyr)
library(psych)
library(skimr)
library(Hmisc)

# Replace with your actual file paths
file1_path <- "C:\\Users\\seanm\\OneDrive\\Desktop\\ALY6980 - XN Project (Group)\\BSFG Deals 011625.xlsx"
file2_path <- "C:\\Users\\seanm\\OneDrive\\Desktop\\ALY6980 - XN Project (Group)\\BSFG Accounts Jan 15 2025.xlsx"

# Read the files into R
data1 <- read_excel(file2_path)
data2 <- read_excel(file1_path)
View(data1)
View(data2)

# Rename specific columns
colnames(data1)[colnames(data1) == "NEW Customer Type"] <- "New Customer Type (Accounts)"
colnames(data2)[colnames(data2) == "NEW Customer Type(NEW Customer Type)"] <- "New Customer Type (Deals)"
colnames(data2)[colnames(data2) == "Lead Received(Lead Received)"] <- "Lead Received (Deals)"
colnames(data1)[colnames(data1) == "Lead Received"] <- "Lead Received (Accounts)"
colnames(data2)[colnames(data2) == "Client Inquiry(Client Inquiry)"] <- "Client Inquiry (Deals)"
colnames(data1)[colnames(data1) == "Client Inquiry"] <- "Client Inquiry (Accounts)"
colnames(data1)[colnames(data1) == "Mailing Country"] <- "Mailing Country (Accounts)"
colnames(data2)[colnames(data2) == "Mailing Country(Mailing Country)"] <- "Mailing Country (Deals)"
colnames(data1)[colnames(data1) == "Mailing State"] <- "Mailing State (Accounts)"
colnames(data2)[colnames(data2) == "Mailing State(Mailing State)"] <- "Mailing State (Deals)"
colnames(data2)[colnames(data2) == "Account Owner(Account Owner).id"] <- "Account Owner.id"
colnames(data1)[colnames(data1) == "Created Time"] <- "Created Time (Accounts)"
colnames(data1)[colnames(data1) == "Account Owner.id"] <- "Account Owner ID"
colnames(data1)[colnames(data1) == "Created Time"] <- "Created Time (Accounts)"
colnames(data1)[colnames(data1) == "Lead Source"] <- "Lead Source (Accounts)"
colnames(data1)[colnames(data1) == "Lead Sub Source.id"] <- "Lead Source ID (Accounts)"
colnames(data1)[colnames(data1) == "Lead Sub Source"] <- "Lead Sub Source (Accounts)"
colnames(data2)[colnames(data2) == "Created Time"] <- "Created Time (Deals)"
colnames(data2)[colnames(data2) == "Lead Source"] <- "Lead Source (Deals)"
colnames(data2)[colnames(data2) == "Lead Sub Source.id"] <- "Lead Sub Source ID (Deals)"
colnames(data2)[colnames(data2) == "Lead Sub Source"] <- "Lead Sub Source (Deals)"
colnames(data2)[colnames(data2) == "Account Owner.id"] <- "Account Owner ID"
colnames(data2)[colnames(data2) == "Created Time(Created Time)"] <- "Created Times (Deals)"
View(data1)
View(data2)

# Merge the datasets on a common column, e.g., "Record ID"
dataset <- full_join(data1, data2, by = c("Record Id", "Account Owner ID"))
View(dataset)

#Summary statistics of the dataset
head(dataset)
summary(dataset)
describe(dataset)
str(dataset)

# Drop specific columns by name
dataset <- dataset[, !(names(dataset) %in% c("Modified By.id", "Partner Rep.id", "Last Enriched Time", "Lead Sub Source.id.x", "Deal Owner.id", "Lead Sub Source.id.y", "Modified By", "Modified By.id", "Status", "Follow-up Date", "Change Log Time", "Locked", "Pipeline", "Customer Type", "Overall Sales Duration", "Enrich Status", "Lead Source ID (Accounts)", "Customer Type(Customer Type)", "Lead Sub Source(Lead Sub Source).id"))]
View(dataset)

# Reorder columns (example: placing 'Column3' first, then 'Column1', then 'Column2')
dataset <- dataset %>% select("Record Id", "Account Owner ID", "Account Owner", "Account Name.id", "Account Type", "Created Time (Accounts)", "Modified Time", "Last Activity Time", "Lead Received (Accounts)", "Lead Source (Accounts)", "Lead Sub Source (Accounts)",
                              "Client Inquiry (Accounts)", "Mailing Zip", "Mailing Country (Accounts)", "Mailing State (Accounts)", "Attended Presentation", "Partner Rep", "New Customer Type (Accounts)", "Lead Source (Deals)", "Lead Sub Source (Deals)", "Created Times (Deals)",
                              "Lead Sub Source ID (Deals)", "Client Inquiry (Deals)", "Lead Received (Deals)", "New Customer Type (Deals)", "Mailing State (Deals)", "Mailing Country (Deals)", "Proposal Sent Date", "Amount", "Deal Name",
                              "Deal Owner", "Created Time (Deals)", "Forecast Date", "Stage", "Closing Date", "Sales Cycle Duration", "Probability (%)")
View(dataset)
table(dataset$`Client Inquiry (Accounts)`)

#Fill in Blank Cells with 'NA'
library(tidyverse)
# Replace blank cells in specific columns with NA
cols_to_check <- c("Deal Name", "Pipeline", "Stage", "Account Name.id", "Deal Owner", 
                   "Created Time", "Forecast Date", "Lead Source", "Lead Sub Source", 
                   "Account Owner.id", "Customer Type", "Client Inquiry", "Lead Received", 
                   "New Customer Type", "Lead Sub Source.id", "Mailing State", 
                   "Mailing Country", "Amount", "Closing Date", "Probability (%)", 
                   "Proposal Start Date", "Sales Cycle Duration", "Overall Sales Duration")

# Loop through the columns and replace blank cells with NA, considering column type
for (col in cols_to_check) {
  # Only apply to character or factor columns
  if (is.character(dataset[[col]]) | is.factor(dataset[[col]])) {
    dataset[[col]][dataset[[col]] == ''] <- NA
  }
}
# Print the modified dataset
print(dataset)
View(dataset)
summary(dataset)

#Merge duplicate columns together
# Check for duplicate column names
duplicated_columns <- names(dataset)[duplicated(names(dataset))]
print(duplicated_columns)

# Load necessary library
library(ggplot2)

# Ensure 'Partner Rep' is treated as a factor to get individual bars for each unique rep
dataset$`Partner Rep` <- as.factor(dataset$`Partner Rep`)

# Create the bar chart
ggplot(dataset, aes(x = `Partner Rep`)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Total Count for Each Partner Rep", x = "Partner Rep", y = "Total Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

#Checking the amount per state
table(dataset$`Mailing State (Deals)`)

state_lookup <- setNames(state.abb, state.name)

# Create a lookup table for state names and their abbreviations
state_lookup <- setNames(state.abb, state.name)

# Edit inaccurate or incomplete values in cells of columns
dataset$`Mailing State (Deals)` <- ifelse(
  dataset$`Mailing State (Deals)` %in% state.name, 
  state_lookup[dataset$`Mailing State (Deals)`], 
  dataset$`Mailing State (Deals)`
)
table(dataset$`Mailing State (Deals)`)
View(dataset)
sum(is.na(dataset$`Mailing State (Deals)`))

dataset$`Mailing State (Deals)`[dataset$`Mailing State (Deals)` == "--"] <- NA
table(dataset$`Mailing State (Deals)`)
dataset$`Mailing State (Deals)`[dataset$`Mailing State (Deals)` == "No aplicable"] <- NA
table(dataset$`Mailing State (Deals)`)
dataset$`Mailing State (Deals)`[dataset$`Mailing State (Deals)` == "XX"] <- NA
table(dataset$`Mailing State (Deals)`)
View(dataset)

table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("United States", "US", "UNITED STATES")] <- "USA"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("CAN")] <- "Canada"
table(dataset$`Mailing Country (Deals)`)
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("AE")] <- "United Arab Emirates"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("AR")] <- "Argentina"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("AU")] <- "Australia"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("Austrailia")] <- "Australia"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("CN")] <- "China"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("CY")] <- "Cyprus"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("GB")] <- "Great Britain"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("HK")] <- "Hong Kong"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("IN")] <- "India"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("IS")] <- "Iceland"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("LB")] <- "Lebanon"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("NO")] <- "Norway"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("PA")] <- "Panama"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("PR")] <- "Puerto Rico"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("RO")] <- "Romania"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("SA")] <- "Saudi Arabia"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("TT")] <- "Trinidad and Tobago"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("TW")] <- "Taiwan"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("UK")] <- "United Kingdom"
table(dataset$`Mailing Country (Deals)`)
dataset$`Mailing Country (Deals)`[dataset$`Mailing Country (Deals)` %in% c("UY")] <- "Uruguay"
table(dataset$`Mailing Country (Deals)`)

View(dataset)
table(dataset$`Forecast Date`)
dataset$`Forecast Date`[dataset$`Forecast Date` == "1899-12-31"] <- NA
table(dataset$`Forecast Date`)

dataset$`Account Type`[dataset$`Account Type` %in% c("Other/Unknown")] <- "Other - Unknown"
table(dataset$`Account Type`)

dataset$`Lead Source (Accounts)`[dataset$`Lead Source (Accounts)` %in% c("Other - Internet")] <- "Internet"
table(dataset$`Lead Source (Accounts)`)
dataset$`Lead Source (Accounts)`[dataset$`Lead Source (Accounts)` %in% c("Other - Webinar Guest")] <- "Webinar Guest"
table(dataset$`Lead Source (Accounts)`)
dataset$`Lead Source (Accounts)`[dataset$`Lead Source (Accounts)` %in% c("Referral - Other")] <- "Referral"
table(dataset$`Lead Source (Accounts)`)

dataset$`Mailing Zip`[dataset$`Mailing Zip` %in% c("0")] <- "NA"
table(dataset$`Mailing Zip`)
dataset$`Mailing Zip`[dataset$`Mailing Zip` %in% c("-7666")] <- "NA"
table(dataset$`Mailing Zip`)

dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("UNITED STATES", "United States", "US")] <- "USA"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("CANADA", "CAN")] <- "Canada"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("UK")] <- "United Kingdom"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("60464")] <- "NA"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("AU")] <- "Australia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("AE")] <- "United Arab Emirates"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("AG")] <- "Antigua and Barbuda"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("AR")] <- "Argentina"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("BD")] <- "Bangladesh"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("BR")] <- "Brazil"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("CN")] <- "China"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("CO")] <- "Colombia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("CY")] <- "Cyprus"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("DE")] <- "Germany"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("EC")] <- "Ecuador"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("ET")] <- "Ethiopia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("GB")] <- "Great Britain"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("GT")] <- "Guatemala"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("HK")] <- "Hong Kong"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("IE")] <- "Ireland"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("IN")] <- "India"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("IS")] <- "Iceland"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("JM")] <- "Jamaica"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("LB")] <- "Lebanon"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("LC")] <- "Saint Lucia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("MM")] <- "Myanmar"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("MN")] <- "Mongolia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("MY")] <- "Malaysia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("NA")] <- "Namibia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("NG")] <- "Nigeria"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("NO")] <- "Norway"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("PA")] <- "Panama"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("PH")] <- "Philippines"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("PR")] <- "Puerto Rico"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("RE")] <- "Reunion"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("RO", "ROMANIA")] <- "Romania"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("SA")] <- "Saudi Arabia"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("SG")] <- "Singapore"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("SS")] <- "South Sudan"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("SV")] <- "El Savador"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("TT")] <- "Trinidad and Tobago"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("TW")] <- "Taiwan"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("UA")] <- "Ukraine"
table(dataset$`Mailing Country (Accounts)`)
dataset$`Mailing Country (Accounts)`[dataset$`Mailing Country (Accounts)` %in% c("UY")] <- "Uruguay"
table(dataset$`Mailing Country (Accounts)`)

dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Arizona - AZ")] <- "AZ"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("British Columbia")] <- "BC"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("California", "California - CA")] <- "CA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Colorado - CO")] <- "CO"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("10538")] <- "NY"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("33511")] <- "FL"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Connecticut")] <- "CT"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Florida", "Florida - FL")] <- "FL"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("District of Columbia - DC")] <- "DC"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Georgia - GA")] <- "GA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Vermont - VT")] <- "VT"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Virginia - VA")] <- "VA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Wisconsin")] <- "WI"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Texas - TX", "Texas")] <- "TX"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Tennessee - TN", "Texas")] <- "TN"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("South Carolina - SC")] <- "SC"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Pennsylvania - PA", "Pennsylvania")] <- "PA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Ohio - OH", "Ohio")] <- "OH"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("New York - NY", "New York")] <- "NY"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("New Jersey - NJ", "New Jersey")] <- "NJ"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("New Hampshire")] <- "NH"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Nevada - NV")] <- "NV"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Mississippi")] <- "MS"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Michigan - MI", "Michigan")] <- "MI"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Louisiana - LA", "Louisiana")] <- "LA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Illinois - IL", "Illinois")] <- "IL"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Idaho")] <- "ID"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Indiana - IN")] <- "IN"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Maryland - MD")] <- "MD"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("--")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("7030")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("BIHOR")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("C")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("No aplicable")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("75062")] <- "TX"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("80122")] <- "CO"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("84332")] <- "UT"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("91356")] <- "CA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Congo-Kinshasa")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Greater London")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Karnataka")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("M")] <- "MN"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Nuevo LeÃ³n")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Ontario")] <- "ON"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Orland Park")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Queensland", "ROO")] <- "NA"
table(dataset$`Mailing State (Accounts)`)
dataset$`Mailing State (Accounts)`[dataset$`Mailing State (Accounts)` %in% c("Tokyo", "XX")] <- "NA"
table(dataset$`Mailing State (Accounts)`)

library(dplyr)

dataset <- dataset %>%
  mutate(
    `Mailing Country (Accounts)` = ifelse(`Mailing State (Accounts)` %in% c("UK", "Australia", "Bahamas"), 
                                          `Mailing State (Accounts)`, 
                                          `Mailing Country (Accounts)`),
    `Mailing State (Accounts)` = ifelse(`Mailing State (Accounts)` %in% c("UK", "Australia", "Bahamas"), 
                                        NA, 
                                        `Mailing State (Accounts)`)
  )

View(dataset)
library(dplyr)

dataset <- dataset %>%
  mutate(
    `Mailing State (Accounts)` = case_when(
      `Mailing Zip` == 10031 ~ "NY",  # Assign NY for ZIP code 10031 (New York)
      `Mailing Zip` == 50060 ~ "IA",  # Assign IA for ZIP code 50060 (Iowa)
      TRUE ~ `Mailing State (Accounts)`  # Leave other rows unchanged
    )
  )

table(dataset$`Mailing State (Accounts)`)
View(dataset)

library(ggplot2)

# Create the data frame
state_counts <- data.frame(
  State = c("AB", "AK", "AL", "AR", "AZ", "BC", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MB",
            "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NA", "NC", "ND", "NE", "NH", "NJ", "NL", "NM", "NS", "NV", "NY", "OH", "OK", "ON", "OR", "PA",
            "PR", "QC", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
  Count = c(3, 5, 34, 10, 91, 14, 843, 73, 44, 30, 15, 913, 669, 19, 18, 13, 520, 64, 15, 17, 33, 69, 1, 
            102, 8, 79, 39, 46, 13, 6, 121, 131, 4, 13, 14, 236, 1, 10, 1, 46, 638, 86, 32, 49, 20, 126, 
            2, 6, 10, 46, 3, 69, 598, 28, 113, 4, 38, 24, 3, 3)
)

# Bar plot
ggplot(state_counts, aes(x = reorder(State, -Count), y = Count, fill = State)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips for better readability
  theme_minimal() +
  labs(title = "State/Province Distribution", x = "State/Province", y = "Count") +
  theme(legend.position = "none")

View(dataset)

library(tidygeocoder)
library(dplyr)

# Geocode dataset based on all available geographic fields
dataset <- dataset %>%
  mutate(
    full_address = paste(`Mailing Zip`, 
                         `Mailing Country (Accounts)`, 
                         `Mailing State (Accounts)`, 
                         `Mailing State (Deals)`, 
                         `Mailing Country (Deals)`, 
                         sep = ", ")
  ) %>%
  geocode(full_address, method = "osm", lat = latitude, long = longitude)

dataset1 <- dataset %>%
  mutate(latitude = ifelse(is.na(latitude), 0, latitude),
         longitude = ifelse(is.na(longitude), 0, longitude))

library(dplyr)

dataset %>%
  count(`Lead Sub Source (Accounts)`, sort = TRUE) %>%
  head(11)


View(dataset1)
summary(dataset)
str(dataset)
write.csv(dataset1, "dataset1.csv", row.names = FALSE)
getwd()

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

View(dataset)

#Vector Tiles
install.packages("mapdeck")
install.packages("sf")  # For handling spatial data
library(mapdeck)
library(sf)
library(dplyr)
install.packages("jsonlite")
install.packages("jsonify")
install.packages("tidygeocoder")
library(tidygeocoder)
library(dplyr)

#Basic Location Mapping (If you just want to see where people are located)
# Geocode "Mailing State (Accounts)"
datasets <- dataset %>%
  geocode(address = `Mailing State (Accounts)`, method = "osm", lat = latitude_accounts, long = longitude_accounts)

# Geocode "Mailing State (Deals)"
datasets <- dataset %>%
  geocode(address = `Mailing State (Deals)`, method = "osm", lat = latitude_deals, long = longitude_deals)

# Check the first few rows
head(datasets)
View(datasets)
table(datasets$latitude_deals)
table(datasets$longitude_deals)

# Remove rows with NA values in latitude_deals or longitude_deals
datasets_clean <- datasets %>%
  filter(!is.na(latitude_deals) & !is.na(longitude_deals)) %>%
  mutate(
    latitude_deals = as.numeric(latitude_deals),
    longitude_deals = as.numeric(longitude_deals)
  )
nrow(datasets_clean)
summary(datasets_clean$intensity)
head(datasets_clean$latitude_deals)
nrow(datasets_clean)
summary(datasets_clean$latitude_deals)
summary(datasets_clean$longitude_deals)

##Basic Mapping
library(mapdeck)
install.packages("leaflet")
library(leaflet)

leaflet(datasets) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude_deals,
    lat = ~latitude_deals,
    color = "blue",
    popup = ~paste("Latitude:", latitude_deals, "<br>Longitude:", longitude_deals)
  )


##Sales Funnel Analysis. Use a funnel chart to analyze deal stages.
library(ggplot2)

stage_counts <- dataset %>%
  group_by(Stage) %>%
  summarise(count = n())

ggplot(na.omit(stage_counts), aes(x = reorder(Stage, count), y = count, fill = Stage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sales Funnel Stages", x = "Stage", y = "Count") +
  theme_minimal()



##Check If Suspended Deals Have Proposal Sent Dates
table(!is.na(dataset$`Proposal Sent Date`), dataset$Stage == "Suspended")
dataset$suspended_after_proposal <- ifelse(
  dataset$Stage == "Suspended" & !is.na(dataset$`Proposal Sent Date`),
  "After Proposal Sent",
  "Before Proposal Sent"
)
table(dataset$suspended_after_proposal)
library(ggplot2)
ggplot(dataset[dataset$Stage == "Suspended", ], aes(x = `Proposal Sent Date`, fill = suspended_after_proposal)) +
  geom_histogram(binwidth = 30, position = "dodge") +
  labs(title = "Suspended Deals: Before vs. After Proposal Sent", x = "Proposal Sent Date", y = "Count of Suspended Deals") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal()
dataset$suspension_delay <- as.Date(dataset$`Closing Date`) - as.Date(dataset$`Proposal Sent Date`)
mean(dataset$suspension_delay, na.rm = TRUE)


##Predictive Models
#Deal Closing Prediction (Logistic Regression)
#Predict whether a deal will close successfully based on factors like pipeline, lead source, and probability.
# Convert categorical variables to factors
dataset$Stage <- as.factor(dataset$Stage)
# Convert 'Stage' to binary (1 = 'Closed Won', 0 = 'Other')
dataset$Stage_binary <- ifelse(dataset$Stage == "Closed Won", 1, 0)

# Check the new column
table(dataset$Stage_binary)


# Define model using backticks to handle spaces and special characters
dataset$Stage_Factor <- as.factor(dataset$Stage)
dataset$Stage_Recode <- recode(dataset$Stage, 
                               "Closed Won" = "Won", 
                               "Closed Lost" = "Lost",
                               "Prospecting" = "Lead")
model <- lm(`Probability (%)` ~ Amount + `Sales Cycle Duration` + `Stage_Factor`, 
            data = dataset)
summary(model)

##Sales Forecasting (Time Series). Predict future sales based on historical data.
library(dplyr)
library(lubridate)

# Ensure 'Created Time (Deals)' is in the correct date format
dataset$`Created Time (Deals)` <- as.Date(dataset$`Created Time (Deals)`)

# Aggregate sales by month (example using Created Time (Deals))
sales_trend <- dataset %>%
  filter(!is.na(Amount)) %>%  # Filter out rows with NA amounts
  mutate(month = floor_date(`Created Time (Deals)`, unit = "month")) %>%
  group_by(month) %>%
  summarise(total_sales = sum(Amount, na.rm = TRUE))

## View the sales_trend data
head(sales_trend)
# Create a time series object
sales_ts <- ts(sales_trend$total_sales, start = c(2020, 1), frequency = 12)

# View the time series object
sales_ts

##Customer Segmentation (K-Means Clustering). Segment customers based on deal amount and sales cycle duration.
# Select relevant columns (Amount and Sales Cycle Duration) from your dataset
df_cluster <- dataset %>% select(Amount, `Sales Cycle Duration`)

# Remove rows with NA values (if any)
df_cluster_clean <- df_cluster %>% drop_na()

# Perform k-means clustering on the cleaned data
kmeans_model <- kmeans(df_cluster_clean, centers = 3)

# Create a new dataframe with the clusters and the cleaned dataset
# Ensure the cluster column is assigned to only the rows in the cleaned dataset
df_cluster_clean$Cluster <- kmeans_model$cluster

# Merge the cluster information back into the original dataset
dataset <- dataset %>%
  mutate(Cluster = ifelse(is.na(Amount) | is.na(`Sales Cycle Duration`), NA, df_cluster_clean$Cluster))

# Check the first few rows to confirm
head(dataset)

# Visualize Clusters
ggplot(dataset, aes(x = Amount, y = `Sales Cycle Duration`, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Customer Segmentation", x = "Deal Amount", y = "Sales Cycle Duration")

table(dataset$`Attended Presentation`)
table(dataset$`Client Inquiry (Accounts)`)
table(dataset$`Probability (%)`)

# Load necessary library
library(ggplot2)

# Create a stacked bar plot
ggplot(dataset %>% filter(!is.na(`Client Inquiry (Accounts)`)), aes(x = `Attended Presentation`, fill = `Client Inquiry (Accounts)`)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No" = "red", "Uncertain" = "yellow", "Yes" = "green")) +
  labs(x = "Attended Presentation", y = "Proportion", fill = "Client Inquiry (Accounts)") +
  theme_minimal()

## Histogram for "Probability (%)"
ggplot(dataset, aes(x = `Probability (%)`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Probability (%)", x = "Probability (%)", y = "Count")

# Histogram for "Attended Presentation"
ggplot(dataset, aes(x = as.factor(`Attended Presentation`))) +
  geom_bar(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Attended Presentation", x = "Attended Presentation", y = "Count")

##Looking at Lead Sources and Lead Sub Sources
# Remove NA values from 'Lead Source (Accounts)' column
cleaned_data <- dataset[!is.na(dataset$`Lead Source (Accounts)`), ]

# Create the plot
ggplot(cleaned_data, aes(x = `Lead Source (Accounts)`)) + 
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Lead Source", x = "Lead Source", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, vjust = 1)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, vjust = 1),  # Adjust angle, hjust, and vjust for better spacing
    plot.margin = margin(1, 1, 2, 1, "cm")  # Increase margin for bottom (space for text)
  )

# Boxplot to compare Deal Amount by Lead Source
ggplot(dataset, aes(x = `Lead Source (Deals)`, y = Amount)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Deal Amount by Lead Source", x = "Lead Source", y = "Deal Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table to summarize Lead Source by Deal Stage
table(dataset$`Lead Source (Deals)`, dataset$Stage)

# Stacked bar plot for Lead Source by Deal Stage
# Filter out NA values in 'Lead Source (Deals)' and 'Stage' columns
# Filter out NA values in 'Lead Source (Deals)' and 'Stage' columns
cleaned_data <- datasets[!is.na(datasets$`Lead Source (Deals)`) & !is.na(datasets$Stage), ]

# Re-define the Stage factor with levels, ensuring proper ordering
cleaned_data$Stage <- factor(cleaned_data$Stage, 
                             levels = c("In Discussion", "Create Proposal", "Sent Proposal", 
                                        "Committed", "Signed Contract - No Deposit", 
                                        "Closed-Won", "Closed-Lost", "Suspended"), 
                             ordered = TRUE)

# Create the plot with the cleaned data
ggplot(cleaned_data, aes(x = `Lead Source (Deals)`, fill = Stage)) + 
  geom_bar(position = "fill") +
  labs(title = "Lead Source vs. Deal Stage", x = "Lead Source", y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate labels and adjust font size
    plot.margin = margin(1, 1, 2, 1, "cm")  # Increase space at the bottom for labels
  )


# Boxplot to compare Probability by Lead Source
ggplot(dataset, aes(x = `Lead Source (Deals)`, y = `Probability (%)`)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Probability of Closing by Lead Source", x = "Lead Source", y = "Probability (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Chi-squared test for Lead Source and Deal Stage (categorical)
chisq.test(table(dataset$`Lead Source (Deals)`, dataset$Stage))

# ANOVA for Lead Source and Deal Amount (continuous)
anova(lm(Amount ~ `Lead Source (Deals)`, data = dataset))

library(dplyr)

# Summarize total sales by Lead Source and Lead Sub Source
dataset %>%
  group_by(`Lead Source (Deals)`, `Lead Sub Source (Deals)`) %>%
  summarise(total_sales = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

##Look at Sales Cycle Duration
table(dataset$`Sales Cycle Duration`)
# Check for missing values in 'Sales Cycle Duration'
sum(is.na(dataset$`Sales Cycle Duration`))  # Check how many NA values

# Calculate the average sales cycle, excluding both NA and 0 values
average_sales_cycle <- mean(dataset$`Sales Cycle Duration`[dataset$`Sales Cycle Duration` > 0], na.rm = TRUE)

# Output the result
average_sales_cycle


# Calculate average sales cycle and total count by Lead Source, excluding 0 and NA values
average_sales_cycle_by_source <- dataset %>%
  filter(`Sales Cycle Duration` > 0) %>%  # Exclude 0 values
  group_by(`Lead Source (Deals)`) %>%
  summarise(
    total_count = n(),  # Count of deals per lead source
    average_cycle = mean(`Sales Cycle Duration`, na.rm = TRUE)  # Calculate mean
  )

# Display the result
average_sales_cycle_by_source

##-	How long does it take to reply after receiving a request for information?
library(dplyr)
#Lead Received Deals to Closing Date
# Convert columns to datetime format
dataset <- dataset %>%
  mutate(`Lead Received` = as.POSIXct(`Lead Received (Deals)`, format = "%Y-%m-%d %H:%M:%S"),
         `Client Inquiry` = as.POSIXct(`Closing Date`, format = "%Y-%m-%d %H:%M:%S"))

# Calculate response time in days
dataset <- dataset %>%
  mutate(Response_Time_Days = as.numeric(difftime(`Closing Date`, `Lead Received`, units = "days")))

# Summary statistics of response time in days
summary(dataset$Response_Time_Days)

#Lead Received Accounts to Created Time Accounts
# Convert columns to datetime format
dataset <- dataset %>%
  mutate(`Lead Received (Deals)` = as.POSIXct(`Lead Received (Deals)`, format = "%Y-%m-%d %H:%M:%S"),
         `Created Time (Deals)` = as.POSIXct(`Created Time (Deals)`, format = "%Y-%m-%d %H:%M:%S"))

# Calculate response time in days
dataset <- dataset %>%
  mutate(Response_Time_Days = as.numeric(difftime(`Created Time (Deals)`, `Lead Received (Deals)`, units = "days")))

# Summary statistics of response time in days
summary(dataset$Response_Time_Days)

ggplot(dataset, aes(x = `Lead Received (Deals)`, y = Response_Time_Days)) +
  geom_line(color = "blue") +
  labs(title = "Response Time Over Time", x = "Date of Lead Received", y = "Response Time (Days)") +
  theme_minimal()

##-	How long does it take to establish different parts of the business relationship?
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Convert relevant columns to Date format
dataset <- dataset %>%
  mutate(
    `Lead Received (Accounts)` = as.Date(`Lead Received (Accounts)`, format="%Y-%m-%d"),
    `Client Inquiry (Accounts)` = as.Date(`Client Inquiry (Accounts)`, format="%Y-%m-%d"),
    `Lead Received (Deals)` = as.Date(`Lead Received (Deals)`, format="%Y-%m-%d"),
    `Proposal Sent Date` = as.Date(`Proposal Sent Date`, format="%Y-%m-%d"),
    `Closing Date` = as.Date(`Closing Date`, format="%Y-%m-%d")
  )

# Compute time durations in days
dataset <- dataset %>%
  mutate(
    Client_Inquiry_Time = as.numeric(`Client Inquiry (Accounts)` - `Lead Received (Accounts)`),
    Proposal_Time = as.numeric(`Proposal Sent Date` - `Lead Received (Deals)`),
    Closure_Time = as.numeric(`Closing Date` - `Lead Received (Deals)`),
    Deal_Finalization_Time = as.numeric(`Closing Date` - `Proposal Sent Date`)
  )

# Compute average durations
avg_times <- dataset %>%
  summarise(
    Client_Inquiry_Time = mean(Client_Inquiry_Time, na.rm = TRUE),
    Proposal_Time = mean(Proposal_Time, na.rm = TRUE),
    Closure_Time = mean(Closure_Time, na.rm = TRUE),
    Deal_Finalization_Time = mean(Deal_Finalization_Time, na.rm = TRUE)
  )

# Convert to long format for visualization
avg_times_long <- tidyr::pivot_longer(avg_times, cols = everything(), names_to = "Stage", values_to = "Days")

ggplot(avg_times_long, aes(x = Stage, y = Days, fill = Stage)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Time to Establish Business Relationship Stages",
       x = "Stage",
       y = "Average Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Convert to long format for box plot
time_durations_long <- dataset %>%
  select(Client_Inquiry_Time, Proposal_Time, Closure_Time, Deal_Finalization_Time) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Stage", values_to = "Days")

ggplot(time_durations_long, aes(x = Stage, y = Days, fill = Stage)) +
  geom_boxplot() +
  labs(title = "Distribution of Time Durations Between Stages",
       x = "Stage",
       y = "Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

##-	Look at the clients that BS has landed and see where they are and how they got here.
landed_clients <- dataset %>%
  filter(Stage == "Closed - Won")

library(maps)

# Reorder the 'Mailing State (Deals)' factor based on count (highest to lowest)
# Remove NA values and reorder 'Mailing State (Deals)' by count (highest to lowest)
landed_clients_cleaned <- landed_clients[!is.na(landed_clients$`Mailing State (Deals)`), ]

# Create the plot
ggplot(landed_clients_cleaned, aes(x = reorder(`Mailing State (Deals)`, `Mailing State (Deals)`, function(x) -length(x)))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Landed Clients by State", x = "State", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

ggplot(landed_clients, aes(x = `Mailing Country (Deals)`)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Landed Clients by Country", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(landed_clients, aes(x = `Lead Source (Deals)`)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Lead Sources of Landed Clients", x = "Lead Source", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count the number of clients per state
state_counts <- landed_clients %>%
  group_by(`Mailing State (Deals)`) %>%
  summarise(client_count = n())

# Now let's make a plot of the clients by state
ggplot(state_counts, aes(x = `Mailing State (Deals)`, y = client_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Landed Clients by State",
       x = "State", 
       y = "Number of Clients") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Rename the 'Mailing State (Accounts)' column to 'state' for compatibility with usmap
state_counts <- state_counts %>%
  rename(state = `Mailing State (Deals)`)

# Ensure that the state names are in the correct format (uppercase)
state_counts$state <- toupper(state_counts$state)

# Plot the map with clients per state
plot_usmap(data = state_counts, values = "client_count") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", name = "Clients") +
  labs(title = "Clients per State")

# State population data
state_population <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
            "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
            "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
            "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  population = c(5024279, 1839106, 7151502, 3011524, 39538223, 5893718, 3959353, 989948, 21538187, 
                 10711908, 1961504, 2937880, 12812508, 6910840, 3605944, 3104614, 4657757, 5024279, 
                 1362359, 6177224, 7029917, 10077331, 5773714, 2961279, 6785528, 1085004, 2961279, 
                 3190369, 1377529, 9288994, 3011524, 20201249, 10439388, 774948, 11799448, 4237256, 
                 4505836, 13002700, 1097379, 5706494, 886667, 7029917, 29145505, 3271616, 1455271, 
                 8631393, 7705281, 2117522, 6154913, 1793716)
)

# Merge the state population data with the state_counts data
state_counts <- merge(state_counts, state_population, by.x = "state", by.y = "state")

# Calculate the number of clients per capita
state_counts$clients_per_capita <- state_counts$client_count / state_counts$population * 100000  # per 100,000

# Check for missing or zero values in clients_per_capita
summary(state_counts$clients_per_capita)

# Plot the map with clients per capita and set manual color limits
plot_usmap(data = state_counts, values = "clients_per_capita") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", name = "Clients per Capita", 
                        na.value = "white",  # color for missing data
                        limits = c(0, max(state_counts$clients_per_capita, na.rm = TRUE))) +  # Set limits based on data range
  labs(title = "Clients per State (Per Capita)")

##Analyze Lead Sources: Which sources generate the most deals?
library(ggplot2)

# Count deals per lead source
lead_source_counts <- table(dataset$`Lead Source (Deals)`)

# Convert to data frame for visualization
lead_source_df <- as.data.frame(lead_source_counts)
colnames(lead_source_df) <- c("Lead Source", "Deal Count")

# Bar plot
ggplot(lead_source_df, aes(x = reorder(`Lead Source`, -`Deal Count`), y = `Deal Count`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Deals by Lead Source", x = "Lead Source", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Conversion Rates: How many leads convert into closed deals?
# Count total leads and closed deals
total_leads <- nrow(dataset[!is.na(dataset$`Lead Received (Deals)`), ])
closed_deals <- nrow(dataset[dataset$Stage == "Closed Won", ])

# Compute conversion rate
conversion_rate <- closed_deals / total_leads * 100
print(paste("Conversion Rate:", round(conversion_rate, 2), "%"))

##Sales Cycle Efficiency: How long does it take to close a deal?
# Remove NA values
sales_cycle <- dataset$`Sales Cycle Duration`[!is.na(dataset$`Sales Cycle Duration`)]

# Calculate statistics
avg_sales_cycle <- mean(sales_cycle)
median_sales_cycle <- median(sales_cycle)

# Print results
print(paste("Average Sales Cycle Duration:", round(avg_sales_cycle, 2), "days"))
print(paste("Median Sales Cycle Duration:", round(median_sales_cycle, 2), "days"))

##Relationship between New Customer Type and Lead Source (Deals)
table(dataset$`New Customer Type (Deals)`, dataset$`Lead Source (Deals)`)

prop.table(table(dataset$`New Customer Type (Deals)`, dataset$`Lead Source (Deals)`), margin = 1) # Row-wise
prop.table(table(dataset$`New Customer Type (Deals)`, dataset$`Lead Source (Deals)`), margin = 2) # Column-wise

install.packages("gtsummary")
library(gtsummary)

library(gtsummary)
library(dplyr)

library(dplyr)

# Create a frequency table
freq_table <- dataset %>%
  count(`New Customer Type (Deals)`, `Lead Source (Deals)`) %>%
  tidyr::spread(`Lead Source (Deals)`, n, fill = 0)

# View table
freq_table

library(ggplot2)
library(dplyr)

dataset %>%
  filter(!is.na(`New Customer Type (Deals)`), !is.na(`Lead Source (Deals)`)) %>%  # Remove NAs
  count(`New Customer Type (Deals)`, `Lead Source (Deals)`) %>%
  ggplot(aes(x = `New Customer Type (Deals)`, y = n, fill = `Lead Source (Deals)`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Lead Sources by Customer Type",
       x = "New Customer Type",
       y = "Percentage",
       fill = "Lead Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dataset %>%
  filter(!is.na(`New Customer Type (Deals)`), !is.na(`Lead Source (Deals)`)) %>%  # Remove NAs
  count(`New Customer Type (Deals)`, `Lead Source (Deals)`) %>%
  ggplot(aes(x = `New Customer Type (Deals)`, y = n, fill = `Lead Source (Deals)`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Lead Source Count by Customer Type",
       x = "New Customer Type",
       y = "Count",
       fill = "Lead Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggmosaic)

dataset %>%
  filter(!is.na(`New Customer Type (Deals)`), !is.na(`Lead Source (Deals)`)) %>%  # Remove NAs
  ggplot() +
  geom_mosaic(aes(weight = 1, x = product(`Lead Source (Deals)`), fill = `New Customer Type (Deals)`)) +
  labs(title = "Mosaic Plot: New Customer Type vs. Lead Source",
       x = "Lead Source",
       y = "New Customer Type",
       fill = "New Customer Type") +
  theme_minimal()
table(dataset$`New Customer Type (Accounts)`)
