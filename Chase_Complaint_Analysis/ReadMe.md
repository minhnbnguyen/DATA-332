# JP Morgan Chase Consumer Complaint Analysis 

## By Minh Nguyen ☀️

![Chase Banner](https://github.com/minhnbnguyen/DATA-332/blob/main/Chase_Complaint_Analysis/visualization/Font-of-the-Chase-logo.jpg)

## Introduction
This project analyzes the customer narrative complaint from JP Morgan Chase to find relationship between customer's emotion with complaint dispute rate. In this project, I compare the emotional content of disputed vs. non-disputed complaints to identify emotional patterns that might predict complaint resolution difficulty.

## Data Dictionary 📖
Our dataset includes the following columns:

- **Date.received**: Date that the complaint was received
- **Product**: Product that was complaint about (i.e Debt Collection, Mortgage,..)
- **Sub.product**: Sub category of the product (i.e Credit Card, Debit Card,...)
- **Issue**: Issue category
- **Sub.issue**: Sub issue category
- **Consumer.complaint.narrative**: Consumer explanation on the issue
- **Company.public.response**: Company public response via website or social media
- **Company**: Name of bank. Note: Our dataset includes all US banks, but for the range of this project I will filter out only JP Morgan Chase Co.
- **State**: State where the headquarter is located
- **ZIP.code**: ZIP code where the headquarter is located
- **Tags**: The department/office that the issue will be directed to 
- **Consumer.consent.provided.**: Consumer consent to public issue
- **Submitted.via**: Platform the issue was submitted
- **Date.sent.to.company**: Date that the issue was sent to the bank
- **Company.response.to.consumer**: Company resolution to the issue
- **Timely.response**: Whether if this is a timely response or not
- **Consumer.disputed.**: Whether if the consumer dispute the resolution or not
- **Complaint.ID**: The unique identifier of each complaint

## Data Cleaning Methodology To Ensure Tiday Data 🧹

### Tag column include multiple variables
- Split tag columns into seperate rows if there are more than 2 tags

```r
complaints_tibble <- complaints_tibble %>%
  separate_rows(Tags, sep = ", ")
```

### Standardize empty cells to na format
- Unified country name formats
- Standardized variations of USA/U.S.A to "United States"
- Determined missing countries from locality information

```r
# convert all empty cells into N/A instead of ""
col_w_empty_cells <- names(complaints_tibble[colSums(complaints_tibble == "", na.rm = TRUE) > 0])

complaints_tibble <- complaints_tibble %>%
  mutate(across(col_w_empty_cells, ~na_if(., "")))

# convert "N/A" to na
na_cols_logical <- sapply(complaints_tibble, function(x) {
  if(is.character(x)) {
    return(any(x == "N/A", na.rm = TRUE))
  } else {
    return(FALSE)
  }
})

cols_with_NA_text <- names(na_cols_logical)[na_cols_logical]

complaints_tibble <- complaints_tibble %>%
  mutate(across(cols_with_NA_text, ~na_if(., "N/A")))

```

### Date Formatting 📆
- Converted all date column from varchar to date format

```r
complaints_tibble$Date.received <- as.Date(complaints_tibble$Date.receive, format = '%m/%d/%Y')
complaints_tibble$Date.sent.to.company <- as.Date(complaints_tibble$Date.sent.to.company, format = '%m/%d/%Y')
```

## Data Summary
Our dataset spans nearly two centuries (1821-2017) and includes specimens from:
- United Kingdom (largest sample)
- Republic of Ireland
- United States
- Canada (smallest sample)

## Key Findings

### Temporal Distribution
- Most butterflies in this dataset were recorded in the 20th century
- Peak recording periods were in the 1920s and 1960s

### Geographic Patterns
- Specimens collected from two continents: Europe and North America
- Gender distribution is relatively equal across regions

### Wingspan Analysis
- Butterflies from North America have larger wingspans than those from Europe
- Statistical analysis confirms this difference is significant (p < 0.05)
