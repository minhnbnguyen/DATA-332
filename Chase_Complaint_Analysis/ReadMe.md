# JP Morgan Chase Consumer Complaint Analysis 

## By Minh Nguyen ☀️

![Chase Banner](https://github.com/minhnbnguyen/DATA-332/blob/main/Chase_Complaint_Analysis/visualization/Font-of-the-Chase-logo.jpg)

## Introduction
This project analyzes the customer narrative complaint from JP Morgan Chase to find relationship between customer's emotion with complaint dispute rate. In this project, I compare the emotional content of disputed vs. non-disputed complaints to identify emotional patterns that might predict complaint resolution difficulty.

## Data Dictionary 📖
Our dataset includes the following columns:

- **coreid**: A unique ID for each butterfly record
- **sex**: Whether the butterfly is male or female
- **country**: Where the butterfly was collected
- **year**: When the butterfly was recorded
- **left-wing/right-wing width**: Width of wings in millimeters
- **left-wing/right-wing length**: Length of wings in millimeters
- **left-wing/right-wing apex**: Measurement from tip to bottom of wing in millimeters
- **left-wing/right-wing posterior spot**: Distance of bottom wing spot from wing vein
- **left-wing/right-wing anterior spot**: Distance of top wing spot from wing vein

## Data Cleaning Methodology 🧹

### Gender Standardization
- Standardized values to "male", "female", or "unknown"
- Converted ambiguous entries (F?, M?) to "unknown"
- Filled NA values with "unknown"

### Location Standardization 🌎
- Unified country name formats
- Standardized variations of USA/U.S.A to "United States"
- Determined missing countries from locality information

### Year Formatting 📆
- Fixed missing and incorrect year values
- Converted all years to numeric format

### Measurement Standardization 📏
- Rounded all measurements to three decimal places
- Converted text-formatted measurements to numeric values

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
