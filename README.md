# ndarc-point-dashboard
A shiny app dashboard that shows a novel way of presenting longitudinal data. 

## Introduction

## Sections
The POINT project had distinct questions relating to various components of the participant's life. These were sectioned into separate tabs for exploration. 

### 1. Overview & Meta data 
The Overview tab, gives an introduction to the paper and provides the metadata for familiarisation. 

### 2. Measures
The POINT study utilised various questionnaires relating to the different aspects of the participant's life. These measures are detailed here with relevant sources and clinical cut offs.

### 3. Demographics
The demographics tab shows baseline demographic information about the participants. It is divided into 4 boxes.

![Alt text](screenshots/Demographics.png?raw=true "Screenshot of the demographics tab.")

### 4. Pain

### 5. Physical Function

### 6. Treatment

### 7. Quality of Life

### 8. Mental Health

### 9. Substance Use

### 10. Medication Diary
At each wave, a seven-day medication diary collected frequency and dose information on all consumed pain-related medicines, psychiatric medicines and prescribed sleep medicines. The measures, tools, and data domains were selected based on recommendations made by the Initiative on Methods, Measurement, and Pain Assessment in Clinical Trials (IMMPACT)

![Alt text](screenshots/Medication Diary.png?raw=true "Screenshot of the medication diary tab.")

### 11. Data Dictionary
Data dictionaries are essential for data management. Many of them can be found in word documents, excel sheets or are a part of software attributes for example in SPSS. Here, the data dictionary (saved as a csv) is called into the shiny app using the 'DT' library. Here, not only is the data dictionary loaded and available, the app can handle the 4000 + variables in this dataset. Furthermore, there is a search bar on the top right that searches for text within the dictionary across all columns. To supplement this feature, categories and subcategories were added in separate columns to aid those who are unfamiliar with the data to find relevant variables and their descriptions.

![Alt text](screenshots/Data Dictionary.png?raw=true "Screenshot of the data dictionary tab.")

### 12. Acknowledgements

## Conclusion