# ndarc-point-dashboard
A shiny app dashboard that shows a novel way of presenting longitudinal data. 

## Introduction
This shiny app dashboard demonstrates a novel way to present your data to the world. This dashboard is based on a six year prospective study of those using pharmaceutical opioids to treat chronic non-cancer pain. 

## Sections
The POINT project had distinct questions relating to various components of the participant's life. These were sectioned into separate tabs for exploration. 

### 1. Overview & Meta data (drafted)
The Overview tab, gives an introduction to the paper and provides the metadata for familiarisation. 

### 2. Measures (drafted)
The POINT study utilised various questionnaires relating to the different aspects of the participant's life. These measures are detailed here with relevant sources and clinical cut offs.

The measures section was created and saved as a comma separareted value (.csv) and is read into the tab using the 'DT' library.

### 3. Demographics (completed)
The demographics tab shows baseline demographic information about the participants. It is divided into 4 boxes.

![Alt text](images/demographics.png "Screenshot of the demographics tab.")

### 4. Pain

### 5. Physical Function

### 6. Treatment

### 7. Quality of Life

### 8. Mental Health

### 9. Substance Use

### 10. Medication Diary (completed)
At each wave, a seven-day medication diary collected frequency and dose information on all consumed pain-related medicines, psychiatric medicines and prescribed sleep medicines. The measures, tools, and data domains were selected based on recommendations made by the Initiative on Methods, Measurement, and Pain Assessment in Clinical Trials (IMMPACT).

The medication diary has five boxes which are divided into three sections. The first row is a selector, to select a medication which is sourced from the data dictionary. The results are filtered only to show 'medication diary' and 'drug'. 

The second row shows the proportion of users using a line plot with confidence intervals with the third box showing a table of summaries which comprise the plot. 

The third row shows a line plot of mean OME across time with the fifth box showing the summary table for the corresponding plot. 

![Alt text](images/medication-diary.png "Screenshot of the medication diary tab.")

### 11. Data Dictionary (completed)
Data dictionaries are essential for data management. Many of them can be found in word documents, excel sheets or are a part of software attributes for example in SPSS. Here, the data dictionary (saved as a csv) is called into the shiny app using the 'DT' library. Here, not only is the data dictionary loaded and available, the app can handle the 4000 + variables in this dataset. Furthermore, there is a search bar on the top right that searches for text within the dictionary across all columns. To supplement this feature, categories and subcategories were added in separate columns to aid those who are unfamiliar with the data to find relevant variables and their descriptions.

![Alt text](images/data-dictionary.png "Screenshot of the data dictionary tab.")

### 12. Acknowledgements

## Conclusion