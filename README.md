
# Smart Service Project CGI Team 1 - Power Predict

Welcome to the GitHub page of Smart Service Project CGI Team 1!

This repository contains the code and datasets for a student project aimed at developing a smart service solution to predict the number of electric vehicle (EV) charging stations per postal code and estimate their average energy consumption. This README file will guide you through the project, its objectives, and the structure of the repository.

## Our Team

Gustaw Kempa i6333296  <br>
Radu Ionescu i6301965 <br>
Claudio Proietti Mercuri i6245870 <br>
Rafke Niemans i6300274 <br>
Lunara Dossayeva i6316054 <br>

## Table of Contents
* [Project Overview](#project-overview)
* [Repository Structure](#repository-structure)
* [Requirements](#requirements)
* [Data Prepatation](#data-preparation)
* [Modeling](#modeling)
* [Dashboard](#dashboard)


## Project Overview

The Smart Service Project focuses on leveraging data analysis and machine learning techniques to predict the demand for EV charging stations and estimate their energy consumption at a postal code level. This solution aims to assist in the planning and optimization of charging infrastructure. The main stakeholders of the project are small municipalities as they are the key decision-makers in the placement of new charging stations. The project is focused on Limburg province where the majority of municipaloties are rather small and could significantly benefit from the smart service.

The proposed smart service can forecast future demand for EV charging stations, their energy consumption thereby suggesting optimal locations for their placement. The solution provides municipalities with a valuable tool for making informed decisions that can benefit their communities and the environment as the could not only see the potential areas for the placement of new charging stations but also the expected load on the electricty in th area.

The final product of the solution comes in the dashboard for the usage of the small municipalities.


## Repository Structure

The repository is organized as follows:

```kotlin
├── code/
├── datasets/
├── output/
└── README.md
```

- `README.R`: This file contains project description, structure and requirements.

- `code/`: This folder contains all the R scripts used for data preparation and modeling.

- `datasets/`: This folder contains all the raw datasets needed for the modeling and forecasting as well as the output datasets from forecasting. The detailed list of the datasets can be found in the datasets table below. 

- `output/`: This folder contains all the output data sets with forecast and Tableau data and dashboard.

- `README.md`: The file you are currently reading. It serves as an overview and guide for the project.

The list of data sets and their sources in the data set folder:

| #  | Datasets | Description | Source | 
| ------------- | ------------- | ------------- | ------------- |
| 1  | Amenities_Categorical.csv | Categorical variables that indicates "1" if there are various amenities (such as hotels, supermarkets, etc) in the radius of 3 km per postal code |[CBS](https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/gegevens-per-postcode)  |
| 2  | BEV 2020.csv, BEV 2021.csv, BEV 2022.csv  | The number of battery-powered electric vehicles (BEV) in the Netherlands in 2020-2022 per postal code | [CBS & RVO](https://klimaatmonitor.databank.nl/jive)|
| 3  | Chargers_limburg.csv  | The number of EV charging points in Limburg province |[CBS & RVO](https://klimaatmonitor.databank.nl/jive)|
| 4  | Fuel_prices.csv  | The historical data of fuel prices 2006-2023 |[CBS](https://www.cbs.nl/) |
| 5  | PHEV 2020.csv, PHEV 2021.csv, PHEV 2022.csv  | The number of the plug-in hybrid electric vehicles (PHEV) in the Netherlands in 2020-2022 per postal code |[CBS & RVO](https://klimaatmonitor.databank.nl/jive)  |
| 6  | Public Fast 2020.csv, Public Fast 2021.csv, Public Fast 2022.csv  | The number of public fast EV charging stations in 2020-2022 per Gemeente |[CBS & RVO](https://klimaatmonitor.databank.nl/jive)  |
| 7  | Public Regular 2020.csv, Public Regular 2021.csv, Public Regular 2022.csv  | The number of public regular EV charging stations in 2020-2022 | [CBS & RVO](https://klimaatmonitor.databank.nl/jive)  |
| 8  | Semi-Public Fast 2020.csv, Semi-Public Fast 2021.csv, Semi-Public Fast 2022.csv  | The number of semi-public fast EV charging stations in 2020-2022 per Gemeente | [CBS & RVO](https://klimaatmonitor.databank.nl/jive)  |
| 9  | Semi-Public Regular 2020.csv, Semi-Public Regular 2021.csv, Semi-Public Regular 2022.csv  | The number of semi-public regular EV charging stations in 2020-2022 per Gemeente| [CBS & RVO](https://klimaatmonitor.databank.nl/jive)  |
| 10  | cars_limburg.csv | The number of fuel-powered cars in Limburg in 2000-2022 per postal code | [CBS](https://www.cbs.nl/) |
| 11  | postal_code.csv | Gemeentes, provinces and their postal codes | [CBS](https://www.cbs.nl/nl-nl/maatwerk/2022/37/buurt-wijk-en-gemeente-2022-voor-postcode-huisnummer) |
| 12 | full_data_wide_am.csv| The final dataset with independent and dependent variables per postal code for 2020-2022 | Output of data wrangling.R script|
| 13 | Tableu_data.csv| The dataset to be uploaded to Tableau to generate dashboards | Output of Power_usage_ addition.R |
| 14 | evdata.csv| The energy consumption by charging stations | [Elaad][https://platform.elaad.io/]|

<p align="right">(<a href="#project-overview">back to top</a>)</p>

## Requirements

In order to replicate the results of this project, you will need R version 4.2.3. and Rstudio. All the required libraries are indicated in R scripts files. 

<p align="right">(<a href="#project-overview">back to top</a>)</p>

## Data Preparation

The data preparation takes place in two R script files: "Data_wrangling.R" and "Power_usage_addition.R" .

This Data wrangling code script involves multiple steps of data cleaning, merging, and transforming. Firstly, it imports various relevant datasets relating to electric vehicles (EVs), plug-in hybrid electric vehicles (PHEVs), charging points, fuel prices, and car registration data for postal code areas in the Netherlands, particularly focusing on Limburg. The data spans several years and is reshaped from wide to long format where deemed necessary. The code then computes aggregated measures and combines these datasets based on postal code and time, and integrates additional information about fuel prices and the number of regular cars. Finally, the code interpolates missing values for the number of cars, includes amenity information for each postal code, and saves the resulting comprehensive dataset as a CSV file that is being used to build and compose our final model.

Unfortunately, the historical data of charging points energy consumption is not available publicly, therefore we used a dataset of energy consumption from a randomly sampled charging stations. Our assumption is that on average the consumption behaviour is the same for every charging stations and this approximation can be used to forecast the energy consumption. In the power demand.R, the randomly sampled data is transformed, energy consumption behaviour is then averaged by month and year.

<p align="right">(<a href="#project-overview">back to top</a>)</p>

## Modeling

Since we are dealing with the multivariate time series and the number of electric vehicles and EV charging stations are both endogenous and exogenous variables to each other, we decided to go with the Meta Prophet model which is good for time series forecasting, easily adjustable and interpretable (See Prophet_CP and Prophet_EV R script). The fuel prices are exogenous variable to our model and we used an ARIMA model to foreccast petrol prices have been forecast (See Fuel prices forecast R script).

## Dashboard

<img width="762" alt="image" src="https://github.com/r-niemans/cgiproject/assets/85016926/f37af76e-aedb-4cdf-ae5d-8109c2008151">

The dashboard herein is designed to offer insightful data on the quantity and distribution of charging points (CPs) across the Limburg region, both historically and with future projections. 

Delving into the dashboard's specifics, the left side features a color-coded map of Limburg, delineated by postal codes. The color gradation signifies the density of charging points (CPs) within each area - the darker the shade, the higher the number of CPs.
Upon selecting an area (number 1 on the screenshot), users are presented with relevant data including the quantity of CPs, the number of electric vehicles (EVs), the associated municipality, and the ratio of CPs to EVs. Additionally, an interactive slider (number 2 on the screenshot) enables users to review the progression of CP installations from 2020 to the present day. Upon integration of predicted data, users will be able to use the slider to examine projected future levels of CPs as well.

Moving on to the dashboard's right side, as initially outlined, a collection of bar charts presents key amenities located within varying radius, either 1, 3, 5 km or 5, 10, 20 km, depending on the facility type (number 3 on the screenshot). When a specific postal code is selected on the map, these bar charts update dynamically, displaying information pertinent to the chosen area. This latter graphics serve a dual purpose: from a business perspective, it supports companies in identifying strategic locations for new CP installations based on existing infrastructure and amenity proximity. From a consumer's viewpoint, it guides the selection of CPs based on their immediate needs for nearby facilities, thus enhancing their experience while their vehicle is being charged. 

In the final version of the dashboard, clicking a postal code area will prompt a line graph to appear. This graph will project the forecasted growth in the number of CPs for the selected area, thereby providing a more comprehensive understanding of the local EV infrastructure. In addition, the user will have the possibility to choose among three scenarios (pessimistic, neutral, and optimistic – based on the confidence intervals) for the future CPs growth.

In the dashboard's ultimate version, it will incorporate supplementary data and charts, including projections of electric vehicle numbers per postal code, anticipated gas prices, and crucially, the forecasted number of charging points per postal code by 2030. 

<p align="right">(<a href="#project-overview">back to top</a>)</p>

