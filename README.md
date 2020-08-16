# Project Save the Lights, Save the World. 
## Team: EconData Stories
#### Author: Mohammad Aliraza


### Description of project

The interactive data apps is designed to enable planning and policy analysts to tier light zones for different areas in hobart. We connect to novel data sources giving us street pole level information on essential light areas and do real time budget forecasting analysis based on policy parameters (budget vs. environment vs. safety).

The app URL is: https://seyani.shinyapps.io/GovHack2020

### Data stories

- How can we empower policy analysts with the right data and tools to make decisions about street light investments.
- How can we combine public and corporate data to support conceptual ideas about lighting policy tradeoffs.
- How can we best visualise the short term and long term financial benefits of various policy options

### How to use

- 1.Ingest_data.R - Ingests data provided by City of Hobart, data cleaning steps include removing cameras/duplicate records, extracting wattage rating for each light asset,
                  API calls Google Places API for nearby bus stops, tourist attractions and parks.
- 2.Analysis.R - scratch file used to describe formulas applied for cost benefit analysis and cost forecasting, and annualised hours between sunset and sunrise.
- app.R - is the main file used to publish web app. It ingests data exported by '1.Ingest_data.R' 


- Input_data FOLDER - folder contains sample streetlight data provided by CoH, sunset_sunrise times derived from GeoScience Australia, and data from Google maps API for each pole.
- Output_data FOLDER - contains combined_file.csv which is used by app.R

### Presentation and Video
The pdf file 'Team EconData presentation.pdf' contains the presentation used for the video and presentation video 
https://youtu.be/Ra1wT2ODup0







