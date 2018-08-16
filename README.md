This tool was developed for [CWetlands – the Constructed Wetlands Knowledge Platform](http://www.cwetlands.net/) of the United Nations University Institute for Integrated Management of Material Fluxes and of Resources [UNU-FLORES](https://flores.unu.edu/en/).
This demand-driven platform compiles data on constructed wetlands in two ways; 
 1. The project team at UNU-FLORES systematically consolidates and curates them according to user needs and
 2. Users may provide their own data through an online submission mechanism. 

CWetlands uses the open source database Geonode to store the data. The purpose of the R tool presented here is to provide an open-source expandable mechanism to support the extraction of meaningful data and information from peer-reviewed journal articles for input into CWetlands. 

The tool currently supports 

..a) the cleaning of special characters and whitespaces of a .txt converted document, as well as the division of the text into sections,  

..b) keyword matching and web screening, 

..c) the extraction of 11 different indicators of the CWetlands database based on regular expressions and web information, and 

..d) the export of the data to Geonode via PostgreSQL. We invite users to enhance  the tool further and to apply it for data input into CWetlands.
