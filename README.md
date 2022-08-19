
This is a public version of the code that created:

Trudeau, J. (2022). Limiting aggressive policing can reduce police and civilian violence. _World Development_. 

**Table of Contents**
1. Data sources, collection and cleaning
2. Figures 
3. Tables

------
**Data sources, collection, and cleaning**

- ```1_cleaning_FC.R``` downloads the Fogo Cruzado (Crossfire) data from their public API, cleans it, and outputs several dataframes for analysis 
  - Input files: police precinct shapefile (```lm_dp_2019.shp```), Fogo Cruzado data from their API
  - Output file: ```fogo_rd.RData```
- ```1_cleaning_ISP.R```

------
**Figures**

*Figure 1: Total shootings and deaths resulting from shootings registered in the Rio de Janeiro Metropolitan Area in 2019, by police presence*

- ```Figure2.R``` replicates *Figure 2: Police involvement in violent events*
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```
  - Output file: ```sharepolice_rdplot.pdf```

- ```Figure3.R``` replicates *Figure 3: Shooting events involving police, by type of policing*
  - Input file: ```fogo_RJ_PE_rawdata.RData```
  - Output file: ```policingtypes.pdf```

- ```Figure4.R``` replicates *Figure 4: Violent and non-violent crime after the limit on police raids*
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```
  - Output file: ```isp_rd.pdf```

- ```Figure5.R``` replicates *Figure 5: Social Isolation Index*
  - Input file: ```isolation_index_daily.dta```
  - Output file: ```inloco.pdf```

-----
**Tables**

- ```Table1.R``` replicates *Table 1: Effect of police raid limit on police violence*
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```

- ```Table2.R``` replicates *Table 2: Effect of police raid limit on shooting-related violence, by type of police involvement*
  - Input file: ```fogo_rd.RData```

- ```Table3.R``` replicates *Table 3: Effect of police raid limit on violent and non-violent crime*
  - Input file: ```isp_daily.RData```

*Table 4: Effect of police raid limit on homicides in precincts with high criminal conflict*

*Table 5: Effect of police raid limit on violence: difference-in-differences specification* 

*Table 6: Effect of police raid limit on shooting-related violence in Recife*

