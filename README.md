
This is a public version of the code that created:

Trudeau, J. (2022). Limiting aggressive policing can reduce police and civilian violence. _World Development_. 

**Table of Contents**
1. Data sources, collection and cleaning
2. Figures 
3. Tables
4. Appendices

------

**Data sources, collection, and cleaning**

- ```1_cleaning_FC.R``` downloads the Fogo Cruzado (Crossfire) data from their public API, cleans it, and outputs several dataframes for analysis 
  - Input files: police precinct shapefile (```lm_dp_2019.shp```), Fogo Cruzado data from their API
  - Output files: ```fogo_rd.RData```, ```fogo_RJ_PE_rawdata.RData```
- ```isp_daily.RData``` is an aggregated version of the microdata at the police-precinct-day level from the Public Safety Institute of Rio de Janeiro (_Instituto de Segurança Pública, ISP_). Researchers can request the microdata on their own (reported at the incident level) [here](http://www.ispdados.rj.gov.br/). 
- ```lm_dp_2019.shp``` is a shapefile showing the geographic boundaries of the _delegacias_, the military police precincts, in Rio de Janeiro. It can also be downloaded from the Public Prosecutor's website [_MP em Mapas_](http://apps.mprj.mp.br/sistema/inloco/).
- ```isolation_index_daily.dta``` is the isolation index from _InLoco_, shown [here](https://public.tableau.com/views/MKTScoredeisolamentosocial/Rankingdosestados?%3Asize=1200%2C853&%3Aembed=y&%3AshowVizHome=n&%3AapiID=host0). 
- ```googletrends.csv``` shows the daily trends from May 15-25 for the names "João Pedro," "Crivella" (Rio de Janeiro's mayor at the time), "Witzel" (Rio de Janeiro's governor at the time), and "Covid" in the state of Rio de Janeiro. It was downloaded directly from google trends [here](https://trends.google.com/trends/?geo=US). 

------

**Figures**

- ```Figure1.R``` replicates *Figure 1: Total shootings and deaths resulting from shootings registered in the Rio de Janeiro Metropolitan Area in 2019, by police presence*
  - Input files: ```fogo_rd.RData```, ```lm_dp_2019.shp```
  - Output files: Four panels of the map shown in Figure 1: ```m1.pdf```, ```m2.pdf```, ```m3.pdf```, ```m4.pdf```

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

------

**Tables**

- ```Table1.R``` replicates *Table 1: Effect of police raid limit on police violence*
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```
  - Output: prints LaTeX-compatible table directly in the script

- ```Table2.R``` replicates *Table 2: Effect of police raid limit on shooting-related violence, by type of police involvement*
  - Input file: ```fogo_rd.RData```
  - Output: prints LaTeX-compatible table directly in the script

- ```Table3.R``` replicates *Table 3: Effect of police raid limit on violent and non-violent crime*
  - Input file: ```isp_daily.RData```
  - Output: prints LaTeX-compatible table directly in the script
  
- ```Table4.R``` replicates Table 4: Effect of police raid limit on homicides in precincts with high criminal conflict*
  - Input file: ```isp_daily.RData```
  - Output: prints LaTeX-compatible table directly in the script

- ```Table5.R``` replicates *Table 5: Effect of police raid limit on violence: difference-in-differences specification* 
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```
  - Output: prints LaTeX-compatible table directly in the script
  
- ```Table6.R``` replicates *Table 6: Effect of police raid limit on shooting-related violence in Recife*
  - Input file: ```fogo_rd.RData```
  - Output: prints LaTeX-compatible table directly in the script

------

**Appendices**

- ```FigureA1.R``` replicates *Figure A1: Google search trends during the João Pedro scandal*
  - Input file: ```googletrends.csv```
  - Output: ```googletrends.pdf```

- ```TablesAppendixB.R``` replicates Tables B1-B18 in the supplementary appendix. 
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```
  - Output: prints LaTeX-compatible tables directly in the script

- ```FiguresAppendixB.R``` replicates Figures B1-B4 in the supplementary appendix. 
  - Input files: ```fogo_rd.RData```, ```isp_daily.RData```
  - Output files: ```polkilling_linear_bw.pdf```,  ```polkilling_quad_bw.pdf```,  ```polkilling_cubic_bw.pdf```,  ```sharepolice_linear_bw.pdf```,  ```sharepolice_quad_bw.pdf```,  ```sharepolice_cubic_bw.pdf```, ```hom_linear_bw.pdf```,  ```hom_quad_bw.pdf```,  ```hom_cubic_bw.pdf```,  ```homtop_linear_bw.pdf```,  ```homtop_quad_bw.pdf```,  ```homtop_cubic_bw.pdf```  
