# EucFACE Phosphorus synthesis code repository
Synthesizing the phosphorus-related (P) pools and fluxes for the Eucalyptus Free Atmospheric CO2 Enrichment (EucFACE) experiment 

# File structures:
<br/> The **Run_programs.R** script: master script that calls in all the functions and scripts. 
<br/> The **definitions** folder: all pre-defined constants and coefficients.
<br/> The **download** folder: stores downloaded data from HIEv. 
<br/> The **modules** folder: sub-folders contain all scripts for processing individual variables. 
<br/> The **plots_tables** folder: stores output tables and plots. 
<br/> The **programs** folder: pre- and post-processing scripts. 

# How to run:
<br/> To initiate the project, you need to firstly have access to HIEv (the Hawkesbury Institute for the Environment online data portal), because all the data will be accessed from HIEv. A token file is required, and you can place your token file in the name of **tokenfile.txt** in the master directory (i.e. the same directory as the **Run_programs.R** script).

<br/> Next to do is to check whether you have the necessary R packages installed. You can run the first several lines of the script **Run_programs.R** to check, as follow:

```
#### Source functions and packages
source("programs/prepare.R")

```

<br/> Then, with all the required packages properly installed, you could run the entire **Run_programs.R** script to generate the result. 

# Key definitions:
<br/> All fluxes are in the unit of mg/m2/d, and all pools are in the unit of g/m2. Concentrations are in their respective units, such as ppm, or %. 
<br/> For variables that are uncertain with respect to data processing, data quality and output unit, please refer to the individual variable comments for details. 


More to be written. 
