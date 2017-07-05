# Read in neccessary EXIO tables
# And derive indirect energy intensities per EXIO sector

# setwd("H:/MyDocuments/IO work/")

path_iot <- "C:/Users/tudeschi/SharePoint/DLE - Documents/IO/Data - EXIOBASE/mrIOT_PxP_ita_coefficient_version2.2.2/"
path_sut <- "C:/Users/tudeschi/SharePoint/DLE - Documents/IO/Data - EXIOBASE/mrSUT_version2.2.2/"

# From IoT folder
final_demand <- read.table(paste(path_iot, "mrFinalDemand_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
final_demand <- final_demand[,c(-1,-2,-3)]

L_inverse <- read.table(paste(path_iot, "L_inverse.txt", sep=""), header=FALSE, sep=",", dec=".")

factor_input <- read.table(paste(path_iot, "mrFactorInputs_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
factor_input <- factor_input[,c(-1,-2)]

# iot <- read.table(paste(path_iot, "mrIot_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
# iot <- iot[,c(-1,-2,-3)]

supplym <- read.table(paste(path_sut, "mrSupply_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".")

fd_materials <- read.table(paste(path_iot, "mrFDMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
fd_materials <- fd_materials[,c(-1,-2)]

# Material extension with more energy carrier resolution from NTNU (ver 2.2.0)
# However these extensions are in TJ unit, which need to be divided by total use by product to get intensities.
materials <- read.table(paste(path_iot, "mrMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
materials <- materials[,c(-1,-2)]
# materials_reduc <- read.table(paste(path_iot, "mrMaterials_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
# materials_reduc <- materials_reduc[,c(-1,-2)]

# final_demand_material <- read.table(paste(path_iot, "mrFDMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
# final_demand_material <- final_demand_material[,c(-1,-2)]

#emissions <- read.table(paste(path_iot, "mrEmissions_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", 
 #                      skip=2, nrows=85, stringsAsFactors = FALSE)
#emissions <- emissions %>% select(-V2, -V3) %>% filter(grepl('CH4|CO2|N2O', V1)) 
#GHG_item <- emissions$V1
#emissions <- emissions %>% select(-V1)


## Emissions from EXIOBASE 2.3 (Changed in 06/16/2017)

path_em <- "C:/Users/tudeschi/SharePoint/DLE - Documents/IO/Data - EXIOBASE/extension2.3.0/"

emissions <- read.table(paste(path_em, "mrEmissions_pxp_version2.3.0.txt", sep=""), header=FALSE, sep="\t", dec=".", 
                        skip=2, nrows=110, stringsAsFactors = FALSE)
emissions <- emissions %>% select(-V2, -V3) %>% filter(grepl('CH4|CO2|N2O|HFC|PFC', V1)) %>% rename(GHG_item = V1)  # Emissions table in kg, except for HFC and PFC in Kg CO2 eq.
GHG_item <- emissions$GHG_item

## Convert all GHG_items to kg CO2 eq

emission_conv = read.csv("C:/Users/tudeschi/Documents/DLE_scripts/Input/Data/Emission metrics_WG1_AR5_EXIOBASE2.3.0.csv") # Convertion factors WG1 AR5(2013), GWP100 with climate-carbon feedback  

emissions = full_join(emissions, emission_conv) %>% 
  mutate_each_(funs(. * GWP100_cc), vars(c(2:9601)))  # Multipling all columns by emission conversion factor >>> Now all rows of "emissions" are in kg CO2 eq / M. EUR

emissions <- emissions %>% select(-GHG_item, -GWP100_cc) # The emissions extension has not intensities but total consumptions.

# From SUT folder
tot_use <- read.table(paste(path_sut, "mrUse_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
tot_use <- tot_use[,c(-1,-2,-3)]

# Get total use by product 
tot_demand <- rowSums(final_demand) + rowSums(tot_use)

TJ_per_MTOE <- 41870
TWh_per_MTOE <- 11.63

nature_input_idx <- 1:19   # number of rows for E-carrier use after removing the row headers
emission_energy_carrier_idx <- 20:70   # number of rows for E-carrier use after removing the row headers
energy_carrier_supply_idx <- 71:139   # number of rows for E-carrier use after removing the row headers
energy_carrier_use_idx <- 140:208   # number of rows for E-carrier use after removing the row headers
energy_pri_carrier_use_idx <- c(140:154, 187:188, 191:194)   # number of rows for E-carrier use after removing the row headers
energy_sec_carrier_use_idx <- energy_carrier_use_idx[!(energy_carrier_use_idx %in% energy_pri_carrier_use_idx)]

energy_use <- materials[nature_input_idx,]  # The energy extension has not intensities but total consumptions.
y <- 1/tot_demand
y[is.infinite(y)] <- 0 
energy_int <- as.matrix(energy_use) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
indirect_E_int <- energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1

# indirect emission intensity >>> Modifications 06/17/2017

emission_int <- as.matrix(emissions) %*% diag(y) # The emissions extension has not intensities but total consumptions >>>> divide by total demand >>> kg CO2 eq / M. EUR = mg CO2 eq / EUR
indirect_GHG_int <- emission_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1

# To clean up the memory
save(L_inverse, file="C:/Users/tudeschi/Documents/DLE_scripts/Outputs/Saved tables/L_inverse.Rda")
save(tot_use, file="C:/Users/tudeschi/Documents/DLE_scripts/Outputs/Saved tables/tot_use.Rda")
save(supplym, file="C:/Users/tudeschi/Documents/DLE_scripts/Outputs/Saved tables/supplym.Rda")
save(indirect_GHG_int, file="C:/Users/tudeschi/Documents/DLE_scripts/Outputs/Saved tables/indirect_GHG_int.Rda")


rm(L_inverse, tot_use, supplym, materials_reduc)
rm(val_AT_rand, val_FR_rand, val_IN_rand)
rm(final_alloc_list_FRA, final_alloc_list_FRA_all, result_FRA, result_FRA_all)
rm(eHH, all_HH_f, all_HH_fl, eHH_cap)
gc()

# Return EXIO indirect intensity in MJ/USD2007
GetSpecificEXIOSectorIntensity <- function(cty, exio_sect) {
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  cty_idx_ex <- seq(200*(cty_place-1)+1, 200*cty_place)   # 7 final demand columns per country
  ex_idx <- which(EX_catnames==exio_sect)
  
  int <- colSums(indirect_E_int[,cty_idx_ex])[ex_idx] * EXR_EUR$r   # MJ/EUR to MJ/USD2007
  
  return(int)
}

# Return EXIO indirect intensity in kg CO2 eq/USD2007
GetSpecificEXIOSectorGHGIntensity <- function(cty, exio_sect) {
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  cty_idx_ex <- seq(200*(cty_place-1)+1, 200*cty_place)   # 7 final demand columns per country
  ex_idx <- which(EX_catnames==exio_sect)
  
  int <- colSums(indirect_GHG_int[,cty_idx_ex])[ex_idx] * EXR_EUR$r   # MJ/EUR to MJ/USD2007
  
  return(int)
}
