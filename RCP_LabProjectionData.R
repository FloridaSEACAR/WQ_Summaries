# Script to provide the mean, sd, and years of data by station for RCP salinity data
# Stephen Durham
# 11/08/2021

library(tidyverse)
library(data.table)
library(here)

#load the data
cont <- fread(here::here("Salinity and Specific Conductivity-2021-Jul-26.csv"))
disc <- fread(here::here("Combined_WQ_WC_NUT_column-2021-Jul-26.csv"))
progmat <- fread("SEACARProgramMatrix_Salinity,Mean,StDev_2021-11-08.csv")

cont[, `:=` (Sal = as.numeric(Sal), SpCond = as.numeric(SpCond),
             ManagedArea = ifelse(str_detect(ManagedArea, "NERR"), paste0(str_sub(ManagedArea, 1, -6), " National Estuarine Research Reserve"), 
                                  ifelse(str_detect(ManagedArea, "NMS"), paste0(str_sub(ManagedArea, 1, -5), " National Marine Sanctuary"), 
                                         ifelse(str_detect(ManagedArea, "St. Andrews"), paste0(ManagedArea, "State Park Aquatic Preserve"),
                                                ifelse(str_detect(ManagedArea, "Coral ECA"), "Southeast Florida Coral Reef Ecosystem Conservation Area",
                                                       ifelse(str_detect(ManagedArea, "Aquatic Preserve"), ManagedArea, 
                                                              ifelse(str_detect(ManagedArea, "Ecosystem Conservation Area"), ManagedArea,
                                                                     paste0(ManagedArea, " Aquatic Preserve"))))))))]

disc[, `:=` (ActivityDepth = as.numeric(ActivityDepth),
             Total_Depth = as.numeric(Total_Depth),
             DO_mgL = as.numeric(`[DissolvedOxygen_mg/L]`),
             `[DissolvedOxygen_mg/L]` = NULL,
             Sal_ppt = as.numeric(Salinity_ppt),
             Salinity_ppt = NULL,
             Temp_C = as.numeric(`[WaterTemperature_DegreesC]`),
             `[WaterTemperature_DegreesC]` = NULL,
             pH = as.numeric(pH),
             DO_sat = as.numeric(`[DissolvedOxygenSaturation_%]`),
             `[DissolvedOxygenSaturation_%]` = NULL,
             SpCond_mScm = as.numeric(`[SpecificConductivity_mS/cm]`),
             `[SpecificConductivity_mS/cm]` = NULL,
             Turb_NTU = as.numeric(Turbidity_NTU),
             Turbidity_NTU = NULL,
             TSS_unspec = as.numeric(`[TotalSuspendedSolids`),
             `[TotalSuspendedSolids` = NULL,
             TSS_mgL = as.numeric(`TSS_mg/L]`),
             `TSS_mg/L]` = NULL,
             Chla_uncorr_ugL = as.numeric(`[Chlorophyllauncorrectedforpheophytin_ug/L]`),
             `[Chlorophyllauncorrectedforpheophytin_ug/L]` = NULL,
             Chla_corr_ugL = as.numeric(`[Chlorophyllacorrectedforpheophytin_ug/L]`),
             `[Chlorophyllacorrectedforpheophytin_ug/L]` = NULL,
             Secchi_m = as.numeric(`[SecchiDepth_m]`),
             `[SecchiDepth_m]` = NULL,
             LightExt = as.numeric(`[LightExtinctionCoefficient_m^-1]`),
             `[LightExtinctionCoefficient_m^-1]` = NULL,
             CDOM_unspec = as.numeric(`[Coloreddissolvedorganicmatter`),
             `[Coloreddissolvedorganicmatter` = NULL,
             CDOM_PCU = as.numeric(`CDOM_PCU]`),
             `CDOM_PCU]` = NULL,
             FDOM_unspec = as.numeric(`[Fluorescentdissolvedorganicmatter`),
             `[Fluorescentdissolvedorganicmatter` = NULL,
             FDOM_QSE = as.numeric(`FDOM_QSE]`),
             `FDOM_QSE]` = NULL,
             TN_mgL = as.numeric(`[TotalNitrogen_mg/L]`),
             `[TotalNitrogen_mg/L]` = NULL,
             TKN_mgL = as.numeric(`[TotalKjeldahlNitrogenTKN_mg/L]`),
             `[TotalKjeldahlNitrogenTKN_mg/L]` = NULL,
             NO23_filtered_mgL = as.numeric(`[NO2+3Filtered_mg/L]`),
             `[NO2+3Filtered_mg/L]` = NULL,
             NH4_filtered_mgL = as.numeric(`[NH4Filtered_mg/L]`),
             `[NH4Filtered_mg/L]` = NULL,
             TP_mgL = as.numeric(`[TotalPhosphorus_mg/L]`),
             `[TotalPhosphorus_mg/L]` = NULL,
             PO4_filtered_mgL = as.numeric(`[PO4Filtered_mg/L]`),
             `[PO4Filtered_mg/L]` = NULL)]

progmat <- separate_rows(progmat,`Managed Area`, sep = ",")
setDT(progmat)
# progmat <- progmat[, `Managed Area` := ifelse(stringr::str_detect(`Managed Area`, "NERR"), paste0(str_sub(`Managed Area`, 1, -6), " National Estuarine Research Reserve"), 
#                                                              ifelse(stringr::str_detect(`Managed Area`, "NMS"), paste0(str_sub(`Managed Area`, 1, -5), " National Marine Sanctuary"), paste0(`Managed Area`, " Aquatic Preserve")))]

progmat[, `Managed Area` := ifelse(str_detect(`Managed Area`, "NERR"), paste0(str_sub(`Managed Area`, 1, -6), " National Estuarine Research Reserve"), 
                                   ifelse(str_detect(`Managed Area`, "NMS"), paste0(str_sub(`Managed Area`, 1, -5), " National Marine Sanctuary"), 
                                          ifelse(str_detect(`Managed Area`, "St. Andrews"), paste0(`Managed Area`, "State Park Aquatic Preserve"),
                                                 ifelse(str_detect(`Managed Area`, "Coral ECA"), "Southeast Florida Coral Reef Ecosystem Conservation Area",
                                                        ifelse(str_detect(`Managed Area`, "Aquatic Preserve"), `Managed Area`, 
                                                               ifelse(str_detect(`Managed Area`, "Ecosystem Conservation Area"), `Managed Area`,
                                                                      paste0(`Managed Area`, " Aquatic Preserve")))))))]

#Eliminate leading spaces from some managed area names
progmat[, `Managed Area` := ifelse(str_detect(`Managed Area`, "^ "), str_sub(`Managed Area`, 2), `Managed Area`)]

#filter for the managed areas we want
cont_rcp <- cont[ProgramID %in% progmat$ID & ManagedArea %in% progmat$`Managed Area`, ]
disc_rcp <- disc[ProgramID %in% progmat$ID & ManagedAreaName %in% progmat$`Managed Area`, ]

#Cheryl said to hold off on including the continuous stations for now, so I just proceed with the discrete data below.
cont_rcp[, `:=` (office = ManagedArea, 
                 station = ProgramLocationID,
                 nyears = length(unique(Year)),
                 mean_sal = mean(Sal, na.rm = TRUE),
                 sd_sal = sd(Sal, na.rm = TRUE),
                 mean_SpCond = mean(SpCond, na.rm = TRUE),
                 sd_SpCond = sd(SpCond, na.rm = TRUE)), by = c("ProgramID", "ProgramName", "ProgramLocationID", "ManagedArea", "Region")]

cont_rcp_sum <- unique(cont_sal_rcp[, .(ProgramID, ProgramName, Region, office, station, nyears, mean_sal, sd_sal)])


disc_rcp[, `:=` (office = ManagedAreaName, 
                 station = ProgramLocationID,
                 nyears = length(unique(Year)),
                 startyear = min(Year),
                 mean_DO_mgL = mean(DO_mgL, na.rm = TRUE), sd_DO_mgL = sd(DO_mgL, na.rm = TRUE),
                 mean_Sal_ppt = mean(Sal_ppt, na.rm = TRUE), sd_Sal_ppt = sd(Sal_ppt, na.rm = TRUE),
                 mean_Temp_C = mean(Temp_C, na.rm = TRUE), sd_Temp_C = sd(Temp_C, na.rm = TRUE),
                 mean_DO_sat = mean(DO_sat, na.rm = TRUE), sd_DO_sat = sd(DO_sat, na.rm = TRUE),
                 mean_SpCond_mScm = mean(SpCond_mScm, na.rm = TRUE), sd_SpCond_mScm = sd(SpCond_mScm, na.rm = TRUE),
                 mean_Turb_NTU = mean(Turb_NTU, na.rm = TRUE), sd_Turb_NTU = sd(Turb_NTU, na.rm = TRUE),
                 mean_TSS_unspec = mean(TSS_unspec, na.rm = TRUE), sd_TSS_unspec = sd(TSS_unspec, na.rm = TRUE),
                 mean_TSS_mgL = mean(TSS_mgL, na.rm = TRUE), sd_TSS_mgL = sd(TSS_mgL, na.rm = TRUE),
                 mean_Chla_uncorr_ugL = mean(Chla_uncorr_ugL, na.rm = TRUE), sd_Chla_uncorr_ugL = sd(Chla_uncorr_ugL, na.rm = TRUE),
                 mean_Chla_corr_ugL = mean(Chla_corr_ugL, na.rm = TRUE), sd_Chla_corr_ugL = sd(Chla_corr_ugL, na.rm = TRUE),
                 mean_Secchi_m = mean(Secchi_m, na.rm = TRUE), sd_Secchi_m = sd(Secchi_m, na.rm = TRUE),
                 mean_LightExt = mean(LightExt, na.rm = TRUE), sd_LightExt = sd(LightExt, na.rm = TRUE),
                 mean_CDOM_unspec = mean(CDOM_unspec, na.rm = TRUE), sd_CDOM_unspec = sd(CDOM_unspec, na.rm = TRUE),
                 mean_CDOM_PCU = mean(CDOM_PCU, na.rm = TRUE), sd_CDOM_PCU = sd(CDOM_PCU, na.rm = TRUE),
                 mean_FDOM_unspec = mean(FDOM_unspec, na.rm = TRUE), sd_FDOM_unspec = sd(FDOM_unspec, na.rm = TRUE),
                 mean_FDOM_QSE = mean(FDOM_QSE, na.rm = TRUE), sd_FDOM_QSE = sd(FDOM_QSE, na.rm = TRUE),
                 mean_TN_mgL = mean(TN_mgL, na.rm = TRUE), sd_TN_mgL = sd(TN_mgL, na.rm = TRUE),
                 mean_TKN_mgL = mean(TKN_mgL, na.rm = TRUE), sd_TKN_mgL = sd(TKN_mgL, na.rm = TRUE),
                 mean_NO23_filtered_mgL = mean(NO23_filtered_mgL, na.rm = TRUE), sd_NO23_filtered_mgL = sd(NO23_filtered_mgL, na.rm = TRUE),
                 mean_NH4_filtered_mgL = mean(NH4_filtered_mgL, na.rm = TRUE), sd_NH4_filtered_mgL = sd(NH4_filtered_mgL, na.rm = TRUE),
                 mean_TP_mgL = mean(TP_mgL, na.rm = TRUE), sd_TP_mgL = sd(TP_mgL, na.rm = TRUE),
                 mean_PO4_filtered_mgL = mean(PO4_filtered_mgL, na.rm = TRUE), sd_PO4_filtered_mgL = sd(PO4_filtered_mgL, na.rm = TRUE)
                 ), by = c("ProgramID", "ProgramName", "ProgramLocationID", "ManagedAreaName", "Region")]

disc_rcp_sum <- unique(disc_rcp[, .(ProgramID, ProgramName, Region, office, station, nyears, startyear, mean_DO_mgL, sd_DO_mgL, 
                                    mean_Sal_ppt, sd_Sal_ppt, mean_Temp_C, sd_Temp_C, mean_DO_sat, sd_DO_sat,
                                    mean_SpCond_mScm, sd_SpCond_mScm, mean_Turb_NTU, sd_Turb_NTU, 
                                    mean_TSS_unspec, sd_TSS_unspec, mean_TSS_mgL, sd_TSS_mgL, 
                                    mean_Chla_uncorr_ugL, sd_Chla_uncorr_ugL, mean_Chla_corr_ugL, sd_Chla_corr_ugL,
                                    mean_Secchi_m, sd_Secchi_m, mean_LightExt, sd_LightExt, 
                                    mean_CDOM_unspec, sd_CDOM_unspec, mean_CDOM_PCU, sd_CDOM_PCU, mean_FDOM_unspec, 
                                    sd_FDOM_unspec, mean_FDOM_QSE, sd_FDOM_QSE, mean_TN_mgL, sd_TN_mgL, 
                                    mean_TKN_mgL, sd_TKN_mgL, mean_NO23_filtered_mgL, sd_NO23_filtered_mgL, 
                                    mean_NH4_filtered_mgL, sd_NH4_filtered_mgL, mean_TP_mgL, sd_TP_mgL,
                                    mean_PO4_filtered_mgL, sd_PO4_filtered_mgL)])

#idvars <- c("ProgramID", "ProgramName", "Region", "office", "station", "nyears", "startyear")
# measurevars <- c("mean_DO_mgL", "sd_DO_mgL", "mean_Sal_ppt", "sd_Sal_ppt", "mean_Temp_C", "sd_Temp_C", 
#                  "mean_DO_sat", "sd_DO_sat", "mean_SpCond_mScm", "sd_SpCond_mScm", "mean_Turb_NTU", "sd_Turb_NTU", 
#                  "mean_TSS_unspec", "sd_TSS_unspec", "mean_TSS_mgL", "sd_TSS_mgL", "mean_Chla_uncorr_ugL", 
#                  "sd_Chla_uncorr_ugL", "mean_Chla_corr_ugL", "sd_Chla_corr_ugL", "mean_Secchi_m", "sd_Secchi_m", 
#                  "mean_LightExt", "sd_LightExt", "mean_CDOM_unspec", "sd_CDOM_unspec", "mean_CDOM_PCU", "sd_CDOM_PCU", 
#                  "mean_FDOM_unspec", "sd_FDOM_unspec", "mean_FDOM_QSE", "sd_FDOM_QSE", "mean_TN_mgL", "sd_TN_mgL", 
#                  "mean_TKN_mgL", "sd_TKN_mgL", "mean_NO23_filtered_mgL", "sd_NO23_filtered_mgL", "mean_NH4_filtered_mgL", 
#                  "sd_NH4_filtered_mgL", "mean_TP_mgL", "sd_TP_mgL", "mean_PO4_filtered_mgL", "sd_PO4_filtered_mgL")

#convert to row format by analyte
disc_rcp_sum2 <- melt(disc_rcp_sum, measure = patterns("^mean", "^sd"), variable.name = "analyte", value.name = c("analyte_mean", "analyte_sd"))

#assign names to the analyte indices
vars <- c("DO_mgL", "Sal_ppt", "Temp_C", "DO_sat", "SpCond_mScm", "Turb_NTU", "TSS_unspec", "TSS_mgL", "Chla_uncorr_ugL", 
          "Chla_corr_ugL", "Secchi_m", "LightExt", "CDOM_unspec", "CDOM_PCU", "FDOM_unspec", "FDOM_QSE", "TN_mgL", 
          "TKN_mgL", "NO23_filtered_mgL", "NH4_filtered_mgL", "TP_mgL", "PO4_filtered_mgL")
disc_rcp_sum2[, analyte := vars[analyte]]

#Calculate the median number of records per year for each station/analyte combination
setnames(disc_rcp_sum2, c("ProgramID", "station", "office", "analyte"), c("id", "st", "of", "an"))
disc_rcp_sum2[, med_records_peryr := as.numeric(median(disc_rcp[ProgramID == id & 
                                                ProgramLocationID == st &
                                                ManagedAreaName == of &
                                                !is.na(eval(as.name(an))), length(eval(as.name(an))), 
                                                by = c("Year")
                                                ][[2]])),
              by = c("id", "st", "of", "an")]

#add column for mean salinity to match formatting of Station Priority Ranking .xlsx sheet
disc_rcp_sum2[, mean_Sal_ppt := as.numeric(mean(disc_rcp[ProgramID == id & 
                                                          ProgramLocationID == st &
                                                          ManagedAreaName == of &
                                                          !is.na(Sal_ppt), Sal_ppt])),
              by = c("id", "st", "of")]

#create a column for identifying discrete vs. continuous stations
disc_rcp_sum2[, type := "discrete"]

#Fix column names and ordering
setnames(disc_rcp_sum2, c("id", "ProgramName", "Region", "st", "of", "an"), c("programid", "programname", "region", "station", "office", "analyte"))
setcolorder(disc_rcp_sum2, c("region", "office", "programid", "programname", "station", "type", "nyears", "startyear", "analyte", "med_records_peryr", "mean_Sal_ppt", "analyte_mean", "analyte_sd"))
setorder(disc_rcp_sum2, region, office, station, analyte)

#Remove NA values
disc_rcp_sum3 <- disc_rcp_sum2[!is.na(med_records_peryr), ]

#Save as .xlsx file
openxlsx::write.xlsx(disc_rcp_sum3, here::here("RCP_LabProjectionData_discrete.xlsx"), keepNA = TRUE, firstRow = TRUE, overwrite = TRUE)


