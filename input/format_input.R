USERPROFILE <- Sys.getenv("USERPROFILE")
library(data.table)

# load estimates
dir_CME_est <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Data and charts for websites/Files 2023/CME/Estimates/")
dtc <- fread(file.path(dir_CME_est, "UNIGME2023_Country_Rates_&_Deaths.csv"))
dir_CME_est_2022 <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Data and charts for websites/Files 2022/CME/Estimates/")
dtc_2022 <- fread(file.path(dir_CME_est_2022, "UNIGME2022_Country_Rates_&_Deaths.csv"))
dtc_SBR <- dtc_2022[Shortind == "SBR"]

dtc <- dtc[Year==2022.5 & Sex=="Total"]
dtc_SBR <- dtc_SBR[Year == 2021.5]

dtc <- rbindlist(list(dtc_SBR, dtc))

dtc <- dtc[!grepl("deaths", Shortind, ignore.case = TRUE)]
setnames(dtc, "ISO.Code", "ISO3Code")
dtc[, paste(range(round(Median)), collapse = "-"), by = Shortind]
dtc[, median(Median), by = Shortind]

fwrite(dtc, "input/IGME_2023_rate_estimates.csv")
