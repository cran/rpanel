#     A function to access datasets

.localdatadir <- NULL

rp.datalink <- function(name, action = "retrieve filename") {
   
   datasets <- matrix(c(
      # "Ben_Nevis",               ".csv",
      #   "https://dap.ceda.ac.uk/badc/deposited2019/operation-weather-rescue/data/ben-nevis-data/ben-nevis/daily/ben_nevis_summit_daily_v1.csv",
      # "centenarians",            ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/centenarians.txt",
      "children_services",       ".zip",
        "https://www.celcis.org/application/files/9016/9340/8261/Data_and_Code_CSRR_Strand_3_-_August_2023_Update.zip",
      "CO2_Mauna_Loa",           ".csv",
        "https://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/in_situ_co2/monthly/monthly_in_situ_co2_mlo.csv",
      "CO2_ice_core",            ".txt",
        "https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3-composite-co2-2008-noaa.txt",
      "CO2_law_ice_core",        ".txt",
        "https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/law/law_co2.txt",
      "cofe_attendance_2019",    ".xlsx",
        "https://www.churchofengland.org/sites/default/files/2021-12/2020StatisticsForMission_tables.xlsx",
      "cofe_giving_2019",        ".xlsx",
        "https://www.churchofengland.org/sites/default/files/2023-03/parish-finance-statistics-2021_diocesan-totals-.xlsx",
      "cofe_deprivation_2019",   ".xlsx",
        "https://www.churchofengland.org/sites/default/files/2022-01/Parish_Census_IMD2019_Summary_Jan2022.xlsx",
      "covid19_deaths_scotland", ".csv",
      'https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdeaths-involving-coronavirus-covid-19',
        # "http://www.stats.gla.ac.uk/~adrian/data/covid19_deaths_scotland.csv",
      "covid19_tracking_UK",     ".xlsx",
        "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveyheadlineresultsuk/2023/20230324covidinfectionsurveyheadlinedataset.xlsx",
      # "DO_Clyde",                ".rda",
      #   "http://www.stats.gla.ac.uk/~adrian/data/DO_Clyde.rda",
      "global_temperature",      ".txt",
        "https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt",
      "income_distribution",     ".ods",
        "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1140100/Table_3.1a_2021.ods",
      "married_men",             ".xls",
        "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/marriagecohabitationandcivilpartnerships/datasets/marriagestatisticscohabitationandcohortanalyses/2011/cohabitationandcohortanalyses11.xls",
      "NAO",                     ".txt",
        "https://climatedataguide.ucar.edu/sites/default/files/2023-07/nao_station_djfm.txt",
      # "paste",                   ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/paste.txt",
      # "pollution",               ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/pollution.txt",
      # "reading",                 ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/reading.txt",
      # "rds",                     ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/rds.txt",
      # "scottish_referendum",     ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/scottish_referendum.txt",
      "sea_surface_temperature", ".nc",
        "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/202306/oisst-avhrr-v02r01.20230618.nc",
      # "trout",                   ".txt",
      #   "http://www.stats.gla.ac.uk/~adrian/data/trout.txt",
      "UN_demography",           ".xlsx",
        "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
      "UN_country_codes",   ".xlsx",
        "https://population.un.org/wpp/assets/Excel%20Files/4_Metadata/WPP2024_F01_LOCATIONS.xlsx"
      # "UK_parliamentary_boundaries_2024", ".zip",
        # "https://geoportal.statistics.gov.uk/datasets/ons::westminster-parliamentary-constituencies-july-2024-boundaries-uk-buc-2.zip"
   ), ncol = 3, byrow = TRUE)
   datasets <- as.data.frame(datasets)
   names(datasets) <- c("name", "type", "remote_file")
   localfiles <- paste(.localdatadir, "/",
                       datasets[ , 1], datasets[ , 2], sep = "")

   # Function called with no arguments
   if (missing(name) & missing(action)) {
      dsets <- datasets
      nchr  <- 40
      ind   <- which(nchar(dsets$remote_file) > nchr)
      dsets$remote_file <- substr(dsets$remote_file, 1, nchr)
      dsets$remote_file[ind] <- paste(dsets$remote_file, "...", sep = "")
      print(dsets)
      if (is.null(.localdatadir))
         cat("A local directory has not been specified.\n")
      else
         cat("The local directory is:", .localdatadir, "\n")
      return(invisible(datasets))
   }
   
   # Check the modes of the arguments
   # but allow the local directory to be set back to NULL
   if (!(is.null(name) & action == "set local directory")) {
      if (!is.character(name)) stop("name must have mode character.")
      if (!is.character(action)) stop("action must have mode character.")
   }
   
   if (action == "set local directory") {
      if (!is.null(name)) {
         if (file.exists(name) && !dir.exists(name))
            stop("a file (not directory) with this name already exists.")
         if (!dir.exists(name)) dir.create(name)
      }
      env <- asNamespace("rpanel")
      # env <- globalenv()
      assign(".localdatadir", name, envir = env)
      locdir <- if (is.null(.localdatadir)) "NULL" else .localdatadir
      cat("Local data directory changed to:", locdir, "\n")
      return(invisible(.localdatadir))
   }
   
   if (action == "download") {
      
      # Remove names which don't match any dataset
      name.match <- if (name == "all") 1:nrow(datasets)
                    else match(name, datasets$name)
      ind <- which(is.na(name.match))
      if (length(ind) > 0) {
         cat("There are no datasets matching the following names:",
             name.match[ind], "\n")
         name.match <- name.match[-ind]
      }
      if (length(name.match) == 0) stop("no match found with dataset names.")
      
      # download to the local directory if this has been specified
      if (is.null(.localdatadir)) {
         cat("A local directory has not been specified. No action taken.\n")
         return(invisible())
      }
      else {
         for (i in name.match) {
            message("Downloading to", localfiles[i], "...")
            return_code <- suppressWarnings(try(utils::download.file(datasets[i, 3],
                                       localfiles[i], quiet = TRUE), silent = TRUE))
            if (inherits(return_code, 'try-error'))
               message(paste(datasets[name.match, 3], "could not be reached."))
         }
         return(invisible(localfiles[name.match]))
      }
   }
   
   if (action == "retrieve filename") {
      if (length(name) > 1)
         stop("to retrieve a filename, name must be of length 1.")
      match.name <- match(name, datasets$name)
      ind <- which(is.na(match.name))
      if (is.na(match.name))
         stop("there is no dataset matching this name.")
      else {
         # return the filename in the local directory, if the file is there
         if (file.exists(localfiles[match.name]))
               return(localfiles[match.name])
         # otherwise download to a temporary file
         else {
            destfile <- tempfile()
            md <- if (substr(datasets[match.name, 2], 2, 4) %in% c("xls", "ods", "zip", "rda"))
                     'wb' else 'w'
            return_code <- suppressWarnings(try(utils::download.file(datasets[match.name, 3],
                                   destfile, mode = md, quiet = TRUE), silent = TRUE))
            if (inherits(return_code, 'try-error'))
               message(paste(datasets[match.name, 3], "could not be reached."))
            else
               return(destfile)
         }
      }
   }
}
