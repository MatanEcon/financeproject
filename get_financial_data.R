# Enhanced index list with more countries and ISO3 codes
indices <- fread('
country,iso3,ticker,exchange
United States,USA,^GSPC,Yahoo
Canada,CAN,^GSPTSE,Yahoo
United Kingdom,GBR,^FTSE,Yahoo
Germany,DEU,^GDAXI,Yahoo
France,FRA,^FCHI,Yahoo
Italy,ITA,FTSEMIB.MI,Yahoo
Spain,ESP,^IBEX,Yahoo
Switzerland,CHE,^SSMI,Yahoo
Japan,JPN,^N225,Yahoo
China,CHN,000001.SS,Yahoo
Hong Kong,HKG,^HSI,Yahoo
India,IND,^NSEI,Yahoo
South Korea,KOR,^KS11,Yahoo
Australia,AUS,^AXJO,Yahoo
Brazil,BRA,^BVSP,Yahoo
Mexico,MEX,^MXX,Yahoo
South Africa,ZAF,^JN0U.JO,Yahoo
Russia,RUS,IMOEX.ME,Yahoo
Saudi Arabia,SAU,TASI.SR,Yahoo
Netherlands,NLD,^AEX,Yahoo
Sweden,SWE,^OMX,Yahoo
Norway,NOR,^OSEAX,Yahoo
Finland,FIN,^OMXH25,Yahoo
Denmark,DNK,^OMXC25,Yahoo
Portugal,PRT,^PSI20.LS,Yahoo
Poland,POL,^WIG20,Yahoo
Greece,GRC,^ATG,Yahoo
Turkey,TUR,XU100.IS,Yahoo
Indonesia,IDN,^JKSE,Yahoo
Malaysia,MYS,^KLSE,Yahoo
Thailand,THA,^SET.BK,Yahoo
Philippines,PHL,^PSEI.PS,Yahoo
Singapore,SGP,^STI,Yahoo
Taiwan,TWN,^TWII,Yahoo
Vietnam,VNM,^VNINDEX,Yahoo
Argentina,ARG,^MERV,Yahoo
Chile,CHL,^IPSA,Yahoo
Colombia,COL,COLCAP.CO,Yahoo
UAE,ARE,DFMGI.DI,Yahoo
Qatar,QAT,DSM.QA,Yahoo
Egypt,EGY,EGX30.CA,Yahoo
Israel,ISR,^TA125.TA,Yahoo
New Zealand,NZL,^NZ50,Yahoo
Austria,AUT,^ATX,Yahoo
Belgium,BEL,^BFX,Yahoo
Czech Republic,CZE,^PX,Yahoo
Hungary,HUN,^BUX,Yahoo
Ireland,IRL,^ISEQ,Yahoo
Luxembourg,LUX,^LUXXX,Yahoo
Slovenia,SVN,^SBITOP,Yahoo
Croatia,HRV,CROBEX.CR,Yahoo
Romania,ROU,^BET,Yahoo
Bulgaria,BGR,SOFIX.SO,Yahoo
Lithuania,LTU,^OMXV,Yahoo
Latvia,LVA,^OMXR,Yahoo
Estonia,EST,^OMXT,Yahoo
Iceland,ISL,^ICEX,Yahoo
Serbia,SRB,^BELEX15,Yahoo
Ukraine,UKR,^PFTS,Yahoo
Kazakhstan,KAZ,^KASE,Yahoo
Morocco,MAR,^MASI,Yahoo
Tunisia,TUN,^TUNINDEX,Yahoo
Nigeria,NGA,^NGSEINDX,Yahoo
Kenya,KEN,^NSEASI,Yahoo
Ghana,GHA,^GSI,Yahoo
Botswana,BWA,^BSE,Yahoo
Mauritius,MUS,^SEMDEX,Yahoo
Jamaica,JAM,^JMSMX,Yahoo
Barbados,BRB,^BSE,Yahoo
Trinidad and Tobago,TTO,^TTSE,Yahoo
Bahrain,BHR,^BAX,Yahoo
Jordan,JOR,^JOSMGNFF,Yahoo
Kuwait,KWT,^BKP,Yahoo
Oman,OMN,^MSI,Yahoo
Lebanon,LBN,^BLOM,Yahoo
Cyprus,CYP,^CYSMMAPA,Yahoo
Malta,MLT,^MALTEX,Yahoo
Sri Lanka,LKA,^CSEALL,Yahoo
Bangladesh,BGD,^DSEX,Yahoo
Pakistan,PAK,^KSE100,Yahoo
Nepal,NPL,^NEPSE,Yahoo
Mongolia,MNG,^MSE,Yahoo
Uzbekistan,UZB,^UZB,Yahoo
Georgia,GEO,^GESI,Yahoo
Armenia,ARM,^AMX,Yahoo
Azerbaijan,AZE,^AZEX,Yahoo
Peru,PER,^SPBLPGPT,Yahoo
Uruguay,URY,^URUIPC,Yahoo
Venezuela,VEN,^IBC,Yahoo
Ecuador,ECU,^ECUINDEX,Yahoo
Paraguay,PRY,^IGPA,Yahoo
Bolivia,BOL,^IGB,Yahoo
Costa Rica,CRI,^CRSMBCT,Yahoo
Panama,PAN,^BVLAC,Yahoo
Guatemala,GTM,^IGVL,Yahoo
Honduras,HND,^HNSE,Yahoo
El Salvador,SLV,^BVES,Yahoo
Nicaragua,NIC,^BVNSE,Yahoo
Dominican Republic,DOM,^IGBC,Yahoo
')

# Function to fetch index data using quantmod
fetch_index_data <- function(ticker, country, iso3, start_date = "1990-01-01", end_date = Sys.Date()) {
  tryCatch({
    cat(paste("Fetching", country, "..."))
    
    # Get stock data
    getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE, warnings = FALSE) -> stock_data
    
    if (is.null(stock_data) || nrow(stock_data) == 0) {
      cat(" FAILED - No data\n")
      return(NULL)
    }
    
    # Extract adjusted close prices
    adj_close <- Ad(stock_data)
    
    # Convert to data.table
    dt <- data.table(
      country = country,
      iso3 = iso3,
      date = index(adj_close),
      adj_close = as.numeric(adj_close)
    )
    
    # Remove NA values
    dt <- dt[!is.na(adj_close)]
    
    if (nrow(dt) < 252) {  # Need at least 1 year of data
      cat(" FAILED - Insufficient data\n")
      return(NULL)
    }
    
    cat(" SUCCESS\n")
    return(dt)
    
  }, error = function(e) {
    cat(paste(" FAILED -", e$message, "\n"))
    return(NULL)
  })
}

# Fetch all data with progress tracking
cat("Fetching index data...\n")
all_data_list <- list()

for (i in 1:nrow(indices)) {
  result <- fetch_index_data(
    ticker = indices$ticker[i], 
    country = indices$country[i], 
    iso3 = indices$iso3[i]
  )
  
  if (!is.null(result)) {
    all_data_list[[indices$iso3[i]]] <- result
  }
  
  # Add small delay to avoid overwhelming the API
  Sys.sleep(0.1)
}

# Combine all successful fetches
all_data <- rbindlist(all_data_list, use.names = TRUE, fill = TRUE)

cat(paste("Successfully fetched data for", length(unique(all_data$iso3)), "countries\n"))

# Calculate returns
all_data[, returns := c(NA, diff(log(adj_close))), by = iso3]
all_data <- all_data[!is.na(returns)]


# Save datasets
cat("Saving datasets...\n")
fwrite(all_data, paste0(output, "stock_indices_data.csv"))

cat("Analysis complete! Files saved:\n")
cat("- stock_indices_data.csv: Raw stock index data\n")
rm(list = )