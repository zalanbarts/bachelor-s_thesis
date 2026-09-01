# ==============================================================================
# cleaners.R
# Functions to clean data and assign market segmentations
# ==============================================================================

# Assigns each currency observation to a developed (DM), emerging (EM), or other market group
assign_market_groups <- function(.data) {
  
  dm_currencies <- c(
    "Australian Dollar", "Canadian Dollar", "Danish Krone", "Euro",
    "Hong Kong Dollar", "Icelandic Krona", "Israeli Sheqel", "Japanese Yen",
    "New Zealand Dollar", "Norwegian Krone", "Singaporean Dollar",
    "South Korean Won", "Swedish Krona", "Swiss Franc", 
    "Taiwanese Dollar", "United Kingdom Pound",
    "Austrian Schilling", "Belgian Franc", "Dutch Guilder", "Finnish Markka", 
    "French Franc", "German Mark", "Greek Drachma", "Irish Pound", 
    "Italian Lira", "Portuguese Escudo", "Spanish Peseta")
  
  em_currencies <- c(
    "Argentine Peso", "Bahraini Dinar", "Brazilian Real", "Chilean Peso", 
    "Chinese Yuan Renminbi", "Colombian Peso", "Czech Koruna", "Egyptian Pound", 
    "Ghanaian Cedi", "Hungarian Forint", "Indian Rupee", "Indonesian Rupiah", 
    "Jordanian Dinar", "Kazakh Tenge", "Kenyan Shilling", "Kuwaiti Dinar", 
    "Malaysian Ringgit", "Mexican Peso", "Moroccan Dirham", "Nigerian Naira", 
    "Omani Rial", "Pakistani Rupee", "Peruvian Sol", "Philippine Peso", 
    "Polish Zloty", "Qatari Riyal", "Romanian Leu", "Russian Federation Ruble", 
    "Saudi Arabian Riyal", "Serbian Dinar", "South African Rand", 
    "Sri Lankan Rupee", "Thai Baht", "Tunisian Dinar", "Turkish Lira", 
    "Ugandan Shilling", "Ukrainian Hryvnia", "United Arab Emirates Dirham", 
    "Vietnamese Dong", "Zambian Kwacha",
    # Added Legacy EM
    "Bulgarian Lev", "Croatian Kuna", "Cyprian Pound", "Estonian Kroon", 
    "Latvian Lat", "Lithuanian Lita", "Maltese Lira", "Slovak Koruna", 
    "Slovenian Tolar")
 
  # Creates a new market_group variable based on the currency classification
  .data |> 
    dplyr::mutate(
      market_group = dplyr::case_when(
        from %in% dm_currencies ~ "DM",
        from %in% em_currencies ~ "EM",
        TRUE ~ "Other"
      )
    )
}
