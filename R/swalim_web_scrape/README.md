## Aim

To download climate data for Somalia from FAO SWALIM for specified dates (default range is 2015 to 2023), use the `swalim_climate()` function, which is located in the `swalim_climate.R` file. The `test_swalim_function.R` script tests this function using ID numbers imported from `id_df.Rdata` (which were scraped from FAO SWALIM using the `get_district_ids_fao.R` script). These ID numbers correspond to the districts and are appended to the base URL in order to generate a complete URL for web scraping the data.
 
