# load library
pacman::p_load(jsonlite,
               tidyverse)

# import JSON string containing district information from the FAO SWALIM website
url <- "https://cdi.faoswalim.org/data/tdi/"

# convert JSON string to a list and process it to obtain a data frame
# containing district codes and names
id_df <- fromJSON(url, simplifyVector = TRUE) %>%
  transpose() %>%
  as.data.frame() %>%
  select(contains("district")) %>%
  gather(key = "ID", value = "district") %>%
  mutate(ID = str_remove(ID, "district\\."))

# save the processed district id
save(id_df, file = here::here("R/swalim_web_scrape/", "id_df.Rdata"))

