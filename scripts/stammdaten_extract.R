library(tidyverse)
library(xml2)
library(lubridate)

# read the xml data from the file
raw_xml <- read_xml("stammdaten/MDB_STAMMDATEN.XML")

# convert to a list
xml_lst <- as_list(raw_xml)

# delete first list entry (version of dataset)
xml_lst[[1]][[1]] <- NULL

# convert to tibble
stammdaten <- as_tibble(xml_lst)

# unnest all nested lists
stammdaten <- stammdaten %>% 
  unnest_wider(DOCUMENT) %>% ## unnest ID, NAMEN, BIOGRAFISCHE_ANGGABEN, WAHLPERIODEN into columns
  unnest_longer(WAHLPERIODEN) %>% ## unnest different WAHLPERIODEN into rows
  unnest_wider(WAHLPERIODEN) %>% ## unnest data in WAHLPERIODEN into columns
  unnest_longer(INSTITUTIONEN) %>% ## unnest different INSTITUTIONEN in WAHLPERIODEN into columns
  unnest_wider(INSTITUTIONEN) %>% ## unnest data in INSTITUTIONEN into columns
  unnest_wider(BIOGRAFISCHE_ANGABEN) %>% ## unest data in BIOGRAFISCHE_ANGABEN into columns
  unnest_longer(NAMEN) %>% ## unnest different NAMEN into rows
  unnest_wider(NAMEN) %>% ## unnest data in NAMEN into columns
  unnest(cols = names(.)) %>% ## 2x unnest over all columns to extract actual data from nested lists
  unnest(cols = names(.))

# delete unneeded columns
stammdaten <- stammdaten %>% 
  select(-c(ANREDE_TITEL, ORTSZUSATZ, NAMEN_id, GEBURTSORT, BERUF, VITA_KURZ,
           WKR_NUMMER, WKR_LAND, MANDATSART, INSTITUTIONEN_id, LISTE, WKR_NAME, WAHLPERIODEN_id,
           FKTINS_VON, FKTINS_BIS, FAMILIENSTAND, RELIGION, GEBURTSLAND, ADEL, STERBEDATUM,
           MDBWP_VON, MDBWP_BIS, MDBINS_VON, MDBINS_BIS, FKT_LANG, PARTEI_KURZ))

# change column types
stammdaten <- stammdaten %>% 
  mutate(across(c(ID, WP), as.integer)) ## transform to int

# keep entries for MdBs
stammdaten <- stammdaten %>% 
  filter(INSART_LANG == "Fraktion/Gruppe") %>% 
  select(-c(INSART_LANG))

# only keep "Fraktion" a MdB started in a leglislature period
stammdaten <- stammdaten %>% 
  group_by(ID, HISTORIE_VON, HISTORIE_BIS, WP) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# only keep the most recent History for NAME
stammdaten <- stammdaten %>%
  group_by(ID, WP) %>% 
  filter(row_number() == n()) %>% 
  ungroup()

# check if duplicate entries remain
stammdaten %>% 
  group_by(ID, WP) %>% 
  filter(n() > 1)
# everything looks ok

# check for number of entries by WP
stammdaten %>% 
  group_by(WP) %>% 
  summarise(mdbs = n())

# numbers are correct for 17-20 WP
## compared to wikipedia pages
## include MdBs that dropped out
## incorrect for WP 15 + 16 and maybe earlier ones, why?
### but wikipedia pages for 15/16 show very few droputs, may be incorrect...


# clean up
##remove unneeded vars
stammdaten <- stammdaten %>% 
  select(-c(HISTORIE_VON, HISTORIE_BIS, ID))

## rename vars
stammdaten <- stammdaten %>% 
  rename(nachname = NACHNAME,
         vorname = VORNAME,
         Titel = AKAD_TITEL,
         praefix = PRAEFIX,
         geb = GEBURTSDATUM,
         geschlecht = GESCHLECHT,
         wp = WP,
         fraktion_lang = INS_LANG)

# reduce to data for 17-19 WP
stammdaten_17_19 <- stammdaten %>% 
  filter(wp >= 17 & wp <= 19)

# compute age at start of legislature period

stammdaten_17_19 <- stammdaten_17_19 %>% 
  mutate(alter = case_when(
    wp == 17 ~ interval(dmy(geb), dmy("27.10.2009")) / duration(num = 1, units = "years"),
    wp == 18 ~ interval(dmy(geb), dmy("22.10.2013")) / duration(num = 1, units = "years"),
    wp == 19 ~ interval(dmy(geb), dmy("24.10.2017")) / duration(num = 1, units = "years")
  ),
  alter = as.integer(floor(alter))
  )

  ## 17: 27. Oktober 2009
  ## 18: 22. Oktober 2013
  ## 19: 24. Oktober 2017

# recode Fraktion at start of WP
table(stammdaten_17_19$fraktion_lang)

stammdaten_17_19 <- stammdaten_17_19 %>% 
  mutate(fraktion = case_when(
    fraktion_lang == "Alternative für Deutschland" ~ "AfD",
    fraktion_lang == "Fraktion Bündnis 90/Die Grünen" ~ "Die Grünen",
    fraktion_lang == "Fraktion der Christlich Demokratischen Union/Christlich - Sozialen Union" ~ "Union",
    fraktion_lang == "Fraktion der Freien Demokratischen Partei" ~ "FDP",
    fraktion_lang == "Fraktion der Sozialdemokratischen Partei Deutschlands" ~ "SPD",
    fraktion_lang == "Fraktion DIE LINKE." ~ "DIE LINKE.",
    fraktion_lang == "Fraktionslos" ~ "Flos")
  ) %>% 
  select(-fraktion_lang)

# reorder columns
stammdaten_17_19 <- stammdaten_17_19 %>% 
  relocate(wp, nachname, vorname, praefix, geschlecht, alter, geb, Titel)

# arrange by WP in name
stammdaten_17_19 <- stammdaten_17_19 %>% 
  arrange(nachname, wp)

# save the data for further use
save(stammdaten_17_19, file = "stammdaten_17_19.RData")
