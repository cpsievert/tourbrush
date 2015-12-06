library(pitchRx)
library(dplyr)

db <- src_sqlite("~/pitchfx/pitchRx.sqlite3")

y <- tbl(db, "atbat") %>%
  filter(pitcher_name == "Yu Darvish")

yu <- tbl(db, "pitch") %>%
  filter(pitch_type %in% c("CU", "FF", "FS", "FT", "SL"),
         !is.na(px), !is.na(pz)) %>%
  inner_join(y, by = c("num", "gameday_link")) %>%
  collect()

devtools::use_data(yu)
