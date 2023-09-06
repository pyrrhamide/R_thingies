
# Numbering rows within groups in a dataframe
# https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame

set.seed(100)

df <- data.frame(
  cat = c(rep("aaa", 5), rep("bbb", 5), rep("ccc", 5)), 
  val = runif(15))             

df <- df[order(df$cat, df$val), ]  
df  

# how to add number within each group ?
# several ways!

# 1) ave()
ave(df$val, df$cat, FUN = seq_along)

# 2) plyr::ddply()
plyr::ddply(df, 'cat', plyr::mutate, id = seq_along(val))

# 3) dplyr::row_number()
library(dplyr)
df |> mutate(id = row_number(), .by = cat)

# 4) data.table::rowid() et autre
library(data.table)
dt <- as.data.table(df)

dt[, .(id = seq_len(.N)), by = cat]
dt[, .(id = rowid(cat))]
