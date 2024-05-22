library(tidyverse)
library(ggplot2)
library(readr)

options(pillar.sigfig = 5)

#######################   airsac volumes   ########################

volume_est <- read_csv("~/student_documents\\UBC\\Research\\Malawi\\data\\sac pressure, pH series (6, 7, 8)/volume_est.csv")
print(volume_est)

# headcap <- read_csv("~/student_documents/UBC/Research/Malawi/KenyaData\\DinoLite\\pH 6_0psi sizes for volumes/head_cap.csv")
# print(headcap)

# above is long format, calcs are easier in wide
# by (23.1%(increase in pH7) - 1.3% (increase from pH5 to in vivo size)) = 21.8% (from glam shot tube expt))
volume_est.wide <- volume_est %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  #mutate(instar = NA) %>%
  group_by(indvd, sac) %>%
  mutate(sac_len = sum(length)) %>%
  mutate(sac_wid = mean(base)) %>%
  mutate(sac_aspect = sac_len / sac_wid) %>%
  print()


# volume_est.wide <- inner_join(volume_est.wide, headcap, by = c("indvd"))
# 
# #################                                               ###############
# #################    yet to do instar correlation for this      ###############
# #################                                               ###############
# volume_est.wide <- volume_est.wide %>%
#   mutate(instar = replace(instar, 
#                           head_cap > 0.88 & 
#                             head_cap < 1.1, 
#                           4)) %>%
#   mutate(instar = replace(instar, 
#                           head_cap > 0.58 & 
#                             head_cap < 0.72, 
#                           3)) %>%
#   mutate(instar = replace(instar, 
#                           head_cap > 0.3 & 
#                             head_cap < 0.5, 
#                           2))
# 
# print(volume_est.wide, n= 20)
#rm(volume_est.wide)


## volume calculation is different for tips vs. midsecs, so separating the data frames
# mid-section measures do still need tip values from other df (but at different row 
# locations than the midsec we're calculating for in the main df)

# tips data frame (this one first because midsec.df will use tip base values)
tip.df <- volume_est.wide %>%
  select(-sac_len, -sac_wid, -sac_aspect) %>%
  filter(region == "tip1" | region == "tip2") %>%
  group_by(indvd, sac, region) %>%
  mutate(volume = (((base/2)^2) * length * ((4/3) * pi)) / 2) %>%
  print()

# mid section (cylinders) data frame
midsec.df <- volume_est.wide %>%
  select(-sac_len, -sac_wid, -sac_aspect) %>%
  ungroup() %>%
  filter(region == "midsec") %>%
  mutate(tip1 = subset(tip.df, # tip1 base for mean diameter in cylinder volume calc
                       region == "tip1", 
                       select = "base")) %>% 
  mutate(tip2 = subset(tip.df, # tip2 base for mean diameter in cylinder volume calc
                       region == "tip2", 
                       select = "base")) %>%
  print()

# brute force rename (above mutates add "$base" for some reason, and this fucks up dplyr renaming/ other stuff) 
midsec.df[ , ncol(midsec.df) - 1] <- midsec.df$tip1 # rename second last column
midsec.df[ , ncol(midsec.df)] <- midsec.df$tip2 # rename last column

print(midsec.df)

midsec.df <- midsec.df %>%
  group_by(indvd, sac) %>%
  mutate(midsec_volume = (((mean(c(base, tip1, tip2)))/2)^2) * pi * length) %>%
  rename(tip1_base = tip1) %>%
  rename(tip2_base = tip2) %>%
  print()



##################     make a dataframe of indvd and sac, combining volumes
##################     with region condensed so each sac appears once,
##################     and a column each for tip volume and midsec volume

# start by using the tip df
vol_df <- tip.df %>%
  select(-base, -length) %>%
  group_by(indvd, sac) %>%
  mutate(sum_tip_vol = volume[1] + volume[2]) %>%
  select(-volume, -region) %>% # , -head_cap removed for now
  unique()

print(vol_df)

# bring in the mid section info
vol_df$midsec_vol <- midsec.df$midsec_volume

# calculate sac volumes
vol_df <- vol_df %>%
  mutate(sac_volume_uL = sum_tip_vol + midsec_vol) %>%
  group_by(indvd) %>%
  mutate(larva_air.vol_uL = 
           (mean(sum_tip_vol)*4) + # x 4, not 8, because it's the sum of both tips
           (mean(midsec_vol)*4)) ## mean tip value (one number) x 8 (8 sac tips per larva), 4 midsecs per larva

print(vol_df)
str(vol_df)


####   pull out some means for the species

# anom <- vol_df %>%
#   filter(spp == "anom") %>%
#   print()
# 
# mean(anom$sac_volume_uL)
# sd(anom$sac_volume_uL)
# 
# pallid <- vol_df %>%
#   filter(spp == "pallid") %>%
#   print()
# 
# mean(pallid$sac_volume_uL)
# sd(pallid$sac_volume_uL)



######
#  get mean fractional difference between anterior and posterior
#####

# assign ant.post category rather than just names

sac_category <- c(post1 = "post", 
                  post2 = "post", 
                  ant1 = "ant", 
                  ant2 = "ant")

print(sac_category)


vol_df <- vol_df %>% 
  mutate(ant.post = sac_category[sac])

print(vol_df)

# get mean values for ant and post volume and take the fractional difference
estimate_ant.diff <- vol_df %>%
  select(indvd, sac_volume_uL, ant.post) %>%
  group_by(ant.post) %>%
  mutate(ant.post_vol = mean(sac_volume_uL)) %>%
  select(-indvd, -sac_volume_uL) %>%
  unique()

print(estimate_ant.diff)  

ant.post_diff <- estimate_ant.diff$ant.post_vol[2] / estimate_ant.diff$ant.post_vol[1]

# right now, each individual has volume made of mean tip and mean midsec
## find indvds with only posteriors recorded and apply ant.post_diff to half the total volume value

where.ant <- vol_df %>%
  select(indvd, ant.post, sac_volume_uL,larva_air.vol_uL) %>%
  filter(ant.post == "ant") %>%
  print()

# Make post only data frame to modify the total volumes using the ant/post ratio
post_only <- vol_df %>%
  select(indvd, ant.post, sac_volume_uL, larva_air.vol_uL) %>%
  filter(!indvd == "larva1" 
         & !indvd == "larva2" 
         & !indvd == "larva3" 
         & !indvd == "larva4") %>%
  mutate(vol.adj = (larva_air.vol_uL / 2) + 
           ((larva_air.vol_uL / 2) * ant.post_diff)) %>% # half the vol plus the other half times diff factor
  mutate(larva_air.vol_uL = NULL) %>%
  rename(larva_air.vol_uL = vol.adj) #the same "larva_air.vol_uL" now uses the adjusted posterior values

print(post_only)
#print(vol_df)

# sub in ajdusted post only total air volumes

larvs_with_ant <- vol_df %>% # includes the posteriors of those larvae
  select(indvd, ant.post, sac_volume_uL, larva_air.vol_uL) %>%
  filter(indvd == "larva1" 
         | indvd == "larva2" 
         | indvd == "larva3" 
         | indvd == "larva4")
print(larvs_with_ant)

# total larva air volume tells you mass displaced to achieve ~ neutral buoyancy (sacs at pH 6)
vol.adj <- rbind(larvs_with_ant, post_only) %>%
  group_by(ant.post) %>% # looking for mean ant sac and mean post sac across larvae, so not grouped by larva
  mutate(mean_1sac_vol.ap = (mean(sac_volume_uL))) %>%
  mutate(sd_1sac_vol.ap = (sd(sac_volume_uL))) %>%
  print()

# a spreadsheet with pH 6 (excised) sac volumes, written to whichever directory
write_csv(vol.adj,
          "~/student_documents/UBC/Research/Malawi/data\\sac pressure, pH series (6, 7, 8)/sac_volume.csv")
