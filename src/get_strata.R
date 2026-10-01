# ---
#   title: "Plot Prioritization"
# author: "Shive Lab (B. Baker)"
# date: "`r Sys.Date()`"
# output: html_document
# ---
library(tidyverse)
library(here)
library(sf)
library(terra)
library(janitor)

groves <- vect(here("data/spatial_data/SEKI_groves/SEKI_groves_list.shp")) %>%
  st_as_sf()

existing_plots <-read_csv(here("data/spatial_data/all_plots_groves_env.csv"))

aspect <- vect(here("data/spatial_data/aspect_polys.shp")) %>%
  st_as_sf() %>%
  st_transform(crs(groves))

trt <- vect(here("data/spatial_data/trt/SEGI_Trtdata_05feb25.shp")) %>%
  project(crs(groves))

fire <- vect(here("data/spatial_data/cbi_all/cbi_all.shp")) %>%
  project(crs(groves))

no_fire <- read_sf(here("data/spatial_data/no_fire/no_fire.shp")) %>%
  st_transform(crs(groves))

plots_sf <- st_read(here("data/spatial_data/fuelsplots_29Jul26/fuelsplots_29Jul26.shp"))%>%
  st_transform(crs(groves))

access <- read.csv(here("data/grove_access.csv"))

#get all spatial files of same type
fire <- fire %>%
  st_as_sf()

trt <- trt %>%
  st_as_sf()

#pull fire related treatments to count as low sev fire
rx_seki <- trt %>%
  # st_as_sf() %>%
  filter(treatment == "Fire-related treatment") %>%
  # vect() %>%
  st_intersection(groves) %>%
  # st_as_sf() %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>%
  mutate(burnsev = 2,
         id = paste(grov_nm, year, rowid, sep = "_"))%>%
  rename(fire_yr = year)%>%
  select(id, fire_yr, burnsev)

no_burn <- no_fire %>%
  mutate(
    burnsev = 0,
    fire_yr = 0000,
    id = "unburned"
  )%>%
  select(id, fire_yr, burnsev)


# aspect_groves <- intersect(aspect_vect, groves_vect) %>%
#   st_as_sf()%>%
aspect_groves <- aspect %>%
  st_intersection(groves) %>%
  #ks: changed names for aspect to make briefer strata names later
  mutate(aspect = case_when(
    Id == 1 ~ "NW_E",
    Id == 2 ~ "SE_W"
  ))


# burn <- intersect(fire, groves_vect) %>%
#   st_as_sf()%>%
burn <- fire %>%
  st_intersection(groves) %>%
  mutate(fire_yr = as.numeric(fire_yr)) %>%
  select(id, fire_yr, burnsev) %>%
  bind_rows(rx_seki) %>%
  bind_rows(no_burn)


grove_area <- sum(groves$grovr_h)

##get plot treatment totals
plots_rx <- st_intersection(rx_seki, plots_sf) #trt per plot

#summarize trt in each plot
plots_rx_count <- plots_rx %>%
  st_drop_geometry() %>%
  summarise(.by = plot_id, trt_ct = n())

#years of treatments
plots_rx_yr <- plots_rx %>%
  group_by(plot_id) %>%
  slice_max(fire_yr) %>%
  rename(trt_yr = fire_yr) %>%
  left_join(plots_rx_count) %>%
  mutate(sev = "Low") %>%
  select(plot_id, trt_yr, trt_ct, sev) %>%
  st_drop_geometry()

#totals by grove
totals_summary <- existing_plots %>%
  summarise(.by = c(grove_name), total = n())

#count total
plot_total <-  sum(totals_summary$total)

#rejoin info
plots_all <- existing_plots %>%
  left_join(plots_rx_yr) %>%
  replace_na(list(trt_yr = 0000, trt_ct = 0, sev = "Unburned")) %>%
  mutate(
    fire_yr = as.numeric(fire_yr),
    burnsev = case_when(fire_yr<trt_yr ~ sev, T ~ burnsev),
    fire_yr = case_when(fire_yr<trt_yr ~ trt_yr, T ~ fire_yr),
    fire_count = fire_count+trt_ct
  )

#give strata classes
plots_classified <- plots_all %>%
  mutate(
    burnsev = case_when(burnsev == "Undetected change" ~ "Unburned",
                        T ~ burnsev),
    #ks: changed names for aspect and time_since to make briefer strata names later
    aspect = case_when(
      aspect <= 90 | aspect > 315~ "NW_E",
      T ~ "SE_W"),
    time_since = case_when(
      fire_yr == 0000 ~ "over5",
      fire_yr  >= 2020 ~ "under5",
      T ~ "over5"
    ),
    count = case_when(fire_count >= 2 ~ "many",
                      T ~ "one"),
    count_v2 = case_when(burnsev == "Unburned" ~ "none", T ~ count),
    strata_nm = paste(aspect,"-",time_since,"-",burnsev,"-",count_v2, sep = ""))

write_csv(plots_classified, here("outputs/classifiedPlacedPlots.csv"))
#
# pc = plots_classified %>%
#   filter(aspect == "SE_W" & time_since == "under5" & burnsev == "Low" &
#            count_v2 == "many")
# nrow(pc)

plot_sets <- plots_classified %>%
  mutate(count_v2 = case_when(burnsev == "Unburned" ~ "none", T ~ count)) %>%
  summarize(.by = c(aspect, burnsev, time_since, count_v2), count_plot = n()) %>%
  #ks: changed names for count_v2 to make briefer strata names later
  mutate(strata_nm = paste(aspect,"-",time_since,"-",burnsev,"-",count_v2, sep = "")) %>%
  group_by(aspect,burnsev,time_since,count_v2,strata_nm) %>%
  summarise(existing_plots = sum(count_plot))
# %>%
#   mutate(prop_plot = round(count_plot/plot_total, 2))


#get burn counts and sev
burn_count <- burn %>%
  summarize(.by = c(id, fire_yr, burnsev), geometry = st_combine(geometry)) %>%
  st_make_valid() %>%
  arrange(-fire_yr)

burn_id <- burn_count %>%
  st_drop_geometry() %>%
  rowid_to_column("rid")

fire_count <- st_intersection(burn_count) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE)

burn_classed <- fire_count %>%
  mutate(time_since = case_when(
    fire_yr == 0000 ~ "over5",
    fire_yr  >= 2020 ~ "under5",
    T ~ "over5"
  ),
  burnsev = case_when(
    burnsev == 0 ~ "Unburned",
    burnsev == 1 ~ "Unburned",
    burnsev == 2 ~ "Low",
    burnsev == 3 ~ "Moderate",
    burnsev == 4 ~ "High",
    T ~ "Unburned"
  ),
  #ks: changed names for count to make briefer strata names later
  count = case_when(n.overlaps >= 2 ~ "many",
                    T ~ "one")) %>%
  summarize(.by = c(time_since, burnsev, count), geometry = st_combine(geometry)) %>%
  st_make_valid()

##there are 28 possible combos in our actual grove data when all unburned are lumped,
groves_set1a <- burn_classed %>%
  st_intersection(aspect_groves) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>%
  mutate(count_v2 = case_when(
    burnsev == "Unburned" ~ "none", T ~ count)) %>%
  summarize(.by = c(aspect, time_since, burnsev, count_v2,grov_nm,grovr_h), geometry = st_combine(geometry)) %>%
  st_make_valid() %>%
  mutate(area_ha = round(as.numeric(st_area(.))*0.0001,3)) %>%
  mutate(strata_nm = paste(aspect,"-", time_since, "-",burnsev, "-",count_v2, sep = ""))
nrow(groves_set1a)
# # write_sf(groves_set1a, here("data/spatial_data/groves_set_full_28strata.shp"))

###########################################################################################
###########################################################################################
##get stats on strata percentages across groves and total hectares

#get a table of strata proportions (excluding groves) with total ha per strata
groves_set_seki_prop = groves_set1a %>%
  group_by(strata_nm) %>%
  summarise(total.strata.area.ha = sum(area_ha),
            strata.prop.area.seki = round(total.strata.area.ha/grove_area, 4))
groves_set_seki_prop

groves_set_seki_prop_lookup = st_drop_geometry(groves_set_seki_prop)


#make list of strata that are less than 2 ha
small = groves_set_seki_prop %>%
  filter(total.strata.area.ha<2) %>%
  mutate(poss_exclude = "park total < 2 ha") %>%
  st_drop_geometry()
small
nrow(small)

#get groves with their proportion of strata 
strata_prop_of_grvs = groves_set1a %>%
  group_by(strata_nm,grov_nm) %>%
  summarise(prop_within_grvs = area_ha/grovr_h)
strata_prop_of_grvs

strata_prop_of_grvs_lookup = st_drop_geometry(strata_prop_of_grvs)

#make sure that the "small" ones to cut aren't a big part of any small grove
strata_prop_of_grvs_small = strata_prop_of_grvs %>%
  left_join(small) %>%
  filter(poss_exclude == "park total < 2 ha")
strata_prop_of_grvs_small

#ok to exclude totals less that 2 ha

#
# ###################################################################################################################
# ###################################################################################################################
# ########## Spatial strata are done, now querying down to actual target areas and plots needed #####################
# ###################################################################################################################
# 

##read in the polys of strata so you don't have to re-run the above each time
# groves_set1a = st_read(here("data/spatial_data/groves_set_full_28strata.shp"))

##and then i am excluding any that are represented by <2 ha TOTAL, leaving 25 strata
groves_set1 <- groves_set1a %>%
  left_join(small) %>%
  filter(is.na(poss_exclude)) %>%
  select(strata_nm, grov_nm, grov_nm,area_ha)
groves_set1 

##this seems to be where shit hits the fan?
groves_classified_poly_explode = st_cast(groves_set1, "MULTIPOLYGON") %>% st_cast("POLYGON")
groves_classified_poly_explode$area_ha = round(as.numeric(st_area(groves_classified_poly_explode)*0.0001))

#see which strata might be removed entirely
sliver_removal = groves_classified_poly_explode %>%
  #filter out individual silvers <0.5 ha
  # mutate(over2 = case_when(area_ha >= 0.5 ~ "keep", 
  #                          area_ha < 0.5 ~ "remove", .default = "oopsy")) %>%
  # filter(over2 == "keep") %>%
  filter(area_ha > 0.5) %>%
  select(strata_nm, grov_nm, area_ha)
sliver_removal

avail_poly = sliver_removal %>%
  # st_cast("MULTIPOLYGON") %>%
  group_by(strata_nm) %>%
  summarise(geometry = st_union(geometry)) %>%
  left_join(groves_set_seki_prop_lookup)
avail_poly$area_ha = round(as.numeric(st_area(avail_poly)*0.0001),2)
avail_poly
# View(sliver_removal)
grove_area-sum(avail_poly$area_ha)

#write_sf(avail_poly, here("outputs/avail_polys_no2haTotal_no05haSlivers_06June2025.shp"))
#excluded by sliver size:
#for 2 ha, exclude 1893
#for 1 ha exclude 1394
#for 0.5 ha exclude 652 ha

#if use the 0.5 threshold, this shows no removal of full strata
diffs = setdiff(groves_set1$strata_nm,sliver_removal$strata_nm)
diffs
unique(groves_set1$strata_nm)
unique(groves_classified_poly$strata_nm)

# check_area = groves_set1 %>%
#   left_join(strata_prop_of_grvs_lookup) %>%
# #these are ones that would be excluded if i used the 2 ha sliver threashold
#     filter(strata_nm %in% c("NW_E-over5-Moderate-one","SE_W-over5-High-one","NW_E-over5-Moderate-many"))
# View(check_area)
# ##this shows that with the 2 ha removal:
# #NW_E-over5-Moderate-one is present mostly in Grant and Redwood meadow, both cases is on the edge and fragmented

#get area of strata per grove
options(scipen = 999)

#get number of plots per strata per grove
existing.plots.area.per.strata.grove = st_intersection(plots_sf,avail_poly) %>%
  group_by(strata_nm,grov_nm) %>%
  summarise(plots_per_strata_by_grv = length(plot_id)) %>%
  st_drop_geometry()
existing.plots.area.per.strata.grove

#get number of plots per strata ONLY
existing.plots.area.per.strata = st_intersection(plots_sf,avail_poly) %>%
  group_by(strata_nm) %>%
  summarise(existing_plots_per_strata = length(plot_id)) %>%
  st_drop_geometry()
existing.plots.area.per.strata

# existing.plots.area.per.strata = avail_poly %>%
#   left_join(existing.plots.area.per.strata1) %>%
#   mutate_if(is.numeric, ~replace(., is.na(.), 0))
# View(existing.plots.area.per.strata)

# write_sf(existing.plots.area.per.strata, here("data/spatial_data/outputs/groveset_strata_all_w_existing_plots.shp"))

#write.csv(existing.plots.area.per.strata_lookup, here("outputs/existing_plots_and_area_per_strata.csv"))


########################################################################
########################################################################
##excluding slivers and getting final plots needs
#can read in from here:

library(scales)
##get number of useful plots
final.strata.that.need.new.plots <- avail_poly %>% 
  left_join(existing.plots.area.per.strata) %>%
  mutate(existing_plots_per_strata = case_when(is.na(existing_plots_per_strata) ~ 0, .default = existing_plots_per_strata),
         area_ha = round(area_ha, 2),
         # diff.prop = prop_area - prop_plot,
         # new.prop = rescale(prop_area_seki, from = c(0,1)),
         # total.plots.needed = ceiling(501*new.prop),
         total.plots.needed = ceiling(445*strata.prop.area.seki),
         useful.plots = case_when(existing_plots_per_strata>total.plots.needed ~ total.plots.needed,
                                  T ~ existing_plots_per_strata),
         NEW.plots.needed = total.plots.needed - useful.plots) %>%
  filter(NEW.plots.needed > 0)
sum(final.strata.that.need.new.plots$NEW.plots.needed)

sum(final.strata.that.need.new.plots$NEW.plots.needed)+sum(final.strata.that.need.new.plots$useful.plots)

final.strata.that.need.new.plots_lookup = st_drop_geometry(final.strata.that.need.new.plots)
#write.csv(final.strata.that.need.new.plots_lookup, here("outputs/final.strat.that.need.new.plots_06June2025.csv"))

# write_sf(final.strata.that.need.new.plots, here("data/spatial_data/outputs/final.strata.that.need.new.plots_06June2025.shp"))

##Create new shapefile that removes "Very Difficult" access groves, and dissolve into one big shapefile
access.min = access %>% 
  mutate(grov_nm = grove) %>%
  select(grov_nm,Ease.of.Access)

#get easier groves and dissolve to one multipolygon
groves_easier = groves %>% 
  # mutate(grove = grove_nm) %>%
  left_join(access.min) %>%
  filter(Ease.of.Access != "Very Difficult") %>%
  # st_buffer(0.5) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  # st_union() %>% # unite to a geometry object
  # st_sf() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON")
groves_easier

# write_sf(groves_easier, here("data/spatial_data/outputs/groves_easier.shp"))

final.strata.that.need.new.plots_easier.1 = final.strata.that.need.new.plots %>%
  st_intersection(groves_easier) %>%
  st_collection_extract("POLYGON") %>%
  mutate(area_ha_strata_grv = round(as.numeric(st_area(.)*0.0001),3))
final.strata.that.need.new.plots_easier.1
sum(final.strata.that.need.new.plots_easier.1$NEW.plots.needed)
View(final.strata.that.need.new.plots_easier.1)

# write_sf(final.strata.that.need.new.plots_easier.1, here("data/spatial_data/outputs/final_strata_need_plots_6June2025.shp"))


# # final.strata.that.need.new.plots_easier.1.grp = final.strata.that.need.new.plots_easier.1 %>%
# #   group_by(strata_nm) %>%
# #   mutate(area_ha_strata = sum(area_ha_strata))
# # View(final.strata.that.need.new.plots_easier.1.grp)
# # # write_sf(final.strata.that.need.new.plots_easier.1, here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier_diss.shp"))
# # 
# # #create a table to join the details on plots needed per strata back
# # final.strata.that.need.new.plots_table = final.strata.that.need.new.plots %>%
# #   st_drop_geometry() %>%
# #   select(strata_nm, existing_plots, total.plots.needed, useful.plots, NEW.plots.needed)
# # 
# # final.strata.that.need.new.plots_easier_table = final.strata.that.need.new.plots_easier.1 %>%
# #   st_drop_geometry()
# # # View(final.strata.that.need.new.plots_easier_table)
# # 
# # # #join the table to the reduced strata shapefile
# # final.strata.that.need.new.plots_easier = final.strata.that.need.new.plots_easier.1  %>%
# #   # st_collection_extract(type = c("POLYGON")) %>%
# #   left_join(final.strata.that.need.new.plots_easier_table)
# # # %>%
# # #   group_by(strata_nm) %>%
# # #   summarise(across(existing_plots:NEW.plots.needed,mean))
# # final.strata.that.need.new.plots_easier$area_ha = st_area(final.strata.that.need.new.plots_easier)*0.0001
# 
# strat_only_needs = final.strata.that.need.new.plots_easier %>%
#   group_by(strata_nm) %>%
#   mutate(area)
# write_sf(final.strata.that.need.new.plots_easier, here("data/spatial_data/outputs/final.strata.that.need.new.plots_easier.shp"))
# 
# #create table version
# write.csv(final.strata.that.need.new.plots_easier_table, here("outputs/plot_needs_by_strata_2June2025_kls.csv"))
# 
# ##split by attributes to get individual shapefiles for the GRTS
# #select the column of the attribute table for the split
# unique <- unique(final.strata.that.need.new.plots_easier$strata_nm)
# 
# #create new polygons based on the unique column ID
# for (i in 1:length(unique)) {
#   tmp <- final.strata.that.need.new.plots_easier[final.strata.that.need.new.plots_easier$strata_nm == unique[i], ]
#   write_sf(tmp, here("data/spatial_data/ind_strata"), unique[i], driver="ESRI Shapefile",
#            overwrite_layer=TRUE)
# }
# 
# 
# ##this table made in "existing_plots_assessment.Rmd"
# groves_seki_summary = read.csv(here("outputs/groves_seki_summary.csv"))
# plots_sf$grove_name = plots_sf$grov_nm
# 
# #make grove summary plot, nonspatial
# groves_summary <- plots_sf %>% 
#   st_drop_geometry() %>% #remove polygon reference
#   summarize(.by = c(grove_name, project), plot_count = n()) %>% #count per type
#   pivot_wider(names_from = project, # make wider dataframe for viz
#               values_from = plot_count, 
#               values_fill = 0) %>% 
#   adorn_totals("col") %>% #calc total plots per grove
#   # adorn_totals() %>%  #calc total plots per type
#   left_join(groves_seki_summary) %>% # add grove info
#   mutate(plots_peracre_buf = round(Total/groveacres,2),
#          acres_perplot_buf = round(groveacres/Total, 0),
#          groveacres = round(groveacres,0),
#          total.existing.plots = Total) %>%
#   select(-c(2:7))
# # View(groves_summary)
# 
# hist(groves_summary$total.existing.plots)
# hist(groves_summary$acres_perplot_buf, breaks = 100)
# 
# ##rough ranking based on total plots and plot density
# groves_summary_rank = groves_summary %>%
#   mutate(total.plots.rank = 
#            case_when(total.existing.plots <10 ~ "very few plots",
#                      total.existing.plots >=10 & 
#                        total.existing.plots <30 ~ "moderate plots",
#                      total.existing.plots >=30 ~ "lotsa plots",
#                      .default = "oopsie"),
#          plot.density.rank = 
#            case_when(acres_perplot_buf <10 ~ "high density",
#                      acres_perplot_buf >=10 & 
#                        acres_perplot_buf <50 ~ "moderate density",
#                      acres_perplot_buf >=50 ~ "low density",
#                      .default = "oopsie"),
#          combo.rank = paste(total.plots.rank,"-",plot.density.rank))
# 
# # View(groves_summary_rank)  
# write.csv(groves_summary_rank,here("outputs/groves_summary_rank.csv"))
# 
# 
# 
# 
# 
