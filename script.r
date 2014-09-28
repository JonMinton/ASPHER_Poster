# Creating the series of images for use in the ASPHER poster

# Jon Minton
# 28 September 2014

# 1)  clear the workspace

rm(list=ls())

# 2) load a function that makes installing and loading R packages a bit 
# easier

source("scripts/LoadPackages.r")

# 3) use this function

RequiredPackages(
  c(
    "plyr",
    "reshape2",
    "lattice",
    "ggplot2",
    "stringr",
    "car",
    "RColorBrewer"
  )
)

######################################################################################
# SOURCE DATA

# 4) load human mortality database (HMD) data on population counts and death counts
# in the 'tidy data' format suggested by Hadley Wickham 
counts <- read.csv("data/counts.csv")
# (For the code used to convert the existing files to the 'tidy' please contact me)

# 5) load a file which shows which of the HMD countries are part of Europe

country_codes <- read.csv("Data/country_codes__new.csv", stringsAsFactors=F)

europe_codes <- country_codes$short[country_codes$europe==1]
######################################################################################
# DERIVED DATA


# What is the first and last year observed for each county?

ddply(counts, .(country), summarise, first_year=min(year), last_year=max(year))


# Now to decide which countries to keep in

countries_to_keep <- c(
  "AUT",  "BEL",    "BGR",  "BLR",  
  "CHE",  "CZE",    "DEUTE","DEUTNP", 
  "DEUTW",  "DNK",    "ESP",    "EST",    
  "FIN",    "FRATNP" ,  "GBR_NIR", "GBR_SCO",
  "GBRTENW","HUN",  "IRL","ITA",
  "LTU","LUX",  "LVA",  "NLD",
  "NOR",  "POL",  "PRT",  "RUS",
  "SVK",  "SVN",  "SWE",  "UKR"
  )

counts_s <- subset(
  counts,
  subset=country %in% countries_to_keep
  )

# to do : 
#  - combine east and west germany from before 1990 to create germany then


10   DEUTE       1956      2011
11  DEUTNP       1990      2011
12   DEUTW       1956      2011

tmp <- ddply(
  counts_s,
  .(year, age, sex),
  summarise,
  death_count=death_count[country=="DEUTE"] + death_count[country=="DEUTW"],
  population_count = population_count[country=="DEUTE"] + population_count[country=="DEUTW"]
  )

tmp <- data.frame(
  country="Germany",
  tmp
  )

counts_s <- rbind(counts_s, tmp)
rm(tmp)
counts_s <- subset(counts_s, subset=(country!="DEUTE") & (country!="DEUTW"))

counts_s <- merge(
  x=counts_s,
  y=country_codes, by.x="country", by.y="short", all.x=T
  )

counts_s$country <- NULL
counts_s$europe <- NULL
counts_s <- rename(counts_s, c("long"= "country"))

counts_s <- subset(counts_s, subset=sex!="total")
counts_s <- subset(counts_s, year >= 1950)

rates <- mutate(counts_s, death_rate=death_count / population_count)

# I want all european countries from 

# 7) aggregate up count data from all available European nations
counts_eu_all <- ddply(
  counts_eu,
  .(sex, year, age),
  summarise,
  n_countries=length(death_count),
  death_count=sum(death_count),
  population_count=sum(population_count)
)


# 8) want to produce a simple summary of this
counts_summaries <- ddply(
  tmp <- subset(counts_eu_all, subset=sex=="total"),
  .(year),
  summarise,
  n_countries=median(n_countries),
  population_count=sum(population_count)
)

# When was the earliest country's data available?
country_by_earliest_year <- ddply(counts_eu, .(country), summarise, earliest=min(year))
country_by_earliest_year <- arrange(country_by_earliest_year, earliest)

# 9) rates for all of Europe

rates_eu_all <- mutate(counts_eu_all, death_rate=death_count/population_count)

##################################################################################################
# FIGURES

# Comments from Danny

# 
# However, more important than any of this the axis of the contour maps need to 
# be labelled on the actual diagrams and 

# 1) If the Diagram can me labelled “Probability of mortality 
# in an given year by sex and single year of age, enumerated Europe, 1751-2010” 
# that would make it much clearer. By stating ‘enumerated’ you make it clear it 
# is not all of Europe. You could add a note below the diagram saying the 
# definition of Europe expands geographically over the period. [DONE]

# 2) Y scale needs to be labelled “single year of age” with text rotated by 90 degrees, 
# [DONE]


# 3) X scale needs to be labelled “single year in which population and mortality was 
# enumerated” or something like that. 

# 4) And the scale needs to be very clearly labbled, maybe in text rotated by 90 degrees 
# say “probability of death within the year (1=100%=certain death).” 

# 5) the text which says female and male needs to be larger. 


# Figure 1: Contour plot
#lattice.options(default.theme = standard.theme(color = FALSE))

# MONOCHROME VERSION OF GRAPH, FOR PRINT
trellis.device(
  tiff(
    "figures/all.tiff",  
    height=15000, width=2000
  ),
  color = FALSE
)

g1 <- contourplot(
  death_rate ~ year * age | country  + age, 
  data=subset(rates, subset=age <=80), 
  region=T, 
  #  col.regions=rev(heat.colors(200)), 
  col.regions=rev(gray(0:199/199)),
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="single age of death",
  xlab="single year in which population and death counts were enumerated",
  cex=1.4,
  
  main=NULL)
print(g1)

dev.off()


# COLOUR VERSION OF GRAPH, FOR ONLINE VERSION OF PAPER
trellis.device(
  tiff(
    "figures/fig_01COLOUR__contour_all_europe.tiff",  
    height=1000, width=2000
  ),
  color = TRUE
)
g1 <- contourplot(
  death_rate ~ year * age | sex, 
  data=subset(rates_eu_all, subset=sex!="total" & age <=80), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="single age of death",
  xlab="single year in which population and death counts were enumerated",
  cex=1.4,
  
  main=NULL)
print(g1)

dev.off()
