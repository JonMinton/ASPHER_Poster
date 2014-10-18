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

# countries_to_keep <- c(
#   "AUT",  "BEL",    "BGR",  "BLR",  
#   "CHE",  "CZE",    "DEUTE","DEUTNP", 
#   "DEUTW",  "DNK",    "ESP",    "EST",    
#   "FIN",    "FRATNP" ,  "GBR_NIR", "GBR_SCO",
#   "GBRTENW","HUN",  "IRL","ITA",
#   "LTU","LUX",  "LVA",  "NLD",
#   "NOR",  "POL",  "PRT",  "RUS",
#   "SVK",  "SVN",  "SWE",  "UKR"
#   )

# The UK and Ireland 

countries_to_keep <- c(
  "GBRTENW",
  "GBR_NIR",
  "GBR_SCO",
  "IRL",
  "BEL",
  "NLD"
  )

counts_s <- subset(
  counts,
  subset=country %in% countries_to_keep
  )


# 
# # to do : 
# #  - combine east and west germany from before 1990 to create germany then
# 
# 
# # 10   DEUTE       1956      2011
# # 11  DEUTNP       1990      2011
# # 12   DEUTW       1956      2011
# 
# tmp <- ddply(
#   counts_s,
#   .(year, age, sex),
#   summarise,
#   death_count=death_count[country=="DEUTE"] + death_count[country=="DEUTW"],
#   population_count = population_count[country=="DEUTE"] + population_count[country=="DEUTW"]
#   )
# 
# tmp <- data.frame(
#   country="Germany",
#   tmp
#   )
# 
# counts_s <- rbind(counts_s, tmp)
# rm(tmp)
# counts_s <- subset(counts_s, subset=(country!="DEUTE") & (country!="DEUTW"))
# 
# counts_s <- merge(
#   x=counts_s,
#   y=country_codes, by.x="country", by.y="short", all.x=T
#   )
# 
# counts_s$country <- NULL
# counts_s$europe <- NULL
# counts_s <- rename(counts_s, c("long"= "country"))

counts_s <- subset(counts_s, subset=sex!="total")
counts_s <- subset(counts_s, year >= 1930)
counts_s <- subset(counts_s, subset=age <=80)

rates <- mutate(counts_s, death_rate=death_count / population_count)


rates$country <- revalue(
  rates$country,
  replace=c(
    "GBR_NIR"="Northern Ireland",
    "GBR_SCO"="Scotland",
    "GBRTENW"="England & Wales",
    "IRL"="Ireland",
    "BEL"="Belgium",
    "NLD"="The Netherlands"
    )
  )




# I want all european countries from 

# 
# # For each of eight countries 
# 
# Scotland
# England & Wales
# Northern Ireland
# Ireland
# Netherlands
# Belgium
# Germany
# France
# Denmark
# Norway


# What I want
# For Each of Ireland, Northern Ireland
# England & Wales
# Scotland
# France
# Holland
# Belgium



# From 1930 to 2010
# for ages 0 to 80

# Using a common scale
# Plot males and females side by side



# Range should be from 0 to 0.2

lims <- seq(0,0.2, length=50)


# 
# # Figure 1: Contour plot
# #lattice.options(default.theme = standard.theme(color = FALSE))
# 
# # MONOCHROME VERSION OF GRAPH, FOR PRINT
  tiff(
    "figures/male_ireland.tiff",  
    height=1000, width=1000
  )

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Ireland" & sex=="male"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  at=lims,
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  colorkey=NULL,
  main=NULL
  )
print(g1)
# 
dev.off()
# 

tiff(
  "figures/female_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Ireland" & sex=="female"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()

# # Figure 1: Contour plot
# #lattice.options(default.theme = standard.theme(color = FALSE))
# 
# # MONOCHROME VERSION OF GRAPH, FOR PRINT
tiff(
  "figures/male_northern_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Northern Ireland" & sex=="male"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()
# 
tiff(
  "figures/female_northern_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Northern Ireland" & sex=="female"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()
# 
tiff(
  "figures/female_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Ireland" & sex=="female"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
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
  data=rates, 
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

# # Mo
