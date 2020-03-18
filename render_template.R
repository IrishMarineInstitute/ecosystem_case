rmarkdown::render("Template_example.Rmd",params = list(
####################0.Select area of interest####
#################################################
xmin=-11,
xmax=-9,
ymin=50,
ymax=51.5,


#######################################
##1.Seabed habitat user settings  ##
#######################################

####Select logical flag for seabed attributes#
#################################################
Biozone = T,
Substrate=T,
Folk_5 = T,
EUNIS=T,
MSFD_BBHT = T,
#save_table_as=T
#save_table_as="csv",
save_table_as="xlsx",
map = T,

#######################################
####2. Copernicus data settings##
#######################################

# Select logical flag for Copernicus Data#

month=c("May", "June", "July"),
year=c(2009:2013),
######################## parameter#################
################### specific to 'physics'##########
thetao=T,
bottomT=T,
so=T,
uo=F,
vo=F,
mlotst=F,
#################specific to 'biogeochemistry'########
chl=T,
phyc=T,
o2=F,
no3=F,
po4=F,
si=F,
fe=F,
nh4=F,
nppv=F,
zeu=F,
spatial_plot=T,
violin_plot=T,
####################
#######################################
##3. Zooplankton OOPS settings##
#######################################
#season="Jan_Mar",
season="Apr_Jun",
#season="Jul_Sep",
#season="Oct_Dec",
Year=2005:2013,
Acartia=T,
Calanus_finmarchicus=T,
Calanus_helgolandicus=T,
Metridia_lucens=T,
Temora_longicornis=T,
Large_copepods=T,
Small_copepods=T,
###########################
time_bin = "1_year", 
#time_bin ="10_years",

############################# 
# #max_rel_error =0
max_rel_error = 0.3 
#max_rel_error =0.5
)
)



###Interactive
#rmarkdown::render("test.Rmd",params = "ask")

