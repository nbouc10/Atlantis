library(ggplot2)
library(gridExtra)
library(Rmisc)
library(cowplot)

#The Goal of this script is to read in data from Atlantis Scenarios, 
#clean it, visualize biomass from the last 25 years of our scenario runs, 
#and conduct ANOVA + fit outputs to a linear model to determine relative effects of mussels vs. nutrient loads

#Set working directory to output folders (depends on which computer I'm working from)
#setwd("/Volumes/NWB_Drive/Outputs12-19/DRE_on" )
DRE_off <- ("C:/Users/nboucher/Box Sync/Atlantis/Outputs8-21-23/Scenarios/Dre_off")
Dre_on <- ("C:/Users/nboucher/Box Sync/Atlantis/Outputs8-21-23/Scenarios/Dre_on")
#Save date and time so that if/when I output plots in a .pdf 
#I have a record of what day they are from if I email them to someone else/move them to a new computer etc.
dt<-Sys.Date()

#List all files in folder and label which scenario is which
#Naming conventions ensure these are read in order
Files_on<- list.files(path = Dre_on, full.names = TRUE)
Lowon<- read.table(Files_on[1], header = TRUE)
Baseon<- read.table(Files_on[2], header = TRUE)
Highon<- read.table(Files_on[3], header = TRUE)

#Pull just the data we want (relative biomass, last 25 years of output)
#Code data with scenario (A, B, C + Mussels or No Mussels) for analysis when we combine into one dataset
#116:231 = 25-50 yrs, 369:461 = 80-100
Lowon  <- Lowon[369:461,2:34]
  Lowon["Nutrients"]<- 'A'
Baseon <- Baseon[369:461,2:34]
  Baseon["Nutrients"]<- 'B'
Highon <- Highon[369:461,2:34]
  Highon["Nutrients"]<- 'C'
Musselson<- rbind(Lowon, Baseon, Highon)
  Musselson["Mussels"]<- "Mussels"
  Musselson$DRE<- NULL

#Read in mussels off scenarios. Could put them all together, 
#but this allows us to keep our data separate as well as compare different runs
  
Files_off<- list.files(path = DRE_off, full.names = TRUE)
Lowoff<- read.table(Files_off[1], header = TRUE)
Baseoff<- read.table(Files_off[2], header = TRUE)
Highoff<- read.table(Files_off[3], header = TRUE)

Lowoff  <- Lowoff[369:461,2:34]
  Lowoff["Nutrients"]<- 'A'
Baseoff <- Baseoff[369:461,2:34]
  Baseoff["Nutrients"]<- 'B'
Highoff <- Highoff[369:461,2:34]
  Highoff["Nutrients"]<- 'C'
Musselsoff<- rbind(Lowoff, Baseoff, Highoff)
  Musselsoff["Mussels"]<- "No Mussels"
  Musselsoff$DRE <- NULL
  Musselsoff$DIN <- NULL

#Rename columns to long names rather than codes for ease of understanding in presentations/publications.
Functionalgroups<- c('Alewife', 'Bloater', 'Slimy Sculpin', 'Deepwater Sculpin', 'Lake Whitefish', 
                     'Round Goby', 'Yellow Perch', 'Walleye', 'Rainbow Smelt', 
                     'Chinook Salmon', 'Coho Salmon', 'Steelhead', 'Lake Trout', 'Burbot', 
                     'Copepods', 'Cladocerans', 'Bythotrephes', 'Mysis', 'Rotifers', 
                     'Protozoans', 'Pelagic Bacteria', 'Benthic Bacteria', 'Amphipods', 'Chironomids', 
                     'Diporeia', 'Oligochaetes', 'Green Algae', 'Picoplankton', 'Macrophytes', 'Labile Detritus',
                     'Refractory Detritus', 'Carrion', 'Nutrients', 'Mussels')
Agg<- rbind(Musselson, Musselsoff)
colnames(Agg)<- Functionalgroups

#Make Box + Whisker plots for each functional group at each of the 6 scenarios
#Save plots to a list so that I can call them based on their number in the list
plist<-list()
for (i in 1:34)local({
  i <- i
  p1 <-ggplot(Agg, aes(x=Agg$Nutrients, y=Agg[,i])) + geom_boxplot() +
               labs(x= "Nutrient Scenario", y = "Relative Biomass" , title = colnames(Agg[i])) +
               facet_grid(. ~ Mussels, scales = "fixed")
  plist[[i]]<<- p1
})

####Formatting for Ed 
for (i in 1:34)local({
  i <- i
  p1 <-ggplot(Agg, aes(x=Agg$Nutrients, y=Agg[,i])) + geom_boxplot() +
    labs(x= "Nutrient Scenario", y = "Relative Biomass" , title = colnames(Agg[i])) +
    facet_grid(. ~ Mussels, scales = "fixed") 
  p1 <- p1+ theme(axis.text = element_text(size = rel(1.3)),
            axis.title=element_text (size = rel(1.3)), 
            title = element_text( size = rel(1.3)),
            strip.text = element_text(size=rel(1.3)),
            panel.background = element_rect(fill = 'white', color = 'black'))
  plist[[i]]<<- p1
})

#Group species by their trophic status
#Salmon, Lake Trout, Steelhead are piscivores etc etc

preyfish<- grid.arrange(plist[[1]], plist[[2]], plist[[3]], plist[[4]], plist[[5]], plist[[6]], plist[[7]], plist[[9]], nrow = 2)
piscivores<-grid.arrange(plist[[8]], plist[[10]], plist[[11]], plist[[12]], plist[[13]], nrow = 1)
pelzoop<- grid.arrange(plist[[14]], plist[[15]], plist[[16]], plist[[17]], plist[[18]], plist[[19]], nrow = 2)
bacteria<- grid.arrange(plist[[20]], plist[[21]], nrow = 1)
benthos<- grid.arrange(plist[[22]], plist[[23]], plist[[24]], plist[[25]], nrow = 1)
primaryproducers<- grid.arrange(plist[[26]], plist[[27]], plist[[28]], plist[[29]], nrow = 1)

#LM and ANOVA for each species, output results to console. 
dvlist<-names(Agg)[1:29]
model<- lapply(dvlist, function(x){
  lm(substitute(i~ Mussels + Nutrients, list(i = as.name(x))), data = Agg)})
lapply(model, summary)
lapply(model, anova)
