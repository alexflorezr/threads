# download looks images
# Altought multiple sources can be used to download the data, I will start >
# > with TAGWALK (TW), oppening the possibility to use its tags in the future

### Libraries ####
library(rvest) # htmlnodes
library(xml2) # readhtml()
library(RCurl)
library(httr) #(GET)

### Variables ####
year <- 2018
# Seasons in TAGWALK are defined as: spring-summe and fall-winter
season <- c("spring-summer", "fall-winter")
# A vector of designers names (spelling is important in this stage)
designer <- c("hermes", "gucci", "prada", "dries-van-noten", "thom-browne", 
              "junya-watanabe", "lanvin", "fendi", "marni", "n21", "lemaire",
              "valentino", "neil-barrett", "haider-ackermann", "berluti", 
              "wooyoungmi")

### internal functions ####
get.look_TW <- function(vD, xsrc){
        for(l in 1:length(xsrc)){
                xlook <- formatC(l, width = 2, format = "d",flag = "0")
                xfile <- paste(paste(vD, "_", sep=""), xlook, ".jpeg", sep="")
                download.file(xsrc[l], destfile = xfile)
        }
}

### Main function ####
get.collection.TW <- function(vSeason, vYear, vDesigner){
        # The parts of the URL
        # Base of the URL << string >>
        vB <- "https://www.tag-walk.com/en/photo/list/man/all-categories/all-cities/"
        # End of the URL << string >>
        vT <- "?page1"
        #Assemble the URL
        xgrid <- expand.grid(vSeason, vYear, vDesigner)
        for (x in 1:nrow(xgrid)){
                vgrid_S <- as.character(xgrid[x,1])
                vgrid_Y <- xgrid[x,2]
                vgrid_D <- as.character(xgrid[x,3])
                xmiddle <- paste(paste(vgrid_S, vgrid_Y, sep="-"), vgrid_D, sep="/")
                xURL <- paste(vB, xmiddle, vT, sep="")
                xdir <- paste(vgrid_D, paste(substr(vgrid_S,1,1), substr(vgrid_Y,3,4), sep=""), sep="_")
                xshort_S <- ifelse(vgrid_S == "spring-summer", "S/S", "F/W")
                xshort_Y <- substr(vgrid_Y, 3, 4)
                # read the html
                xhtml <- read_html(xURL)
                # get the source file names to download the images
                xlegend <- html_text(html_nodes(xhtml, ".legende")[1])
                xleg <- gsub(" ", "", strsplit(xlegend, split = "—")[[1]][2])
                if(xleg == paste(xshort_S, xshort_Y, sep = "")){
                        xsrc <- xml_attr(xml_find_all(xhtml, ".//img"), "src")
                        # remove the 1st all 2-last images (not looks)
                        xsrc <- xsrc[-c(1,(length(xsrc)*c(1,1)+c(0,-1)))]
                        if (length(xsrc) >= 99){
                                next()
                        }else{
                                if(!(xget$status_code == 404)){
                                        setwd("~/Desktop/Threads/Designers/")
                                        if(!dir.exists(xdir)){
                                                dir.create(xdir)
                                
                                        }
                                        setwd(xdir)
                                        get.look_TW(vD = vgrid_D, xsrc)
                                }
                        }
                }
        }
}

vSeason <- season
vDesigner <- designer
vYear <- year
get.collection.TW(vSeason = Season, vYear = Year, vDesigner = Designer[1:4])
