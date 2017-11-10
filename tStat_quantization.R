# Color quatization for DVN SS18
# Required libraries
library(jpeg)
library(grid)
library(gridExtra)

# Internal functions
# CQ == Color quantization, retreives the n main solid colors in the picture
CQ <- function(vimage, n){
        col_vctr <- vector()
        xdf <- data.frame(
                red = matrix(vimage[,,1], ncol=1),
                green = matrix(vimage[,,2], ncol=1),
                blue = matrix(vimage[,,3], ncol=1))
        # Estimate the K means
        K = kmeans(xdf,n)
        col_vctr <- rgb(K$centers)
        col_vctr
}
# heat plots a heatmap with the colors in each outfit
xheat <- function(vA){
        ximage <- matrix(1,nrow=dim(vA)[1], ncol=dim(vA)[2])
        image(ximage, col=as.vector(vA))
}

# The script
setwd("~/Desktop/A/Tela/Designers/DVN/SS18_M/Looks_cut/")
files <- dir()
looks_df <- matrix(nrow=length(dir()), ncol=8)
for(f in seq_along(files)){
        xlook <- readJPEG(files[f])
        looks_df[f,] <- sort(CQ(xlook, 8))
}
image(looks_df, useRaster = T)


# plotting segments
plot.seg <- function(vA){
        plot(1,1, type="n", xlim=c(0,dim(vA)[2]), ylim=c(0,dim(vA)[1]))
        x <- c(0,0,1,1)
        y <- c(0,1,1,0)
        for(row in 1:dim(vA)[1]){
                for(col in 1:dim(vA)[2]){
                        polygon(x,y, col=vA[row,col], border = NA)
                        x <- x+1
                }
                y <- y +1
                x <- c(0,0,1,1)
        }
}
plot.seg(looks_df)
