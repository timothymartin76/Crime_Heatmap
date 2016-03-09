##Create heatmap of 2015 NYC Felony data

##You will need these packages
require(ggplot2)
require(reshape2)
require(RColorBrewer)

##Access NYPD crime data for 2015 here https://data.cityofnewyork.us/Public-Safety/year/d9z7-6jnh
mydata<- read.csv("2015_Felonies.csv", sep=',')

##Prepare data - row names
row.names(mydata) <- mydata$Month

##Remove first column
mydata <- mydata[,2:8]

##Since the Offenses have varying ranges we need to scale all values to have mean=0, sd=1 for all variables
scaled <- as.matrix(scale(mydata))

##get rownames
rnames<- rownames(scaled)

# Add rownames to the data frame as a column
df<-cbind(rnames,data.frame(scaled))

##Melt data for ggplot2
md2<-melt(df)

##Change column headers
names(md2)<- c("Month", "Offense", "value")

##Order the Month variable
md2$Month <- factor(md2$Month, levels = unique(md2$Month), ordered = TRUE)

##Create a palette using RColorBrewer
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

#Create heatmap using ggplot2
zp1 <- ggplot(md2, aes(x =Month, y = Offense, fill = value))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100)) + geom_text(size=4, aes(fill = md2$value, label = round(md2$value))) + xlab("Month") + ylab("Offense")
zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
zp1 <- zp1 + theme_bw() +  ggtitle("2015 NYC Felony Offenses - Heatmap") + theme(text= element_text(size=14))
print(zp1)
