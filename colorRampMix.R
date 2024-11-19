# trying mixed colorramp for temp/precip plots
# https://stackoverflow.com/questions/11773295/how-can-one-mix-2-or-more-color-palettes-to-show-a-combined-color-value
# MAC 08/13/23

# brown 250, 191, 102 (250, 218, 102), 250, 225, 102
# white 255, 255, 255
# green 102, 250, 124 (102, 250, 181)
# 
# blue 102, 178, 250 (102, 201, 250)
# white 255, 255, 255
# red 250, 114, 102 (250, 102, 132) (250, 102, 154)

# categories
cats<-expand.grid(temp=c("cool","avg","warm"),
                  prec=c("dry","avg","wet"))

# category colors
tempCols<-cbind.data.frame(cat=c("cool","avg","warm"),
                           rgbColr=c(102, 255, 250),
                           rgbColg=c(201,255,102),
                           rgbColb=c(250,255,154))

precCols<-cbind.data.frame(cat=c("dry","avg","wet"),
                           rgbColr=c(250, 255, 102),
                           rgbColg=c(225,255,250),
                           rgbColb=c(102,255,181))
# combine
cats<-merge(cats, tempCols, by.x = "temp",by.y="cat")
cats<-merge(cats, precCols, by.x = "prec",by.y="cat")
# combined colors
cats$red<-round(cats$rgbColr.x*cats$rgbColr.y/255,0)
cats$green<-round(cats$rgbColg.x*cats$rgbColg.y/255,0)
cats$blue<-round(cats$rgbColb.x*cats$rgbColb.y/255,0)
cats$rgbComb<-rgb(green=cats$green, red=cats$red, blue=cats$blue, maxColorValue=255)

library(ggplot2)
ggplot(cats, aes(x=prec, y=temp)) + 
  geom_tile(aes(fill=rgbComb), color="white") + 
  scale_fill_identity()

# save df of combos/colors
colorsTP<-cats[,c("prec","temp","rgbComb")]
colorsTP$code<-paste0(colorsTP$temp,"-",colorsTP$prec)
save(colorsTP, file = "./data/colorPal.RData")



