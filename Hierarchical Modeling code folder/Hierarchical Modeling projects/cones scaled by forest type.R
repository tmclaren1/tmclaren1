
cones <- data.frame("Tot_Cones" = c(31,31,186,186,186,
              872, 872, 872, 872, 
              37, 37, 37, 172, 172, 172, 172,
              49, 49, 49, 120, 120, 120, 120,
              3243, 3243, 3243, 3243,
              73, 73, 73, 119, 119, 119, 119,
              151, 151, 151, 101, 101, 101, 101,
              3, 3, 324, 324, 324, 324,
              9, 9, 9, 775, 775, 775, 775,
              155, 155, 155, 1979, 1979, 1979, 1979,
              1189, 1189, 1189, 1189),
"Site" = c("Avalanche Peak","Avalanche Peak","Avalanche Peak", "Avalanche Peak","Avalanche Peak",
           "Baronette", "Baronette", "Baronette", "Baronette", 
           "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam",
           "Canyon", "Canyon", "Canyon", "Canyon","Canyon", "Canyon", "Canyon",
           "Confluence", "Confluence", "Confluence", "Confluence",
           "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North",
           "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South",
           "Dunraven", "Dunraven", "Dunraven", "Dunraven", "Dunraven", "Dunraven",
           "Mammoth", "Mammoth", "Mammoth", "Mammoth", "Mammoth", "Mammoth", "Mammoth",
           "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake",
           "West Thumb", "West Thumb", "West Thumb", "West Thumb"),
"Forest_type" = c("PIAL","PIAL","PIAL", "PIAL","PIAL", 
                  "PIEN", "PIEN", "PIEN", "PIEN", 
                  "PSME", "PSME", "PSME", "PSME", "PSME", "PSME", "PSME",
                  "PICO", "PICO", "PICO", "PICO", "PICO", "PICO", "PICO",
                  "PIFL", "PIFL", "PIFL", "PIFL",
               
                     "PICO", "PICO", "PICO", "PICO", "PICO", "PICO", "PICO",
                  "PIAL", "PIAL", "PIAL", "PIAL", "PIAL", "PIAL", "PIAL",
                  "PIAL", "PIAL", "PIAL", "PIAL", "PIAL", "PIAL",
                  "PIFL", "PIFL", "PIFL", "PIFL", "PIFL", "PIFL", "PIFL",
                  "PSME", "PSME", "PSME", "PSME", "PSME", "PSME", "PSME",
                  "PIEN", "PIEN", "PIEN", "PIEN"))
cones


PIAL<-cones%>%
  filter(Forest_type=="PIAL")

PICO<-cones%>%
  filter(Forest_type=="PICO")

PIEN<-cones%>%
  filter(Forest_type=="PIEN")

PIFL<-cones%>%
  filter(Forest_type=="PIFL")

PSME<-cones%>%
  filter(Forest_type=="PSME")

PIAL$Scaled_Cones<-scale(PIAL$Tot_Cones)
PICO$Scaled_Cones<-scale(PICO$Tot_Cones)
PIEN$Scaled_Cones<-scale(PIEN$Tot_Cones)
PIFL$Scaled_Cones<-scale(PIFL$Tot_Cones)
PSME$Scaled_Cones<-scale(PSME$Tot_Cones)


Cones_scaled<-rbind(PIAL,PICO,PIEN,PIFL,PSME)

Cones_scaled <- Cones_scaled[order(Cones_scaled$Site),]

Scaled_Cones<-as.vector(Cones_scaled$Scaled_Cones)
