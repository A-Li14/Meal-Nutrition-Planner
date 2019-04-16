
###Explore Nutrition data
require(readxl)
require(data.table)

###Nutrient values are for 100g of the food
ds = data.table(read_excel("ABBREV.xlsx"))

ds$Shrt_Desc
ds$GmWt_Desc1[1:100]
ds$GmWt_Desc2[1:100]

ds[1:10,.(Shrt_Desc,Energ_Kcal,GmWt_1,GmWt_Desc1,GmWt_2,GmWt_Desc2)]

# def = fread("CompositionOfFoods_DataDictionary.CSV")
# setnames(def,gsub(" ","_",names(def)))
# 
# def[1:5]
# def[,Field_Name]
# 
# def[Field_Name=="GmWt_Desc1",]

ds[order(Energ_Kcal,decreasing = T)]

ds[,summary(Energ_Kcal)]
ds[order(Energ_Kcal),plot(Energ_Kcal)]
plot(ecdf(ds$Energ_Kcal))
ds[,hist(Energ_Kcal)]

ds[Energ_Kcal>50&Energ_Kcal<100,Shrt_Desc]

ds[,Food_Group:=""]
ds[grep("BEAN",Shrt_Desc),Food]
ds[grep("FAT",Shrt_Desc)]


###I wonder if the Access version of the ABBREV file has food groups already. It's always good to be 
###lazy about these sorts of tasks
#install.packages("RODBC")
require(RODBC)

#channel = odbcDriverConnect("Access Data/sr27.accdb")

#odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=Access Data/sr27.accdb")

channel = odbcConnectAccess2007("~/R/Data/Nutrition Data/Access Data/sr27.accdb")




