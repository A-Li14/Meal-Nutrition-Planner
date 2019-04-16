###Tidy the abbrev file for use in a dashboard

require(readxl)
require(data.table)

###Nutrient values are for 100g of the food
ds = data.table(read_excel("ABBREV.xlsx"))
ds = ds[!duplicated(Shrt_Desc)] 
###There are 4 instances of repeated Shrt_Desc. In one case, GmWt1 and 2 are reversed. 
###For the others, the nutrient values differ. Uncomment the line below and run it to see
#ds[duplicated(Shrt_Desc)|duplicated(Shrt_Desc,fromLast=T)]

nutrient_list = names(ds)
nutrient_list = nutrient_list[!nutrient_list%in%c("NDB_No","Shrt_Desc","GmWt_1","GmWt_Desc1","GmWt_2","GmWt_Desc2","Refuse_Pct")]

###Having issues learning how to display the nutrient list using checkboxGroupInput in a pretty format
###Theoretically, a CSS solution is available. A hack to get around this is to split the nutrition list
###into two separate checkboxGroupInput widgets and combining the results. Below, splitting the list
nutrient_list1 = c("Energ_Kcal","Lipid_Tot_(g)","FA_Sat_(g)","FA_Mono_(g)","FA_Poly_(g)","Cholestrl_(mg)","Sodium_(mg)","Carbohydrt_(g)","Fiber_TD_(g)",
                   "Sugar_Tot_(g)","Protein_(g)",nutrient_list[grep("Vit",nutrient_list)],"Water_(g)","Calcium_(mg)","Iron_(mg)")
nutrient_list2 = nutrient_list[!nutrient_list%in%nutrient_list1]

###Use statistical methods to try to predict food groups based on nutrition. Food groups are:
#Meat, Grain, Vegetables, Fruits, Dairy, and Processed. This will be useful as a filtering option in the app



