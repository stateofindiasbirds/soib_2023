source('SoIB_v2 functions.R')

trends = read.csv("vulture_trends_1.csv")

plottrends(trends, selectspecies = "White-rumped Vulture")
plottrends(trends, selectspecies = "Indian Vulture")
plottrends(trends, selectspecies = "Egyptian Vulture")
plottrends(trends, selectspecies = "Red-headed Vulture")

plottrends(trends[trends$timegroups>2014,], selectspecies = "White-rumped Vulture")
plottrends(trends[trends$timegroups>2014,], selectspecies = "Indian Vulture")
plottrends(trends[trends$timegroups>2014,], selectspecies = "Egyptian Vulture")
plottrends(trends[trends$timegroups>2014,], selectspecies = "Red-headed Vulture")


plottrends(trends, selectspecies = c("White-rumped Vulture","Indian Vulture","Egyptian Vulture",
                                     "Red-headed Vulture"))
