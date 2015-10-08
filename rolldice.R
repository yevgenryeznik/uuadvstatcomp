repeat {
	dice <- sample(1:6, 3)
  #dice <- sample(1:6, 3, replace = T)##--1
	if (sum(dice) == 18) break()
}
