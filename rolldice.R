repeat {
	dice <- sample(1:6, 3)
  #dice <- sample(1:6, 3, replace = T)##--12
	if (sum(dice) == 18) break()
}
