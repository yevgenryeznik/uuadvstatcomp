repeat {
	dice <- sample(1:6, 3)
  #dice <- sample(1:6, 3, replace = T)##--123--45
	if (sum(dice) == 18) break()
}
