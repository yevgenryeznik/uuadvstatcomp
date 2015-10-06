repeat {
	dice <- sample(1:6, 3)
	recover()
	if (sum(dice) == 18) break()
}