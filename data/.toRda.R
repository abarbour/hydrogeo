diffusiv <- read.csv(".diffusiv.txt",
	header=TRUE,
	comment.char="#",
	stringsAsFactors=TRUE,
	strip.white=TRUE)
save(diffusiv, file="diffusiv.rda")
