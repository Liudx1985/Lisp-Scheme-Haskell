-- getline.hs
getline = do {
	c <- getChar;
	if c == '\n'
		then return ""
		else do {
			a <- getline;
			return (c:a)
		}
}
