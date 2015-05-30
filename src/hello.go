func say(msg string) {
	print msg
}

func main(args [4]string) {
	msg := "hello, world"
	say(msg)
	
	for i := 0; i < 10; i++ {
		print i
	}
}
