func main(args [10]string) {
	print "unsorted:" //empty causes no Code attribute to be generated.  Weird.
	s := mkTestSlice()
	//printSlice(s)
	//Sort(s)
	//print "sorted:"
	//printSlice(s)
}

func mkTestSlice() []int {
	s := make([]int, 10)
	s[0] = 7
	s[1] = 4
	s[2] = 1
	s[3] = 3
	s[4] = 8
	s[5] = 0
	s[6] = 9
	s[7] = 2
	s[8] = 6
	s[9] = 6
	return s
}

func printSlice([]int s) {
	for (i := 0; i < len(s); i++) {
		print s[i]
	}
}

//Sort sorts the given list in place.
func Sort(items []int) {
	qsort(items)
}

func qsort(items []int) {
	if len(items) <= 1 { //len is a builtin func.
		return
	}
	pivotIndex := partition(items)
	qsort(items[:pivotIndex])   //sort the slice from 0 (incl) to pivotIndex (excl)
	qsort(items[pivotIndex-1:]) //sort the slice from pivotIndex-1 (incl) to len (excl)
}

//partially based on the Wikipedia in-place algorithm
func partition(items []int) int {
	//we take the pivot to be the rightmost element
	pivot := items[len(items) - 1]
	leftPos := 0
	for i := 1; i < len(items); i++ {
		if items[i] < pivot {
			swap(items, i, leftPos)
			leftPos++
		}
	}
	swap(items, len(items) - 1, leftPos)
	return leftPos //need to detect missing return and generate err msg.
}

func swap(items []int, i1, i2 int) {
	items[i1], items[i2] = items[i2], items[i1]
}
