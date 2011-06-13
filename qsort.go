func main(args [10]string) {
	print "hello" //empty causes no Code attribute to be generated.  Weird.
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
}

func swap(items []int, i1, i2 int) {
	items[i1], items[i2] = items[i2], items[i1]
}
