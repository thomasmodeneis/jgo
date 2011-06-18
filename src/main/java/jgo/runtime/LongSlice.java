package jgo.runtime;

import java.util.*;

/**
 * A mutable slice of some array of longs.
 * 
 * @author Harrison Klaperman
 */
class LongSlice implements Slice<Long>, Iterable<Long> {
	private long[] array;
	private int offset;
	private int length;
	
	/**
	 * Creates a complete slice of the specified long array.
	 * The resultant slice will be equal in length to the given array
	 * and will begin at the zeroth element of the array.
	 * 
	 * @param arr the array from which to create this slice
	 * 
	 * @throws NullPointerException if the passed array is null
	 */
	LongSlice(long[] arr) {
		this(arr, 0, arr.length);
	}
	
	/**
	 * Creates a slice of the given long array with offset and length
	 * as specified.
	 * 
	 * @param arr the array from which to create this slice
	 * @param off the index in the array where this slice is to begin
	 * @param len the length of this slice
	 * 
	 * @throws NullPointerException      if the passed array is null
	 * @throws IllegalArgumentException  if the passed length is negative,
	 *                                   or would exceed capacity
	 * @throws IndexOutOfBoundsException if the passed offset is not a valid
	 *                                   index into the array
	 */
	LongSlice(long[] arr, int off, int len) {
		if (array == null)
			throw new NullPointerException("array is null");
		if (len < 0 || off + len > arr.length)
			throw new IllegalArgumentException("invalid length");
		if (off < 0 || arr.length <= off)
			throw new IndexOutOfBoundsException("invalid offset");
		
		array  = arr;
		offset = off;
		length = len;
	}
	
	/**
	 * @inheritDoc
	 */
	public LongSlice slice(int low, int high) {
		if (low < 0 || length <= low)
			throw new IndexOutOfBoundsException("lower bound invalid");
		if (high < 0 || length <= high)
			throw new IndexOutOfBoundsException("upper bound invalid");
		
		return new LongSlice(array, offset + low, high - low);
	}
	
	/**
	 * @inheritDoc
	 */
	public LongSlice slice() {
		return slice(0, len());
	}
	
	/**
	 * @inheritDoc
	 */
	public LongSlice sliceLow(int low) {
		return slice(low, len());
	}
	
	/**
	 * @inheritDoc
	 */
	public LongSlice sliceHigh(int high) {
		return slice(0, high);
	}
	
	/**
	 * Returns the element of this slice at the specified index.
	 */
	public Long get(int index) {
		if (index >= length)
			throw new IndexOutOfBoundsException();
		return array[offset + index];
	}
	
	/**
	 * Updates the element of this slice at the specified index.
	 */
	public void set(int index, Long value) {
		if (index >= length)
			throw new IndexOutOfBoundsException();
		array[offset + index] = value;
	}
	
	/**
	 * Returns the length of this slice.
	 */
	public int len() {
		return length;
	}
	
	/**
	 * Returns the capacity of this slice.
	 */
	public int cap() {
		return array.length - offset;
	}
	
	/**
	 * Returns an iterator over the elements of this slice.
	 */
	public Iterator<Long> iterator() {
		return new Iterator<Long>() {
			private int index = offset;
			
			public boolean hasNext() {
				return index < offset + length;
			}
			
			public Long next() {
				if (!hasNext())
					throw new NoSuchElementException();
				return array[index++];
			}
			
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}
}
