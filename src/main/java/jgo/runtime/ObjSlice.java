package jgo.runtime;

import java.util.*;

/**
 * A mutable slice of some array of objects.
 * 
 * @param <T> the element type of this slice
 * 
 * @author Harrison Klaperman
 */
public class ObjSlice<T> implements Slice<T>, Iterable<T> {
	private Object[] array;
	private int offset;
	private int length;
	
	/**
	 * Creates a complete slice of the specified object array.
	 * The resultant slice will be equal in length to the given array
	 * and will begin at the zeroth element of the array.
	 * 
	 * @param arr the array from which to create this slice
	 * 
	 * @throws NullPointerException if the passed array is null
	 */
	public ObjSlice(T[] arr) {
		this(arr, 0, arr.length);
	}
	
	/**
	 * Creates a slice of the given object array with offset and length
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
	public ObjSlice(T[] arr, int off, int len) {
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
	public ObjSlice<T> slice(int low, int high) {
		if (low < 0 || length <= low)
			throw new IndexOutOfBoundsException("lower bound invalid");
		if (high < 0 || length <= high)
			throw new IndexOutOfBoundsException("upper bound invalid");
		
		return new ObjSlice<T>((T[])array, offset + low, high - low);
	}
	
	/**
	 * @inheritDoc
	 */
	public ObjSlice<T> slice() {
		return slice(0, len());
	}
	
	/**
	 * @inheritDoc
	 */
	public ObjSlice<T> sliceLow(int low) {
		return slice(low, len());
	}
	
	/**
	 * @inheritDoc
	 */
	public ObjSlice<T> sliceHigh(int high) {
		return slice(0, high);
	}
	
	/**
	 * Returns the element of this slice at the specified index.
	 */
	public T get(int index) {
		if (index >= length)
			throw new IndexOutOfBoundsException();
		return (T)array[offset + index];
	}
	
	/**
	 * Updates the element of this slice at the specified index.
	 */
	public void set(int index, T value) {
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
	public Iterator<T> iterator() {
		return new Iterator<T>() {
			private int index = offset;
			
			public boolean hasNext() {
				return index < offset + length;
			}
			
			public T next() {
				if (!hasNext())
					throw new NoSuchElementException();
				return (T)array[index++];
			}
			
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}
	
	
	/* FACTORIES */
	public static <T> ObjSlice<T> fromArray(T[] arr) {
		return fromArray(arr, 0, arr.length);
	}
	
	public static <T> ObjSlice<T> fromArrayLow(T[] arr, int low) {
		return new ObjSlice<T>(arr).slice(low, arr.length);
	}
	
	public static <T> ObjSlice<T> fromArray(T[] arr, int high) {
		return new ObjSlice<T>(arr).slice(0, high);
	}
	
	public static <T> ObjSlice<T> fromArray(T[] arr, int low, int high) {
		return new ObjSlice<T>(arr).slice(low, high);
	}
}
