package jgo.runtime;

import java.util.*;

/**
 * A mutable slice of some array, as featured in the Go programming
 * language.
 * 
 * TODO:  Make this interface extend java.util.List.
 * 
 * @param <T> the element type of this slice
 * 
 * @author Harrison Klaperman
 */
public interface Slice<T> extends Iterable<T> {
	
	/**
	 * Slices this slice on the given bounds.
	 * 
	 * @param low  the lower bound of the slice to be created
	 * @param high the upper bound of the slice to be created
	 * @return a subslice of this slice beginning at the lower
	 *         bound (inclusive) and ending at the upper bound
	 *         (exclusive)
	 * 
	 * @throws IllegalArgumentException  if the lower bound exceeds
	 *                                   the upper bound
	 * @throws IndexOutOfBoundsException if the bounds are not valid
	 *                                   indices into this slice
	 */
	Slice<T> slice(int low, int high);
	
	/**
	 * Duplicates this slice.
	 * 
	 * @return a (non-proper) subslice of this slice beginning
	 *         at the lower bound (inclusive) and ending at the
	 *         upper bound (exclusive)
	 */
	Slice<T> slice();
	
	/**
	 * Produces a suffix of this slice
	 * 
	 * @param low the lower bound of the slice to be created
	 * @return a subslice of this slice beginning at the lower bound
	 *         (inclusive) and ending at the end of this bound
	 * 
	 * @throws IndexOutOfBoundsException if the bound is not a valid
	 *                                   index into this slice
	 */
	Slice<T> sliceLow(int low);
	
	/**
	 * Produces a prefix of this slice
	 * 
	 * @param high the upper bound of the slice to be created
	 * @return a subslice of this slice beginning at index {@code 0}
	 *         (inclusive) and ending at the upper bound (exclusive)
	 * 
	 * @throws IndexOutOfBoundsException if the bound is not a valid
	 *                                   index into this slice
	 */
	Slice<T> sliceHigh(int high);
	
	/**
	 * Returns the element of this slice at the specified index.
	 */
	T get(int index);
	
	/**
	 * Updates the element of this slice at the specified index.
	 */
	void set(int index, T value);
	
	/**
	 * Returns a pointer to the contents of this slice at the
	 * specified index.
	 */
	Ptr<T> ptrTo(int index);
	
	/**
	 * Returns the length of this slice.
	 */
	int len();
	
	/**
	 * Returns the capacity of this slice.
	 */
	int cap();
	
	/**
	 * Returns an iterator over the elements of this slice.
	 */
	Iterator<T> iterator();
}
