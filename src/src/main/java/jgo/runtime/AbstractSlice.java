package jgo.runtime;

import java.util.*;

/**
 * A mutable slice.
 * 
 * @param <T> the element type of this slice
 * 
 * @author Harrison Klaperman
 */
abstract class AbstractSlice<T> implements Slice<T> {
	/**
	 * Returns a pointer to the contents of this slice at the
	 * specified index.
	 */
	public Ptr<T> ptrTo(final int index) {
		return new Ptr<T>() {
			public T get() {
				return AbstractSlice.this.get(index);
			}
			public void set(T v) {
				AbstractSlice.this.set(index, v);
			}
		};
	}
}
