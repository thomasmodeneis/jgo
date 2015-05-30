package jgo.runtime;

import java.util.*;

/**
 * An object representing a pointer.
 * 
 * @param <T> the element type of this pointer
 * 
 * @author Harrison Klaperman
 */
public interface Ptr<T> {
	/**
	 * Inderects through this pointer, returning the value
	 * of the variable pointed at.
	 * 
	 * @return the value of the referent of this pointer
	 */
	T get();
	
	/**
	 * Indirects through this pointer, setting the variable
	 * pointed at to the value specified.
	 * 
	 * @param newValue the value which the referent of this
	 *                 pointer is to be set to
	 */
	void set(T newValue);
}
