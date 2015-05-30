package jgo.runtime;

import java.util.*;

/**
 * A pointer which stores its referent as a field.
 * 
 * <b>Important:</b> This class cannot be used to make a
 * pointer to (take the "address" of) a local variable or
 * other lvalue.  See the documentation of the factory
 * method <code>make(T)</code> for more info.
 * 
 * @param <T> the element type of this pointer
 * 
 * @author Harrison Klaperman
 */
class VarPtr<T> implements Ptr<T> {
	private T referent;
	
	private VarPtr(T initialValue) {
		referent = initialValue;
	}
	
	/**
	 * Inderects through this pointer, returning the value
	 * of the variable pointed at.
	 * 
	 * @return the value of the referent of this pointer
	 */
	public T get() {
		return referent;
	}
	
	/**
	 * Indirects through this pointer, setting the variable
	 * pointed at to the value specified.
	 * 
	 * @param newValue the value which the referent of this
	 *                 pointer is to be set to
	 */
	public void set(T newValue) {
		referent = newValue;
	}
	
	/**
	 * Creates a variable-pointer whose initial value is null.
	 * 
	 * @return a new variable-pointer
	 */
	public static <T> VarPtr<T> make() {
		return new VarPtr<T>(null);
	}
	
	/**
	 * Creates a variable-pointer whose initial value is
	 * that specified.
	 * 
	 * <p><b>Important:</b> Passing a local variable, field,
	 * or other lvalue to this constructor does <i>not</i>
	 * create a pointer whose referent is that lvalue!
	 * Instead, it creates a pointer to what is in effect
	 * a new variable, initializing that variable to the
	 * current value of the passed lvalue.</p>
	 * 
	 * <p>In other words, the following code will <i>not</i> work: <br />
	 * <code>
	 * int i = 1;
	 * Ptr<Integer> p = new VarPtr<Integer>(i);
	 * p.set(42);
	 * assert i == 42; //still is 1.
	 * </code></p>
	 * 
	 * @param initialValue the value to initialize the
	 *                     referent of this pointer to
	 * 
	 * @return a new variable pointer initialized to the
	 *         specified value
	 */
	public static <T> VarPtr<T> make(T initialValue) {
		return new VarPtr<T>(initialValue);
	}
}
