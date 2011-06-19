package jgo.runtime;

import java.util.*;

/**
 * Utility functions for pointers.
 * 
 * @param <T> the element type of this pointer
 * 
 * @author Harrison Klaperman
 */
public class Ptrs {
	private Ptrs() { }
	
	public static <T> Ptr<T> fromArray(final T[] arr, final int index) {
		return new Ptr<T>() {
			public T get() {
				return arr[index];
			}
			public void set(T v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Boolean> fromArray(final boolean[] arr, final int index) {
		return new Ptr<Boolean>() {
			public Boolean get() {
				return arr[index];
			}
			public void set(Boolean v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Long> fromArray(final long[] arr, final int index) {
		return new Ptr<Long>() {
			public Long get() {
				return arr[index];
			}
			public void set(Long v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Integer> fromArray(final int[] arr, final int index) {
		return new Ptr<Integer>() {
			public Integer get() {
				return arr[index];
			}
			public void set(Integer v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Short> fromArray(final short[] arr, final int index) {
		return new Ptr<Short>() {
			public Short get() {
				return arr[index];
			}
			public void set(Short v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Character> fromArray(final char[] arr, final int index) {
		return new Ptr<Character>() {
			public Character get() {
				return arr[index];
			}
			public void set(Character v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Byte> fromArray(final byte[] arr, final int index) {
		return new Ptr<Byte>() {
			public Byte get() {
				return arr[index];
			}
			public void set(Byte v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Double> fromArray(final double[] arr, final int index) {
		return new Ptr<Double>() {
			public Double get() {
				return arr[index];
			}
			public void set(Double v) {
				arr[index] = v;
			}
		};
	}
	
	public static Ptr<Float> fromArray(final float[] arr, final int index) {
		return new Ptr<Float>() {
			public Float get() {
				return arr[index];
			}
			public void set(Float v) {
				arr[index] = v;
			}
		};
	}
}
