package jgo.runtime;

import java.util.*;

/**
 * Utility functions for slices.
 * 
 * @author Harrison Klaperman
 */
public final class Slices {
	private Slices() { }
	
	
	
	public static <T> Slice<T> make(int len) {
		return new ObjSlice<T>((T[])new Object[len]);
	}
	
	public static <T> Slice<T> make(int len, int cap) {
		return new ObjSlice<T>((T[])new Object[cap], 0, len);
	}
	
	
	
	public static <T> Slice<T> fromArray(T[] arr) {
		return new ObjSlice<T>(arr);
	}
	
	public static <T> Slice<T> fromArrayLow(T[] arr, int low) {
		return new ObjSlice<T>(arr).slice(low, arr.length);
	}
	
	public static <T> Slice<T> fromArrayHigh(T[] arr, int high) {
		return new ObjSlice<T>(arr).slice(0, high);
	}
	
	public static <T> Slice<T> fromArray(T[] arr, int low, int high) {
		return new ObjSlice<T>(arr).slice(low, high);
	}
	
	
	
	public static Slice<Boolean> fromArray(boolean[] arr) {
		return new BoolSlice(arr);
	}
	
	public static Slice<Boolean> fromArrayLow(boolean[] arr, int low) {
		return new BoolSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Boolean> fromArrayHigh(boolean[] arr, int high) {
		return new BoolSlice(arr).slice(0, high);
	}
	
	public static Slice<Boolean> fromArray(boolean[] arr, int low, int high) {
		return new BoolSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Long> fromArray(long[] arr) {
		return new LongSlice(arr);
	}
	
	public static Slice<Long> fromArrayLow(long[] arr, int low) {
		return new LongSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Long> fromArrayHigh(long[] arr, int high) {
		return new LongSlice(arr).slice(0, high);
	}
	
	public static Slice<Long> fromArray(long[] arr, int low, int high) {
		return new LongSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Integer> fromArray(int[] arr) {
		return new IntSlice(arr);
	}
	
	public static Slice<Integer> fromArrayLow(int[] arr, int low) {
		return new IntSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Integer> fromArrayHigh(int[] arr, int high) {
		return new IntSlice(arr).slice(0, high);
	}
	
	public static Slice<Integer> fromArray(int[] arr, int low, int high) {
		return new IntSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Short> fromArray(short[] arr) {
		return new ShortSlice(arr);
	}
	
	public static Slice<Short> fromArrayLow(short[] arr, int low) {
		return new ShortSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Short> fromArrayHigh(short[] arr, int high) {
		return new ShortSlice(arr).slice(0, high);
	}
	
	public static Slice<Short> fromArray(short[] arr, int low, int high) {
		return new ShortSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Character> fromArray(char[] arr) {
		return new CharSlice(arr);
	}
	
	public static Slice<Character> fromArrayLow(char[] arr, int low) {
		return new CharSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Character> fromArrayHigh(char[] arr, int high) {
		return new CharSlice(arr).slice(0, high);
	}
	
	public static Slice<Character> fromArray(char[] arr, int low, int high) {
		return new CharSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Byte> fromArray(byte[] arr) {
		return new ByteSlice(arr);
	}
	
	public static Slice<Byte> fromArrayLow(byte[] arr, int low) {
		return new ByteSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Byte> fromArrayHigh(byte[] arr, int high) {
		return new ByteSlice(arr).slice(0, high);
	}
	
	public static Slice<Byte> fromArray(byte[] arr, int low, int high) {
		return new ByteSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Double> fromArray(double[] arr) {
		return new DoubleSlice(arr);
	}
	
	public static Slice<Double> fromArrayLow(double[] arr, int low) {
		return new DoubleSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Double> fromArrayHigh(double[] arr, int high) {
		return new DoubleSlice(arr).slice(0, high);
	}
	
	public static Slice<Double> fromArray(double[] arr, int low, int high) {
		return new DoubleSlice(arr).slice(low, high);
	}
	
	
	
	public static Slice<Float> fromArray(float[] arr) {
		return new FloatSlice(arr);
	}
	
	public static Slice<Float> fromArrayLow(float[] arr, int low) {
		return new FloatSlice(arr).slice(low, arr.length);
	}
	
	public static Slice<Float> fromArrayHigh(float[] arr, int high) {
		return new FloatSlice(arr).slice(0, high);
	}
	
	public static Slice<Float> fromArray(float[] arr, int low, int high) {
		return new FloatSlice(arr).slice(low, high);
	}
}
