package objd.react;

import objd.lang.*;
import objd.concurrent.DispatchQueue;

public abstract class React<T> extends Observable_impl<T> {
    @Override
    public abstract void attachObserver(final Observer<T> observer);
    @Override
    public abstract void detachObserver(final Observer<T> observer);
    public abstract T value();
    public static <T> React<T> applyValue(final T value) {
        return new Val<T>(value);
    }
    public static <A, T> React<T> applyAF(final React<A> a, final F<A, T> f) {
        return ((React<T>)(((React)(new MappedReact<A, T>(a, f)))));
    }
    public static <A, B, T> React<T> applyABF(final React<A> a, final React<B> b, final F2<A, B, T> f) {
        return ((React<T>)(((React)(new MappedReact2<A, B, T>(a, b, f)))));
    }
    public static <A, B, C, T> React<T> applyABCF(final React<A> a, final React<B> b, final React<C> c, final F3<A, B, C, T> f) {
        return ((React<T>)(((React)(new MappedReact3<A, B, C, T>(a, b, c, f)))));
    }
    public static <A, T> React<T> asyncQueueAF(final DispatchQueue queue, final React<A> a, final F<A, T> f) {
        return ((React<T>)(((React)(new AsyncMappedReact<A, T>(queue, a, f)))));
    }
    public static <A, T> React<T> asyncAF(final React<A> a, final F<A, T> f) {
        return React.<A, T>asyncQueueAF(DispatchQueue.aDefault, a, f);
    }
    public static <A, B, T> React<T> asyncQueueABF(final DispatchQueue queue, final React<A> a, final React<B> b, final F2<A, B, T> f) {
        return ((React<T>)(((React)(new AsyncMappedReact2<A, B, T>(queue, a, b, f)))));
    }
    public static <A, B, T> React<T> asyncABF(final React<A> a, final React<B> b, final F2<A, B, T> f) {
        return React.<A, B, T>asyncQueueABF(DispatchQueue.aDefault, a, b, f);
    }
    public static <A, B, C, T> React<T> asyncQueueABCF(final DispatchQueue queue, final React<A> a, final React<B> b, final React<C> c, final F3<A, B, C, T> f) {
        return ((React<T>)(((React)(new AsyncMappedReact3<A, B, C, T>(queue, a, b, c, f)))));
    }
    public static <A, B, C, T> React<T> asyncABCF(final React<A> a, final React<B> b, final React<C> c, final F3<A, B, C, T> f) {
        return React.<A, B, C, T>asyncQueueABCF(DispatchQueue.aDefault, a, b, c, f);
    }
    public <R> React<R> mapF(final F<T, R> f) {
        return ((React<R>)(((React)(new MappedReact<T, R>(this, f)))));
    }
    public <R> React<R> flatMapF(final F<T, React<R>> f) {
        return ((React<R>)(((React)(new FlatMappedReact<T, R>(this, f)))));
    }
    public <R> React<R> asyncMapQueueF(final DispatchQueue queue, final F<T, R> f) {
        return ((React<R>)(((React)(new AsyncMappedReact<T, R>(queue, this, f)))));
    }
    public <R> React<R> asyncMapF(final F<T, R> f) {
        return this.<R>asyncMapQueueF(DispatchQueue.aDefault, f);
    }
    public React() {
    }
    public String toString() {
        return "React";
    }
}