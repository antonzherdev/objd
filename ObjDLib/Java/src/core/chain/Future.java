package core.chain;

public class Future<T> {
    public static Future<T> applyF(F<Void, T> f) {
    }
    public static Future<Tuple2<A, B>> joinAB(Future<A> a,Future<B> b) {
    }
    public static Future<Tuple3<A, B, C>> joinABC(Future<A> a,Future<B> b,Future<C> c) {
    }
    public static Future<Tuple4<A, B, C, D>> joinABCD(Future<A> a,Future<B> b,Future<C> c,Future<D> d) {
    }
    public static Future<Tuple5<A, B, C, D, E>> joinABCDE(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e) {
    }
    public static Future<R> mapABF(Future<A> a,Future<B> b,F2<A, B, R> f) {
    }
    public static Future<R> mapABCF(Future<A> a,Future<B> b,Future<C> c,F3<A, B, C, R> f) {
    }
    public static Future<R> mapABCDF(Future<A> a,Future<B> b,Future<C> c,Future<D> d,F4<A, B, C, D, R> f) {
    }
    public static Future<R> mapABCDEF(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e,F5<A, B, C, D, E, R> f) {
    }
    public static Future<T> successfulResult(T result) {
    }
    public abstract Try<T> result();
    public boolean isCompleted() {
    }
    public boolean isSucceeded() {
    }
    public boolean isFailed() {
    }
    public abstract void onCompleteF(F<Try<T>, Void> f);
    public void onSuccessF(F<T, Void> f) {
    }
    public void onFailureF(F<Object, Void> f) {
    }
    public Future<R> mapF(F<T, R> f) {
    }
    public Future<Void> forF(F<T, Void> f) {
    }
    public Future<R> flatMapF(F<T, Future<R>> f) {
    }
    public Try<T> waitResultPeriod(float period) {
    }
    public Try<T> waitResult() {
    }
    public void waitAndOnSuccessAwaitF(float await,F<T, Void> f) {
    }
    public void waitAndOnSuccessFlatAwaitF(float await,F<I, Void> f) {
    }
    public T getResultAwait(float await) {
    }
    public Future<Tuple2<T, R>> joinAnother(Future<R> another) {
    }
    public Future() {
    }
    static ClassType<Future<T>> type;
}