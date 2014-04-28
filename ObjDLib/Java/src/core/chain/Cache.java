package core.chain;

public class Cache<X, R> {
    public final F<X, R> f;
    private X _lastX;
    private R _lastF;
    public R applyX(final X x) {
        if(this._lastX != null && this._lastX.equals(x)) {
            if(this._lastF == null) {
                throw new RuntimeException("Not null");
            }
            return this._lastF;
        } else {
            this._lastX = x;
            this._lastF = this.f.apply(x);
            if(this._lastF == null) {
                throw new RuntimeException("Not null");
            }
            return this._lastF;
        }
    }
    public Cache(final F<X, R> f) {
        this.f = f;
    }
}