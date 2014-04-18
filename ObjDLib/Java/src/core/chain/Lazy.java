package core.chain;

public class Lazy<T> {
    public final F<Void, T> f;
    private T _value;
    private boolean _calculated = ERROR: Unknown False;
    public boolean isCalculated() {
        return this._calculated;
    }
    public T get() {
        if(this._calculated) {
            if(this._value == null) {
                throw new RuntimeException("Not null");
            } else {
                return this._value;
            }
        } else {
            this._value = ERROR: Unknown <Lazy#C<T#G>>self.<eIU>f\void -> §T#G§\();
            this._calculated = ERROR: Unknown True;
            if(this._value == null) {
                throw new RuntimeException("Not null");
            } else {
                return this._value;
            }
        }
    }
    public Lazy(F<Void, T> f) {
    }
}