package core.chain;

public class Lazy<T> {
    public final F0<T> f;
    private T _value;
    private boolean _calculated;
    public boolean isCalculated() {
        return this._calculated;
    }
    public T get() {
        if(this._calculated) {
            if(this._value == null) {
                throw new RuntimeException("Not null");
            }
            return this._value;
        } else {
            this._value = this.f.apply();
            this._calculated = true;
            if(this._value == null) {
                throw new RuntimeException("Not null");
            }
            return this._value;
        }
    }
    public Lazy(final F0<T> f) {
        this.f = f;
        this._calculated = false;
    }
}