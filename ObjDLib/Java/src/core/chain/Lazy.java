package core.chain;

public class Lazy<T> {
    public final F<Void, T> f;
    private T _value;
    private boolean _calculated = ERROR: Unknown False;
    public boolean isCalculated() {
        return _calculated;
    }
    public T get() {
        ERROR: Unknown if(<Lazy#C<T#G>>self.<emp>_calculated\bool\) return <Lazy#C<T#G>>self.<emp>_value\(§T#G§)?\.get
else {
    (<Lazy#C<T#G>>self.<emp>_value\(§T#G§)?\ = some(<Lazy#C<T#G>>self.<eIU>f\void -> §T#G§\())\§(T#G)?§\)
    (<Lazy#C<T#G>>self.<emp>_calculated\bool\ = True)
    return <Lazy#C<T#G>>self.<emp>_value\(§T#G§)?\.get
};
    }
    public Lazy(F<Void, T> f) {
    }
}