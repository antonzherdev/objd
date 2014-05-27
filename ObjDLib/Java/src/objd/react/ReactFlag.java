package objd.react;

import objd.lang.*;
import objd.collection.ImArray;

public final class ReactFlag extends MReact<Boolean> {
    public final ImArray<Observable<Object>> reacts;
    private final ImArray<Observer<Object>> observers;
    public void set() {
        _setValue(true);
    }
    public void setValue(final boolean value) {
        _setValue(value);
    }
    public void clear() {
        _setValue(false);
    }
    public void processF(final P0 f) {
        if(this.value()) {
            f.apply();
            this.clear();
        }
    }
    public ReactFlag(final boolean initial, final ImArray<Observable<Object>> reacts) {
        super(initial);
        this.reacts = reacts;
        this.observers = ((ImArray<Observer<Object>>)(((ImArray)(reacts.chain().<Observer<Object>>mapF(((F<Observable<Object>, Observer<Object>>)(((F)(new F<Observable<Object>, Observer<Object>>() {
            @Override
            public Observer<Object> apply(final Observable<Object> r) {
                return r.observeF(((P<Object>)(((P)(new P<Object>() {
                    @Override
                    public void apply(final Object _) {
                        _setValue(true);
                    }
                })))));
            }
        }))))).toArray()))));
    }
    static public ReactFlag applyInitial(final boolean initial) {
        return new ReactFlag(initial, ImArray.<Observable<Object>>empty());
    }
    static public ReactFlag applyReacts(final ImArray<Observable<Object>> reacts) {
        return new ReactFlag(true, reacts);
    }
    static public ReactFlag apply() {
        return new ReactFlag(true, ImArray.<Observable<Object>>empty());
    }
    public String toString() {
        return String.format("ReactFlag(%s)", this.reacts);
    }
}