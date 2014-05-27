package objd.react;

import objd.lang.*;
import objd.collection.ImArray;

public final class ReactFlag extends MReact<Boolean> {
    public final ImArray<Observable<?>> reacts;
    private final ImArray<Observer<?>> observers;
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
    public ReactFlag(final boolean initial, final ImArray<Observable<?>> reacts) {
        super(initial);
        this.reacts = reacts;
        this.observers = ((ImArray)(reacts.chain().<Observer<?>>mapF(((F)(new F<Observable<?>, Observer<?>>() {
            @Override
            public Observer<?> apply(final Observable<?> r) {
                return r.observeF(((P)(new P<?>() {
                    @Override
                    public void apply(final ? _) {
                        _setValue(true);
                    }
                })));
            }
        }))).toArray()));
    }
    static public ReactFlag applyInitial(final boolean initial) {
        return new ReactFlag(initial, ImArray.<Observable<?>>empty());
    }
    static public ReactFlag applyReacts(final ImArray<Observable<?>> reacts) {
        return new ReactFlag(true, reacts);
    }
    static public ReactFlag apply() {
        return new ReactFlag(true, ImArray.<Observable<?>>empty());
    }
    public String toString() {
        return String.format("ReactFlag(%s)", this.reacts);
    }
}