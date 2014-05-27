package objd.react;

import objd.lang.*;

public final class Slot<T> extends MReact<T> {
    private React<T> _base;
    private Observer<T> _observer;
    public void connectTo(final React<T> to) {
        ERROR: Unknown synchronized(<Slot#C<T#G>>self) {
    (<Slot#C<T#G>>self.<fmp>_base\(^React#C<§T#G§>)?\ = some(<l>to\React#C<§T#G§>\)\(^React#C<§T#G§>)?\)
    if((<Slot#C<T#G>>self.<fmp>_observer\(^Observer#C<§T#G§>)?\ != none<^Observer#C<§T#G§>>)) <Slot#C<T#G>>self.<fmp>_observer\(^Observer#C<§T#G§>)¿\.<dI>detach\void\
    (<Slot#C<T#G>>self.<fmp>_observer\(^Observer#C<§T#G§>)?\ = some(<l>to\React#C<§T#G§>\.<rdI>observe(f\[§T#G§] -> void\ = newValue : §T#G§ -> void = weak <Slot#C<T#G>>self.<rdq>_set(value\§T#G§\ = <l>newValue\§T#G§\)\void\)\Observer#C<§T#G§>\)\(^Observer#C<§T#G§>)?\)
    <Slot#C<T#G>>self.<rdq>_set(value\§T#G§\ = <l>to\React#C<§T#G§>\.<dIa>value\§T#G§\)\void\
};
    }
    public void setValue(final T value) {
        connectTo(new Val<T>(value));
    }
    public Slot(final T initial) {
        super(initial);
    }
    public String toString() {
        return "Slot";
    }
}