package core.chain;

public class IterableF<T> extends ImIterable_impl<T> {
    public final F<Void, Iterator<T>> iteratorF;
    @Override
    public Iterator<T> iterator() {
        return ERROR: Unknown <IterableF#C<T#G>>self.<eIU>iteratorF\void -> Iterator#T<§T#G§>\();
    }
    public IterableF(F<Void, Iterator<T>> iteratorF) {
        this.iteratorF = iteratorF;
    }
}