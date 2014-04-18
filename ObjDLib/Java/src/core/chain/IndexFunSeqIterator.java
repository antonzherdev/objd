package core.chain;

public class IndexFunSeqIterator<T> implements Iterator<T> {
    public int count;
    public F<Integer, T> f;
    private int i;
    public boolean hasNext() {
    }
    public T next() {
    }
    public IndexFunSeqIterator(int count,F<Integer, T> f) {
    }
    static ClassType<IndexFunSeqIterator<T>> type;
}