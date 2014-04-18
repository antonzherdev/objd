package core.chain;

public class RangeIterator implements Iterator<int> {
    public final int start;
    public final int end;
    public final int step;
    private int i = ERROR: Unknown <RangeIterator#C>self.<eIU>start\int\;
    public boolean hasNext() {
        return ERROR: Unknown (((<RangeIterator#C>self.<eIU>step\int\ > 0) && (<RangeIterator#C>self.<emp>i\int\ <= <RangeIterator#C>self.<eIU>end\int\)) || ((<RangeIterator#C>self.<eIU>step\int\ < 0) && (<RangeIterator#C>self.<emp>i\int\ >= <RangeIterator#C>self.<eIU>end\int\)));
    }
    public Integer next() {
        ERROR: Unknown local ret : int = <RangeIterator#C>self.<emp>i\int\;
        ERROR: Unknown (<RangeIterator#C>self.<emp>i\int\ += <RangeIterator#C>self.<eIU>step\int\);
        return ERROR: Unknown <l>ret\int\;
    }
    public RangeIterator(int start,int end,int step) {
    }
    static final ClassType<RangeIterator> type;
}