package objd.collection;

import objd.lang.*;

public class RangeIterator extends Iterator_impl<Integer> {
    public final int start;
    public final int end;
    public final int step;
    private int i;
    @Override
    public boolean hasNext() {
        return (this.step > 0 && this.i <= this.end) || (this.step < 0 && this.i >= this.end);
    }
    @Override
    public Integer next() {
        final int ret = this.i;
        this.i += this.step;
        return ret;
    }
    public RangeIterator(final int start, final int end, final int step) {
        this.start = start;
        this.end = end;
        this.step = step;
        this.i = start;
    }
    public String toString() {
        return String.format("RangeIterator(%d, %d, %d)", this.start, this.end, this.step);
    }
}