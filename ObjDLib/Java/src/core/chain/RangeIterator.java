package core.chain;

public class RangeIterator implements Iterator<int> {
    public final int start;
    public final int end;
    public final int step;
    private int i = start;
    @Override
    public boolean hasNext() {
        return (this.step > ERROR: Unknown 0 && this.i <= this.end) || (this.step < ERROR: Unknown 0 && this.i >= this.end);
    }
    @Override
    public Integer next() {
        int ret = this.i;
        this.i += this.step;
        return ret;
    }
    public RangeIterator(int start,int end,int step) {
        this.start = start;
        this.end = end;
        this.step = step;
    }
}