package core.chain;

public final class Range extends ImSeq_impl<int> {
    public final int start;
    public final int end;
    public final int step;
    public final int count = (step > 0) ? ((start <= end) ? (((int)(end - start) / step + 1)) : (((int)0))) : ((step < 0) ? ((start >= end) ? (((int)(end - start) / step + 1)) : (((int)0))) : (((int)1)));
    @Override
    public Integer applyIndex(int index) {
        if(index < this.count) {
            return this.start + this.step * index;
        } else {
            return null;
        }
    }
    @Override
    public Iterator<Integer> iterator() {
        return new RangeIterator(this.start, this.end, this.step);
    }
    public Range setStep(int step) {
        return new Range(this.start, this.end, step);
    }
    @Override
    public boolean isEmpty() {
        if(this.step > 0) {
            return this.start > this.end;
        } else {
            if(this.step < 0) {
                return this.start < this.end;
            } else {
                return false;
            }
        }
    }
    public static Range applyI(int i) {
        return new Range(i, i, 1);
    }
    public Range(int start,int end,int step) {
        this.start = start;
        this.end = end;
        this.step = step;
    }
}