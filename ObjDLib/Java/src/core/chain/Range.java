package core.chain;

public final class Range extends ImSeq_impl<int> {
    public final int start;
    public final int end;
    public final int step;
    public final int count;
    @Override
    public int count() {
        return count;
    }
    @Override
    public Integer applyIndex(final int index) {
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
    public Range setStep(final int step) {
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
    public static Range applyI(final int i) {
        return new Range(i, i, 1);
    }
    public Range(final int start, final int end, final int step) {
        this.start = start;
        this.end = end;
        this.step = step;
        this.count = (step > 0) ? ((start <= end) ? (((int)(end - start) / step + 1)) : (((int)0))) : ((step < 0) ? ((start >= end) ? (((int)(end - start) / step + 1)) : (((int)0))) : (((int)1)));
    }
}