package core.chain;

public final class PArray<T> extends ImSeq_impl<T> {
    public final int stride;
    public final F2<Pointer, Integer, T> wrap;
    public final int count;
    public final int length;
    public final Pointer bytes;
    public final boolean copied;
    public static PArray<T> applyStrideWrapCountCopyBytes(int stride,F2<Pointer, Integer, T> wrap,int count,Pointer copyBytes) {
        int len = count * stride;
        return new PArray<T>(stride, wrap, count, len, copyBytes.copy(count * stride), true);
    }
    @Override
    public Iterator<T> iterator() {
        return new PArrayIterator<T>(this);
    }
    @Override
    public void dealloc() {
        if(this.copied) {
            this.bytes.free();
        }
    }
    @Override
    public T applyIndex(int index) {
        if(index >= this.count) {
            return null;
        } else {
            return this.wrap.apply(this.bytes, index);
        }
    }
    public void forRefEach(P<Pointer> each) {
        Pointer __b = this.bytes;
        int __i = 0;
        while(__i < this.count) {
            each.apply(__b);
            ERROR: Unknown <lm>__i\int\++;
            ERROR: Unknown <lm>__b\§T#G§*\++;
        }
    }
    public PArray(int stride,F2<Pointer, Integer, T> wrap,int count,int length,Pointer bytes,boolean copied) {
        this.stride = stride;
        this.wrap = wrap;
        this.count = count;
        this.length = length;
        this.bytes = bytes;
        this.copied = copied;
    }
}