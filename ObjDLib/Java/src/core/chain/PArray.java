package core.chain;

public final class PArray<T> extends ImSeq_impl<T> {
    public final int stride;
    public final F2<Pointer, Integer, T> wrap;
    public final int count;
    @Override
    public int count() {
        return count;
    }
    public final int length;
    public final Pointer bytes;
    public final boolean copied;
    public static <T> PArray<T> applyStrideWrapCountCopyBytes(final int stride, final F2<Pointer, Integer, T> wrap, final int count, final Pointer copyBytes) {
        final int len = count * stride;
        return new PArray<T>(stride, wrap, count, len, copyBytes.copyBytes(count * stride), true);
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
    public T applyIndex(final int index) {
        if(index >= this.count) {
            return null;
        } else {
            return this.wrap.apply(this.bytes, index);
        }
    }
    public void forRefEach(final P<Pointer> each) {
        Pointer __b = this.bytes;
        int __i = 0;
        while(__i < this.count) {
            each.apply(__b);
            __i++;
            __b++;
        }
    }
    public PArray(final int stride, final F2<Pointer, Integer, T> wrap, final int count, final int length, final Pointer bytes, final boolean copied) {
        this.stride = stride;
        this.wrap = wrap;
        this.count = count;
        this.length = length;
        this.bytes = bytes;
        this.copied = copied;
    }
}