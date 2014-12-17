package objd.collection;

import objd.lang.*;
import java.nio.IntBuffer;

public class Int4Buffer extends Buffer<Integer> {
    public final IntBuffer bytes;
    @Override
    public IntBuffer bytes() {
        return bytes;
    }
    public int get() {
        return this.bytes.get();
    }
    public void setV(final int v) {
        this.bytes.put(v);
    }
    public void forF(final P<Integer> f) {
        int i = 0;
        this.bytes.clear();
        while(i < this.count) {
            f.apply(this.bytes.get());
            i++;
        }
    }
    public Int4Buffer(final int count) {
        super(count, ((int)(4)));
        this.bytes = IntBuffer.allocate(((int)(count)));
    }
    public String toString() {
        return "Int4Buffer";
    }
}