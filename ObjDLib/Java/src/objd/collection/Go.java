package objd.collection;

import objd.lang.*;

public class Go extends Enum<Go> {
    public Go(final int ordinal, final String name) {
        super(ordinal, name);
    }
    static final Go Continue;
    static final Go Break;
    static ImArray<Go> values() {
    }
    static {
        Continue = new Go(((int)(0)), "Continue");
        Break = new Go(((int)(1)), "Break");
    }
}