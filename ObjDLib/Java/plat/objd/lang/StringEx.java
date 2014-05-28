package objd.lang;

import objd.chain.Chain;
import objd.collection.ImSeq;
import objd.collection.ImSeq_impl;
import objd.collection.Iterator;
import objd.collection.Seq;

public class StringEx extends ImSeq_impl<Character> {
    private final String s;

    public StringEx(String s) {

        this.s = s;
    }

    public Chain<Character> chain() {
        return Chain.applyCollection(this);
    }

    @Override
    public Iterator<Character> iterator() {
        final int n = s.length();

        return new Iterator<Character>() {
            int i = 0;

            @Override
            public boolean hasNext() {
                return i < n;
            }

            @Override
            public Character next() {
                return s.charAt(i++);
            }
        };
    }

    @Override
    public ImSeq<Character> addItem(Character item) {
        return new StringEx(s + item);
    }

    @Override
    public ImSeq<Character> addSeq(Seq<Character> seq) {
        if(seq instanceof StringEx) return new StringEx(s + ((StringEx) seq).s);
        StringBuilder b = new StringBuilder(s);
        Iterator<Character> i = seq.iterator();
        while (i.hasNext()) b.append(i.next());
        return new StringEx(b.toString());
    }

    @Override
    public ImSeq<Character> subItem(Character item) {
        int i = s.indexOf(item);
        if(i < 0) return this;
        return new StringEx(s.substring(0, i) + s.substring(i + 1));
    }

    @Override
    public boolean isEmpty() {
        return s.isEmpty();
    }

    @Override
    public Character head() {
        return s.isEmpty() ? null : s.charAt(0);
    }

    @Override
    public Character applyIndex(int index) {
        return 0 <= index && index < s.length() ? s.charAt(index) : null;
    }

    @Override
    public Character last() {
        return s.isEmpty() ? null : s.charAt(s.length() - 1);
    }

    @Override
    public ImSeq<Character> tail() {
        return s.isEmpty() ? this : new StringEx(s.substring(1));
    }

    @Override
    public boolean containsItem(Character item) {
        return s.contains(item.toString());
    }

    @Override
    public String toString() {
        return s;
    }

    @Override
    public int hashCode() {
        return s.hashCode();
    }

    @Override
    public int count() {
        return s.length();
    }

    @Override
    public boolean equals(Object to) {
        if(to == this) return true;
        if(to == null) return false;
        if(to instanceof StringEx) return s.equals(((StringEx) to).s);
        if(to instanceof String) return s.equals(to);
        return false;
    }
}
