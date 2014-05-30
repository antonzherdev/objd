package objd.lang;

import objd.collection.Iterator;
import objd.collection.Seq_impl;

public class StringAdapter extends Seq_impl<Character> {
    final String string;

    public StringAdapter(String string) {
        this.string = string;
    }

    @Override
    public Iterator<Character> iterator() {
        return new Iterator<Character>() {
            int i = 0;
            @Override
            public boolean hasNext() {
                return i < string.length();
            }

            @Override
            public Character next() {
                char c = string.charAt(i);
                i++;
                return c;
            }
        };
    }
}
