package objd.collection;

import objd.lang.F;
import objd.lang.StringAdapter;

public class StringBuilder implements Builder<Character, StringAdapter> {
    private final java.lang.StringBuilder builder = new java.lang.StringBuilder();
    public void appendStr(String str) {
        builder.append(str);
    }

    public void appendObj(Object item) {
        builder.append(item);
    }

    @Override
    public void appendItem(Character item) {
        builder.append(item);
    }

    public StringAdapter build() {
        return new StringAdapter(builder.toString());
    }

    @Override
    public String toString() {
        return builder.toString();
    }

    @Override
    public void appendAllItems(Traversable<Character> items) {
        items.goOn(new F<Character, Go>() {
            @Override
            public Go apply(Character character) {
                builder.append(character);
                return Go.Continue;
            }
        });
    }

    public void appendCh(char item) {
        builder.append(item);
    }
}
