package objd.collection;

import java.util.ArrayList;
import java.util.Collections;

@SuppressWarnings("unchecked")
public class ImArray<T> extends Array<T> implements ImSeq<T> {
    public ImArray() {
        super();
    }

    public ImArray(ArrayList<T> list) {
        super(list);
    }

    @Override
    public ImArray<T> addItem(T item) {
        ArrayList<T> l = new ArrayList<T>(list);
        l.add(item);
        return new ImArray<T>(l);
    }

    @Override
    public ImArray<T> addSeq(Seq<T> seq) {
        ArrayList<T> l = new ArrayList<T>(list);
        Iterator<T> i = seq.iterator();
        while(i.hasNext()) l.add(i.next());
        return new ImArray<T>(l);
    }

    @Override
    public ImArray<T> subItem(T item) {
        ArrayList<T> l = new ArrayList<T>(list);
        l.remove(item);
        return new ImArray<T>(l);
    }

    @Override
    public MArray<T> mCopy() {
        return new MArray<T>(new ArrayList<T>(list));
    }

    private static ImArray empty = new ImArray<Object>();
    public static <T> ImArray<T> empty() {
        return (ImArray<T>)empty;
    }
    public static <T> ImArray<T> fromObjects(T ... items) {
        ArrayList<T> ts = new ArrayList<T>(items.length);
        Collections.addAll(ts, items);
        return new ImArray<T>(ts);
    }
}
