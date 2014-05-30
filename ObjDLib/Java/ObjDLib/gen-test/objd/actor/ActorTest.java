package objd.actor;

import objd.lang.*;
import objd.collection.ImArray;
import objd.collection.Iterator;
import objd.concurrent.Future;
import objd.test.PackageObjectTest;
import objd.collection.Set;
import org.junit.Test;
import objd.concurrent.Promise;
import objd.test.TestCase;

public class ActorTest extends TestCase {
    @Test
    public void testTypedActor() {
        final TestedActor a = new TestedActor();
        ImArray<String> items = ImArray.<String>empty();
        int en = 0;
        Log.infoText("!!ADD");
        final int count = 10000;
        {
            final Iterator<Integer> __il__5i = Int.to(1, count).iterator();
            while(__il__5i.hasNext()) {
                final Integer i = __il__5i.next();
                items = items.addItem(String.format("%s", i));
            }
        }
        Int.to(1, count).chain().<Future<Void>>mapF(new F<Integer, Future<Void>>() {
            @Override
            public Future<Void> apply(final Integer i) {
                return Future.<Void>applyF(new F0<Void>() {
                    @Override
                    public Void apply() {
                        a.addNumber(String.format("%s", i));
                        return null;
                    }
                });
            }
        }).voidFuture().getResultAwait(((double)(5)));
        Log.infoText("!!END_ADD");
        final ImArray<String> result = a.getItems().getResultAwait(((double)(5)));
        final ImArray<String> result2 = a.getItemsF().getResultAwait(((double)(5)));
        Log.infoText("!!GOT");
        PackageObjectTest.<Set<String>>assertEqualsAB(items.chain().toSet(), result.chain().toSet());
        PackageObjectTest.<Set<String>>assertEqualsAB(items.chain().toSet(), result2.chain().toSet());
        PackageObjectTest.assertTrueValue(en != count);
    }
    @Test
    public void testTypedActor2() {
        repeatTimesF(((int)(100)), new P0() {
            @Override
            public void apply() {
                final TestedActor a = new TestedActor();
                ImArray<String> items = ImArray.<String>empty();
                int en = 0;
                Log.infoText("!!ADD");
                final int count = 1000;
                {
                    final Iterator<Integer> __il__0p1_5i = Int.to(1, count).iterator();
                    while(__il__0p1_5i.hasNext()) {
                        final Integer i = __il__0p1_5i.next();
                        items = items.addItem(String.format("%s", i));
                    }
                }
                Int.to(1, count).chain().<Future<Void>>mapF(new F<Integer, Future<Void>>() {
                    @Override
                    public Future<Void> apply(final Integer i) {
                        return a.addNumber(String.format("%s", i));
                    }
                }).voidFuture().getResultAwait(((double)(5)));
                Log.infoText("!!END_ADD");
                final ImArray<String> result = a.getItems().getResultAwait(((double)(5)));
                final ImArray<String> result2 = a.getItemsF().getResultAwait(((double)(5)));
                Log.infoText("!!GOT");
                PackageObjectTest.<Set<String>>assertEqualsAB(items.chain().toSet(), result.chain().toSet());
                PackageObjectTest.<Set<String>>assertEqualsAB(items.chain().toSet(), result2.chain().toSet());
                PackageObjectTest.assertTrueValue(en != count);
            }
        });
    }
    @Test
    public void testLock() {
        repeatTimesF(((int)(1000)), new P0() {
            @Override
            public void apply() {
                final TestedActor a = new TestedActor();
                final int count = 100;
                final ImArray<Tuple<Integer, Promise<String>>> arr = Int.to(1, count).chain().<Tuple<Integer, Promise<String>>>mapF(new F<Integer, Tuple<Integer, Promise<String>>>() {
                    @Override
                    public Tuple<Integer, Promise<String>> apply(final Integer _) {
                        return new Tuple<Integer, Promise<String>>(_, Promise.<String>apply());
                    }
                }).toArray();
                {
                    final Iterator<Tuple<Integer, Promise<String>>> __il__0p1_3i = arr.iterator();
                    while(__il__0p1_3i.hasNext()) {
                        final Tuple<Integer, Promise<String>> t = __il__0p1_3i.next();
                        a.lockFuture(t.b);
                    }
                }
                final Future<ImArray<String>> f = a.getItems();
                arr.chain().shuffle().forEach(new P<Tuple<Integer, Promise<String>>>() {
                    @Override
                    public void apply(final Tuple<Integer, Promise<String>> t) {
                        t.b.successValue(String.format("%s", t.a));
                    }
                });
                final ImArray<String> exp = arr.chain().<String>mapF(new F<Tuple<Integer, Promise<String>>, String>() {
                    @Override
                    public String apply(final Tuple<Integer, Promise<String>> _) {
                        return String.format("w%s", _.a);
                    }
                }).toArray();
                final ImArray<String> items = f.getResultAwait(((double)(5)));
                PackageObjectTest.<ImArray<String>>assertEqualsAB(items, exp);
            }
        });
    }
    public ActorTest() {
    }
    public String toString() {
        return "ActorTest";
    }
}