package objd.actor;

import objd.lang.*;
import objd.collection.ImArray;
import objd.concurrent.Future;

public class TestedActor extends Actor {
    public ImArray<String> items;
    public Future<Void> addNumber(final String number) {
        return this.<Void>futureF(new F0<Void>() {
            @Override
            public Void apply() {
                TestedActor.this.items = TestedActor.this.items.addItem(number);
                return ;
            }
        });
    }
    public Future<ImArray<String>> getItems() {
        return this.<ImArray<String>>promptF(new F0<ImArray<String>>() {
            @Override
            public ImArray<String> apply() {
                return TestedActor.this.items;
            }
        });
    }
    public Future<ImArray<String>> getItemsF() {
        return this.<ImArray<String>>futureF(new F0<ImArray<String>>() {
            @Override
            public ImArray<String> apply() {
                return TestedActor.this.items;
            }
        });
    }
    public Future<String> lockFuture(final Future<String> future) {
        return this.<String, String>lockAndOnSuccessFutureF(future, new F<String, String>() {
            @Override
            public String apply(final String s) {
                TestedActor.this.items = TestedActor.this.items.addItem(String.format("w%s", s));
                return s;
            }
        });
    }
    public TestedActor() {
        this.items = ImArray.<String>empty();
    }
    public String toString() {
        return "TestedActor";
    }
}