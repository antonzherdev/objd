package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class FutureTest extends TestCase {
    public void testPromiseOnComplete() {
        AtomicInt n = new AtomicInt();
        int count = 100000;
        1.to(count).forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                Promise<Integer> p = Promise().apply<Integer>();
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        return p.successValue(i);
                    }
                });
                p.onCompleteF(new P<Try<Integer>>() {
                    @Override
                    public void apply(Try<Integer> _) {
                        n.incrementAndGet();
                    }
                });
            }
        });
        Thread().sleepPeriod(ERROR: Unknown 1.cast<float>);
        ().assertEqualsAB<Integer>(n.intValue(), ERROR: Unknown <l>count\int\.cast<int4>);
    }
    public void testMap() {
        AtomicInt n = new AtomicInt();
        int count = 100;
        AtomicInt result = new AtomicInt();
        1.to(count).parForEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                Promise<Integer> p = Promise().apply<Integer>();
                Future<Integer> m = p.mapF<Integer>(new F<Integer, Integer>() {
                    @Override
                    public Integer apply(Integer _) {
                        return _ + 1;
                    }
                });
                result.addAndGetValue(ERROR: Unknown (<l>i\§^int§\ + 1).cast<int4>);
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        return p.successValue(i);
                    }
                });
                m.onCompleteF(new P<Try<Integer>>() {
                    @Override
                    public void apply(Try<Integer> _) {
                        n.addAndGetValue(ERROR: Unknown <l>_\Try#C<§^int§>\.<dIa>get\§^int§\.cast<int4>);
                    }
                });
            }
        });
        Thread().sleepPeriod(ERROR: Unknown 3.cast<float>);
        ().assertEqualsAB<AtomicInt>(n, result);
    }
    public void testFlatMap() {
        AtomicInt n = new AtomicInt();
        int count = 100;
        int result = 0;
        1.to(count).forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                Promise<Integer> p = Promise().apply<Integer>();
                Future<Integer> m = p.flatMapF<Integer>(new F<Integer, Future<Integer>>() {
                    @Override
                    public Future<Integer> apply(Integer _) {
                        return Future().applyF<Integer>(new F0<Integer>() {
                            @Override
                            public Integer apply() {
                                return _ + 1;
                            }
                        });
                    }
                });
                result += i + 1;
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        return p.successValue(i);
                    }
                });
                m.onCompleteF(new P<Try<Integer>>() {
                    @Override
                    public void apply(Try<Integer> _) {
                        n.addAndGetValue(ERROR: Unknown <l>_\Try#C<§^int§>\.<dIa>get\§^int§\.cast<int4>);
                    }
                });
            }
        });
        Thread().sleepPeriod(ERROR: Unknown 3.cast<float>);
        ().assertEqualsAB<Integer>(n.intValue(), ERROR: Unknown <lm>result\int\.cast<int4>);
    }
    public FutureTest() {
    }
}