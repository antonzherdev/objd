package core.chain;

public class FutureTest extends TestCase {
    public void testPromiseOnComplete() {
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown local count : int = 100000;
        ERROR: Unknown 1.to(count).forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                ERROR: Unknown local p : Promise#C<§^int§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\;
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
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown local count : int = 100;
        ERROR: Unknown local var result : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown 1.to(count).parForEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                ERROR: Unknown local p : Promise#C<§^int§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\;
                ERROR: Unknown local m : Future#C<§^int§> = <l>p\Promise#C<§^int§>\.<rdI>map(f = _ : §^int§ -> ^int = return (<l>_\§^int§\ + 1))\Future#C<§^int§>\;
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
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown local count : int = 100;
        ERROR: Unknown local var result : int = 0;
        ERROR: Unknown 1.to(count).forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                ERROR: Unknown local p : Promise#C<§^int§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\;
                ERROR: Unknown local m : Future#C<§^int§> = <l>p\Promise#C<§^int§>\.<rdI>flatMap(f = _ : §^int§ -> ^Future#C<§^int§> = return <to>Future\Future#C.class\.<dIt>apply(f =  -> §^int§ = return (<l>_\§^int§\ + 1))\Future#C<§^int§>\)\Future#C<§^int§>\;
                result += i + ERROR: Unknown 1;
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