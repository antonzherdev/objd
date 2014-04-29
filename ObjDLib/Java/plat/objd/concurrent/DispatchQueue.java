package objd.concurrent;


import objd.lang.P0;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DispatchQueue {
    public static DispatchQueue aDefault = new DispatchQueue(Runtime.getRuntime().availableProcessors());
    private final ExecutorService e;

    public DispatchQueue(int nThreads) {
        e = Executors.newFixedThreadPool(nThreads);
    }

    public void asyncF(final P0 p0) {
        e.execute(new Runnable() {
            @Override
            public void run() {
                p0.apply();
            }
        });
    }
}
