package test;

import objd.lang.P0;

public class TestCase {
    protected void repeatTimesF(int times, P0 p0) {
        for (int i = 0; i < times; i++) {
            System.out.printf("= Repeat %d\n", i + 1);
            p0.apply();
        }
    }
}
