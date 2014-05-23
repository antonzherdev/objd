package objd.lang;

import java.util.Random;

public class Seed extends Random {
    public Seed() {
        super();
    }

    public Seed(long seed) {
        super(seed);
    }

    public int nextIntMinMax(int min, int max) {
        return (nextInt()%(max - min)) + min;
    }
}
