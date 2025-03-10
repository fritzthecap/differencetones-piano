package fri.music;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

public final class MathUtils
{
    /** 2, 3, 4 -> 12. */
    public static int leastCommonMultipleInt(Stream<Integer> numbers) {
        return numbers.reduce(1, (x, y) -> x * y / (int) gcd(x, y));
    }
    public static long leastCommonMultipleLong(Stream<Long> numbers) {
        return numbers.reduce(1L, (x, y) -> x * y / gcd(x, y));
    }
    public static int leastCommonMultiple(int... numbers) {
        return leastCommonMultiple(IntStream.of(numbers));
    }
    public static long leastCommonMultiple(long... numbers) {
        return leastCommonMultiple(LongStream.of(numbers));
    }
    public static int leastCommonMultiple(IntStream numbers) {
        return leastCommonMultipleInt(numbers.boxed());
    }
    public static long leastCommonMultiple(LongStream numbers) {
        return leastCommonMultipleLong(numbers.boxed());
    }
    
    /** 6, 9, 15 -> 3. */
    public static int greatestCommonDivisorInt(Stream<Integer> numbers) {
        return numbers.reduce(0, (x, y) -> (int) gcd((long) x, (long) y));
    }
    public static long greatestCommonDivisorLong(Stream<Long> numbers) {
        return numbers.reduce(0L, (x, y) -> gcd(x, y));
    }
    public static int greatestCommonDivisor(int... numbers) {
        return greatestCommonDivisor(IntStream.of(numbers));
    }
    public static long greatestCommonDivisor(long... numbers) {
        return greatestCommonDivisor(LongStream.of(numbers));
    }
    public static int greatestCommonDivisor(IntStream numbers) {
        return greatestCommonDivisorInt(numbers.boxed());
    }
    public static long greatestCommonDivisor(LongStream numbers) {
        return greatestCommonDivisorLong(numbers.boxed());
    }
    
    private static long gcd(long x, long y) {
        return (y == 0L) ? x : gcd(y, x % y);
    }

    
    /** 1.5 -> 3/2 */
    public static long[] toFraction(double number) {
        final Fraction fraction = new Fraction(number);
        return new long[] { 
                fraction.getNumerator().longValue(), 
                fraction.getDenominator().longValue() 
            };
    }

    /** 10/4 -> 5/2 */
    public static long[] reduceFraction(long dividend, long divisor) {
        final Fraction fraction = new Fraction(dividend, divisor);
        return new long[] { 
                fraction.getNumerator().longValue(), 
                fraction.getDenominator().longValue() 
            };
    }
    
    
    /** Adopted and simplified from apache-commons-math BigFraction. */
    public static class Fraction
    {
        private static final double DEFAULT_EPSILON = 1e-5; // 1.0e-20
        private static final int DEFAULT_ITERATIONS = 100; // 10000

        private final double doubleValue;
        private final BigInteger dividend; // numerator
        private final BigInteger divisor; // denominator

        /**
         * @param dividend numerator, the number above the fraction line.
         * @param divisor denominator, the number below the fraction line.
         */
        public Fraction(long dividend, long divisor) {
            this((double) dividend / (double) divisor);
        }
        
        /**
         * @param value the number to turn into a (reduced) fraction.
         */
        public Fraction(double value) {
            this(value, DEFAULT_EPSILON, Integer.MAX_VALUE, DEFAULT_ITERATIONS);
        }

        private Fraction(double value, double epsilon, int maxDenominator, int maxIterations) {
            this.doubleValue = value;
            
            final long overflow = Long.MAX_VALUE;
            double r0 = value;
            long a0 = (long) Math.floor(r0);

            if (Math.abs(a0) > overflow) {
                throw new RuntimeException("MAX_VALUE overflow!");
            }

            // check for (almost) integer arguments, which should not go to iterations.
            if (Math.abs(a0 - value) < epsilon) {
                dividend = BigInteger.valueOf(a0);
                divisor  = BigInteger.ONE;
                return;
            }

            long p0 = 1;
            long q0 = 0;
            long p1 = a0;
            long q1 = 1;

            long p2 = 0;
            long q2 = 1;

            int n = 0;
            boolean stop = false;
            do {
                ++n;
                final double r1 = 1.0 / (r0 - a0);
                final long a1 = (long) Math.floor(r1);
                p2 = (a1 * p1) + p0;
                q2 = (a1 * q1) + q0;
                if ((p2 > overflow) || (q2 > overflow)) {
                    // in maxDenominator mode, if the last fraction was very close to the actual value
                    // q2 may overflow in the next iteration; in this case return the last one.
                    if (epsilon == 0.0 && Math.abs(q1) < maxDenominator) {
                        break;
                    }
                    throw new RuntimeException("maxDenominator overflow!");
                }

                final double convergent = (double) p2 / (double) q2;
                if ((n < maxIterations) &&
                    (Math.abs(convergent - value) > epsilon) &&
                    (q2 < maxDenominator)) {
                    p0 = p1;
                    p1 = p2;
                    q0 = q1;
                    q1 = q2;
                    a0 = a1;
                    r0 = r1;
                } else {
                    stop = true;
                }
            } while (!stop);

            if (n >= maxIterations) {
                throw new RuntimeException("maxIterations overflow!");
            }

            if (q2 < maxDenominator) {
                dividend = BigInteger.valueOf(p2);
                divisor  = BigInteger.valueOf(q2);
            } else {
                dividend = BigInteger.valueOf(p1);
                divisor  = BigInteger.valueOf(q1);
            }
        }

        
        /** @return the calculated-by-division or constructor-given value. */
        public double doubleValue() {
            return doubleValue;
        }

        /** @return the reduced dividend (numerator). */
        public BigInteger getNumerator() {
            return dividend;
        }
        
        /** @return the reduced divisor (denominator). */
        public BigInteger getDenominator() {
            return divisor;
        }
    }   // end class Fraction
    
    
    
    private MathUtils() {}
    
    public static void main(String[] args) {
        if (args.length < 2)
            throw new IllegalArgumentException("Expected at least 2 numbers to caclulate LCM and GCD from!");
        
        // calculate arguments
        int[] numbers = new int[args.length];
        for (int i = 0; i < args.length; i++)
            numbers[i] = Integer.valueOf(args[i]);

        int lcm = MathUtils.leastCommonMultiple(numbers);
        System.out.println(lcm+" is the least common multiple of "+Arrays.toString(numbers));
        
        int gcd = MathUtils.greatestCommonDivisor(numbers);
        System.out.println(gcd+" is the greatest common divisor of "+Arrays.toString(numbers));
    }
}