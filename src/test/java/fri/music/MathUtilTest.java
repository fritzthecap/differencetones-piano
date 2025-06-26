package fri.music;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class MathUtilTest
{
    @Test
    void leastCommonMultiple() {
        final int lcm = MathUtils.leastCommonMultiple(new int[] { 2, 3, 4 });
        assertEquals(12, lcm);
    }

    @Test
    void greatestCommonDivisor() {
        final int gcd = MathUtils.greatestCommonDivisor(new int[] { 6, 9, 15 });
        assertEquals(3, gcd);
    }

    @Test
    void toFraction() {
        long[] fraction;
        
        fraction = MathUtils.toFraction(0.5);
        assertEquals(1, fraction[0]);
        assertEquals(2, fraction[1]);
        
        fraction = MathUtils.toFraction(0.6);
        assertEquals(3, fraction[0]);
        assertEquals(5, fraction[1]);
        
        fraction = MathUtils.toFraction(0.85);
        assertEquals(17, fraction[0]);
        assertEquals(20, fraction[1]);
        
        fraction = MathUtils.toFraction(1.25);
        assertEquals(5, fraction[0]);
        assertEquals(4, fraction[1]);
    }
    
    @Test
    void hugeDecimalToFraction() {
        final double number = 1234567890123.1234567890123;
        final long[] fraction = MathUtils.toFraction(number);
        final double divisionResult = (double) fraction[0] / (double) fraction[1];
        assertEquals(number, divisionResult);
    }
        
    @Test
    void periodicNumberToFraction() {
        long[] fraction;
        
        fraction = MathUtils.toFraction(1.333333333333333);
        //System.out.println(fraction[0]+"/"+fraction[1]);
        assertEquals(4, fraction[0]);
        assertEquals(3, fraction[1]);
        
        fraction = MathUtils.toFraction(1.666666666666666);
        assertEquals(5, fraction[0]);
        assertEquals(3, fraction[1]);
        
        fraction = MathUtils.toFraction(1.777777777777777);
        assertEquals(16, fraction[0]);
        assertEquals(9, fraction[1]);
        
        fraction = MathUtils.toFraction(0.777777777777777);
        assertEquals(7, fraction[0]);
        assertEquals(9, fraction[1]);
        
        fraction = MathUtils.toFraction(1.599999999999999); // ~ 1.6
        assertEquals(8, fraction[0]);
        assertEquals(5, fraction[1]);
    }

    @Test
    void reduceFraction() {
        final long[] reducedFraction = MathUtils.reduceFraction(20, 12);
        assertEquals(5, reducedFraction[0]);
        assertEquals(3, reducedFraction[1]);
    }

    @Test
    void reducePeriodicFraction() {
        long[] fractionInt;
        
        fractionInt = MathUtils.reduceFraction(13333333333333333L, 10000000000000000L);
        assertEquals(4, fractionInt[0]);
        assertEquals(3, fractionInt[1]);
        
        fractionInt = MathUtils.reduceFraction(16666666666666667L, 10000000000000000L);
        assertEquals(5, fractionInt[0]);
        assertEquals(3, fractionInt[1]);
        
        fractionInt = MathUtils.reduceFraction(17777777777777777L, 10000000000000000L);
        assertEquals(16, fractionInt[0]);
        assertEquals(9, fractionInt[1]);
        
        fractionInt = MathUtils.reduceFraction(15999999999999999L, 10000000000000000L);
        assertEquals(8, fractionInt[0]);
        assertEquals(5, fractionInt[1]);
    }
    
    @Test
    void mathUtilsFraction() {
        MathUtils.Fraction fraction;
        
        fraction = new MathUtils.Fraction(1.3333333333333333);
        assertEquals(4, fraction.dividend.intValue());
        assertEquals(3, fraction.divisor.intValue());

        fraction = new MathUtils.Fraction(1.6666666666666667);
        assertEquals(5, fraction.dividend.intValue());
        assertEquals(3, fraction.divisor.intValue());

        fraction = new MathUtils.Fraction(1.7777777777777777);
        assertEquals(16, fraction.dividend.intValue());
        assertEquals(9, fraction.divisor.intValue());

        fraction = new MathUtils.Fraction(1.5999999999999999);
        assertEquals(8, fraction.dividend.intValue());
        assertEquals(5, fraction.divisor.intValue());
    }
    
    @Test
    void goldenRatio() {
        final double goldenRatio = (Math.sqrt(5.0) + 1.0) / 2.0;
        final long[] fraction1 = MathUtils.toFraction(goldenRatio);
        System.out.println("Golden Ratio: "+fraction1[0]+"/"+fraction1[1]+" = "+goldenRatio);
        
        assertEquals(1.6180339887498948, goldenRatio);
        assertEquals(377, fraction1[0]);
        assertEquals(233, fraction1[1]);
        
        final double goldenRatioInversion = (1.0 / goldenRatio);
        final long[] fraction2 = MathUtils.toFraction(goldenRatioInversion);
        System.out.println("Its inverse:  "+fraction2[0]+"/"+fraction2[1]+" = "+goldenRatioInversion);
        
        assertEquals(0.6180339887498948, goldenRatioInversion);
        assertEquals(144, fraction2[0]);
        assertEquals(233, fraction2[1]);
    }
}